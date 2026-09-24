{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | A small typed client over the five HTTP surfaces of @run serve --http
PATH@ ("Salmon.Actions.Serve.Http"): the four reads, the command in both
modes, and the event stream.

Milestone 6 of @specs\/generic-server.md@ ("terminal client against the
socket") wants the client to be __a client of the socket, not a mode of
@serve@__, so that it works against a remote host over @ssh -L@ unchanged.
This is that client, minus any terminal: @salmon-tui@ in @salmon-apps@ is
one caller, and a script or a CI step is another. It speaks in the wire
objects ('Aeson.Value') and in "Salmon.Client.Model"'s 'Event', never in
the server's Haskell types — the same generic stance the server takes
(the protocol never interprets a seed), so this client drives any salmon
binary.

Unix socket only, no TCP, no auth: the spec's security section says
permissions are the whole access story until TLS and a token exist
(milestone 8), and a client with no TCP in it cannot be pointed at a
network by mistake.

= Reads bypass the loop

'dag', 'status', 'history' and 'seedHelp' read the loop's own world through
the server and __never stand the tending machines down__: only 'command'
and 'commandAsync' put a line in the inbox, and a line is what stops
tending before it runs. A client that polls a read is therefore free; a
client that types is acting, and should say so to whoever is watching.

= The stream

'events' opens @\/events@ once and hands each event to a callback until the
callback says stop, the connection ends, or an exception escapes.
Reconnecting is the caller's, with the last 'Model.eventSeq' it saw as
@since@: the server replays what its ring still holds and sends a @gap@
first when it does not, and what to do about a gap (re-read @\/dag@) is a
decision about the caller's model, not about the connection. A keep-alive
comment line is consumed here and never reaches the callback.
-}
module Salmon.Client.Http (
    -- * A client
    Client,
    clientPath,
    newUnixClient,
    ClientError (..),

    -- * Reads
    dag,
    status,
    history,
    seedHelp,

    -- * Commands
    command,
    commandAsync,
    Enqueued (..),

    -- * The stream
    events,
    Since,
    Filter (..),
    noFilter,

    -- * Parsing the stream
    SseBlock (..),
    splitBlocks,
    parseBlock,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), Result (..), Value (..), eitherDecode, eitherDecodeStrict, encode, fromJSON, object, withObject, (.:), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (toList)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal (makeConnection)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS
import System.Posix.IO (FdOption (CloseOnExec), setFdOption)
import System.Posix.Types (Fd (..))
import Text.Read (readMaybe)

import Salmon.Actions.Serve.Events (Filter (..), noFilter)
import Salmon.Client.Model (Event (..), eventOf)

-------------------------------------------------------------------------------

-- | A connection factory for one socket path.
data Client = Client
    { clientPath :: FilePath
    , clientManager :: HTTP.Manager
    }

{- | A client for the unix socket at the path. Every connection it opens is
marked close-on-exec, for the reason "Salmon.Actions.Serve.Events"'s spec
found: a child spawned from the same process inherits any descriptor not
so marked, and a stream held open by a child looks like a client that
never hung up.
-}
newUnixClient :: FilePath -> IO Client
newUnixClient path = do
    manager <-
        HTTP.newManager
            HTTP.defaultManagerSettings
                { HTTP.managerRawConnection = pure $ \_ _ _ -> do
                    sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
                    Socket.withFdSocket sock $ \fd -> setFdOption (Fd fd) CloseOnExec True
                    Socket.connect sock (Socket.SockAddrUnix path)
                    makeConnection (SocketBS.recv sock 4096) (SocketBS.sendAll sock) (Socket.close sock)
                , -- a stream is open for as long as the loop runs
                  HTTP.managerResponseTimeout = HTTP.responseTimeoutNone
                }
    pure (Client path manager)

-- | What the server answered with when it did not answer the question.
data ClientError
    = -- | a non-2xx status, with the @error@ text the server put in the body
      Refused !Int !Text
    | -- | a 2xx answer that was not the JSON expected
      Undecodable !Text
    deriving (Show, Eq)

instance Exception ClientError

-------------------------------------------------------------------------------
-- reads

-- | @GET \/dag@: the envelope, with @mode@, @seq@ and @nodes@.
dag :: Client -> IO Value
dag c = getJSON c "/dag"

-- | @GET \/status@: the object @status --json@ prints, plus @seq@.
status :: Client -> IO Value
status c = getJSON c "/status"

-- | @GET \/history@: the object @history --json@ prints, plus @elided@.
history :: Client -> IO Value
history c = getJSON c "/history"

-- | @GET \/help\/seed@: the binary's own @config --help@ and the command reference.
seedHelp :: Client -> IO Value
seedHelp c = getJSON c "/help/seed"

getJSON :: Client -> String -> IO Value
getJSON c route = do
    req <- HTTP.parseRequest ("http://salmon" <> route)
    resp <- HTTP.httpLbs req c.clientManager
    decodeAnswer resp

-------------------------------------------------------------------------------
-- commands

-- | The synchronous @POST \/command@: the reports the line produced.
command :: Client -> Text -> IO [Value]
command c line = do
    v <- postLine c "/command" line
    case v of
        Array xs -> pure (toList xs)
        other -> throwIO (Undecodable ("a sync command answers with an array: " <> Text.pack (show other)))

-- | The @?async@ answer: the number the line was queued at, and the origin
-- its reports carry.
data Enqueued = Enqueued
    { enqueuedSeq :: !Word64
    , enqueuedOrigin :: !Text
    }
    deriving (Show, Eq)

instance FromJSON Enqueued where
    parseJSON = withObject "enqueued" $ \o -> Enqueued <$> o .: "seq" <*> o .: "origin"

-- | @POST \/command?async@: queued, and the number to read @\/events@ from.
commandAsync :: Client -> Text -> IO Enqueued
commandAsync c line = do
    v <- postLine c "/command?async" line
    case fromJSON v of
        Success e -> pure e
        Error err -> throwIO (Undecodable ("an async command answers with seq and origin: " <> Text.pack err))

postLine :: Client -> String -> Text -> IO Value
postLine c route line = do
    req0 <- HTTP.parseRequest ("http://salmon" <> route)
    let req =
            req0
                { HTTP.method = "POST"
                , HTTP.requestHeaders = [(HTTP.hContentType, "application/json")]
                , HTTP.requestBody = HTTP.RequestBodyLBS (encode (object ["line" .= line]))
                }
    resp <- HTTP.httpLbs req c.clientManager
    decodeAnswer resp

decodeAnswer :: HTTP.Response LByteString.ByteString -> IO Value
decodeAnswer resp = do
    let code = HTTP.statusCode (HTTP.responseStatus resp)
        body = HTTP.responseBody resp
    unless (code >= 200 && code < 300) $
        throwIO (Refused code (fromMaybe (Text.decodeUtf8 (LByteString.toStrict body)) (errorText body)))
    either (throwIO . Undecodable . Text.pack) pure (eitherDecode body)
  where
    -- the server's own {"error": ...} text when the body is one
    errorText body = case eitherDecode body of
        Right (Object o) | Just (String t) <- KeyMap.lookup (Key.fromText "error") o -> Just t
        _ -> Nothing

-------------------------------------------------------------------------------
-- the stream

-- | 'Nothing' for live only; @'Just' n@ for everything after @n@ the ring still holds.
type Since = Maybe Word64

{- | Open @\/events@ and hand every event to the callback until it answers
'False'. Returns normally when the callback stops it or the server ends the
stream (the loop quit); a connection error is the exception @http-client@
raises. The @gap@ event arrives like any other, with 'eventSeq' 'Nothing'.
-}
events :: Client -> Since -> Filter -> (Event -> IO Bool) -> IO ()
events c since filt onEvent = do
    req <- HTTP.parseRequest ("http://salmon/events" <> query)
    HTTP.withResponse req c.clientManager $ \resp -> do
        let code = HTTP.statusCode (HTTP.responseStatus resp)
        unless (code == 200) $ do
            body <- LByteString.fromChunks <$> HTTP.brConsume (HTTP.responseBody resp)
            throwIO (Refused code (Text.decodeUtf8 (LByteString.toStrict body)))
        buf <- newIORef ByteString.empty
        let loop = do
                chunk <- HTTP.brRead (HTTP.responseBody resp)
                if ByteString.null chunk
                    then pure ()
                    else do
                        b <- readIORef buf
                        let (blocks, rest) = splitBlocks (b <> chunk)
                        writeIORef buf rest
                        more <- deliver (concatMap parseBlock blocks)
                        if more then loop else pure ()
            deliver [] = pure True
            deliver (SseComment : bs) = deliver bs
            deliver (SseEvent _ v : bs) = do
                more <- onEvent (eventOf v)
                if more then deliver bs else pure False
        loop
  where
    query = case params of
        [] -> ""
        ps -> "?" <> Text.unpack (Text.intercalate "&" ps)
    params =
        [ "since=" <> Text.pack (show n) | Just n <- [since] ]
            ++ [ "stream=" <> Text.intercalate "," (Set.toList ss) | Just ss <- [filt.filterStreams] ]
            ++ [ "origin=" <> o | Just os <- [filt.filterOrigins], o <- Set.toList os ]

-- | One block of the stream: an event with its @id@ (absent on a @gap@), or a comment.
data SseBlock
    = SseEvent !(Maybe Word64) !Value
    | SseComment
    deriving (Show, Eq)

-- | The complete blocks (ended by a blank line) in a buffer, and what is left.
splitBlocks :: ByteString.ByteString -> ([ByteString.ByteString], ByteString.ByteString)
splitBlocks bs =
    case ByteString.breakSubstring "\n\n" bs of
        (block, rest)
            | ByteString.null rest -> ([], bs)
            | otherwise ->
                let (more, left) = splitBlocks (ByteString.drop 2 rest)
                 in (block : more, left)

{- | One block. A block whose every line is a comment is 'SseComment'; one
with a @data:@ line that is JSON is an event; anything else (an empty
block, a @data:@ line that is not JSON) is dropped, since the server never
sends one and a client has nothing to do with it.
-}
parseBlock :: ByteString.ByteString -> [SseBlock]
parseBlock block
    | null ls = []
    | all (":" `ByteString.isPrefixOf`) ls = [SseComment]
    | otherwise =
        case [eitherDecodeStrict raw | Just raw <- fmap (fieldOf "data:") ls] of
            (Right v : _) -> [SseEvent (readMaybe . Char8.unpack =<< headMay [i | Just i <- fmap (fieldOf "id:") ls]) v]
            _ -> []
  where
    ls = Char8.lines block
    fieldOf name l
        | name `ByteString.isPrefixOf` l = Just (Char8.dropWhile (== ' ') (ByteString.drop (ByteString.length name) l))
        | otherwise = Nothing
    headMay (x : _) = Just x
    headMay [] = Nothing
