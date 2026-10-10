{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | @salmon-report@'s inbound reachability test: does a TCP connection made
from a /second vantage point/ (a machine outside the network) to a declared
external address and port arrive? The first vantage point cannot tell: from
inside, a connection to one's own external address tests the router's
hairpin, not the way in.

What is declared, and nothing else is contacted:

* @--inbound HOST:PORT[\@LISTEN]@: the external address and port to connect
  to. With @\@LISTEN@, @salmon-report@ itself listens on that local port for
  the length of the report and hands a one-time token to whoever connects; a
  vantage that reads the token back has proven the connection arrived /at
  this host/. Without it, something must already listen, and "connected"
  only says that something accepted at that address.
* @--vantage-ssh [USER\@]HOST@: a machine to ask, reached with the
  operator's own @ssh@. Keys, ports, jump hosts and host keys are @ssh@'s
  business (its configuration, or the file given with
  @--vantage-ssh-config@); nothing here reads key material, and
  @BatchMode=yes@ means a login that would prompt fails instead.

The vantage needs @bash@ (for @\/dev\/tcp@) and coreutils' @timeout@. It runs
one fixed command, 'remoteCommand', in which only the host and the port vary;
both are re-rendered from a strict alphabet and parsed numbers, so nothing a
declaration says reaches the remote shell as syntax or @ssh@ as an option.

This module is the pure half (declarations, the argument vector, reading the
vantage's answer, the judgement) and the token listener; "Report" holds the
shell-out and the probes.
-}
module Report.Inbound (
    -- * Declarations
    InboundSpec (..),
    noInbound,
    InboundTarget (..),
    parseInboundTarget,
    renderTarget,
    Vantage (..),
    parseVantage,

    -- * Asking the vantage
    connectSeconds,
    remoteCommand,
    sshArgs,
    VantageAnswer (..),
    parseVantageOutput,

    -- * Judging
    inboundQuestion,
    inboundMethod,
    judgeInbound,

    -- * The token listener
    Listener (..),
    withListener,
    withListeners,
) where

import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, bracket, bracketOnError, finally, try)
import Control.Monad (forever, void)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Char (isAlphaNum, isDigit)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Network.Socket (
    Family (AF_INET, AF_INET6),
    SockAddr (..),
    Socket,
    SocketOption (IPv6Only, ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultProtocol,
    listen,
    setCloseOnExecIfNeeded,
    setSocketOption,
    socket,
    socketPort,
    tupleToHostAddress,
    withFdSocket,
 )
import Network.Socket.ByteString (sendAll)
import Numeric (showHex)
import System.IO (IOMode (ReadMode), withBinaryFile)
import Text.Read (readMaybe)

import Report.Dns (Judgement (..))
import Report.Nmap (checkHost)

-------------------------------------------------------------------------------
-- Declarations

-- | What the operator declares for the inbound test. Empty is "no test".
data InboundSpec = InboundSpec
    { inbTargets :: [Text]
    -- ^ @HOST:PORT[\@LISTEN]@, see 'parseInboundTarget'
    , inbVantageSsh :: [Text]
    -- ^ @[USER\@]HOST@ destinations handed to @ssh@, see 'parseVantage'
    , inbSshConfig :: Maybe FilePath
    -- ^ an @ssh@ configuration file (@ssh -F@), the operator's own when absent
    }
    deriving (Eq, Show, Generic)

instance ToJSON InboundSpec
instance FromJSON InboundSpec

noInbound :: InboundSpec
noInbound = InboundSpec [] [] Nothing

-- | An external address and port, and the local port to listen on, if any.
data InboundTarget = InboundTarget
    { inHost :: Text
    , inPort :: Int
    , inListen :: Maybe Int
    }
    deriving (Eq, Ord, Show)

{- | Reads @HOST:PORT[\@LISTEN]@ (an IPv6 address goes in brackets). The host
is one DNS name or one address, as for @--nmap@.
-}
parseInboundTarget :: Text -> Either Text InboundTarget
parseInboundTarget raw = do
    let (target, at) = Text.breakOn "@" (Text.strip raw)
        (h, p) = Text.breakOnEnd ":" target
    lport <- if Text.null at then Right Nothing else Just <$> port "local port" (Text.drop 1 at)
    bracketed <- maybe (Left "not of the form HOST:PORT") Right (Text.stripSuffix ":" h)
    host <- checkHost (Text.dropAround (`elem` ("[]" :: String)) bracketed)
    eport <- port "port" p
    pure (InboundTarget host eport lport)
  where
    port what x
        | not (Text.null x) && Text.all isDigit x && Text.length x <= 5
        , Just n <- readMaybe (Text.unpack x)
        , n >= 1 && n <= 65535 =
            Right n
        | otherwise = Left ("not a " <> what <> " between 1 and 65535: " <> if Text.null x then "(empty)" else x)

-- | @HOST:PORT@, with an IPv6 address back in its brackets.
renderTarget :: InboundTarget -> Text
renderTarget t
    | Text.any (== ':') (inHost t) = "[" <> inHost t <> "]:" <> p
    | otherwise = inHost t <> ":" <> p
  where
    p = Text.pack (show (inPort t))

-- | An @ssh@ destination, checked: @[USER\@]HOST@.
newtype Vantage = Vantage {vantageDest :: Text}
    deriving (Eq, Ord, Show)

{- | Reads @[USER\@]HOST@. Anything @ssh@ could read as an option or the
remote shell as syntax is refused; a port, a jump host or an identity belong
in the @ssh@ configuration.
-}
parseVantage :: Text -> Either Text Vantage
parseVantage raw = case Text.splitOn "@" dest of
    [h] -> Vantage <$> checkHost h
    [u, h]
        | Text.null u || "-" `Text.isPrefixOf` u || not (Text.all userChar u) -> Left "not a user name ssh would take"
        | otherwise -> (\host -> Vantage (u <> "@" <> host)) <$> checkHost h
    _ -> Left "not of the form [USER@]HOST"
  where
    dest = Text.strip raw
    userChar c = (isAlphaNum c && c < '\x80') || c `elem` ("._-" :: String)

-------------------------------------------------------------------------------
-- Asking the vantage

-- | How long the vantage waits for its connection (and for the token), and @ssh@ for its own.
connectSeconds :: Int
connectSeconds = 4

connectedMarker, exitMarker :: Text
connectedMarker = "salmon-inbound:connected"
exitMarker = "salmon-inbound:exit:"

{- | The one command the vantage runs, as a single string for its login
shell: open a TCP connection with @bash@'s @\/dev\/tcp@ under @timeout@, say
so, read the token back when one is expected (filtered down to the token's
alphabet), and always end with the exit status on a line of its own.
-}
remoteCommand :: InboundTarget -> Text
remoteCommand t =
    "bash -c 'r=0; timeout "
        <> Text.pack (show connectSeconds)
        <> " bash -c \"exec 3<>/dev/tcp/"
        <> inHost t
        <> "/"
        <> Text.pack (show (inPort t))
        <> " && echo "
        <> connectedMarker
        <> maybe "" (const " && head -c 128 <&3 | tr -cd a-zA-Z0-9_-") (inListen t)
        <> "\" 2>&1 || r=$?; echo; echo "
        <> exitMarker
        <> "$r'"

{- | The arguments of the one @ssh@ a probe runs. No terminal, no prompt
(@BatchMode@), a bounded connection, and @--@ before the destination.
-}
sshArgs :: Maybe FilePath -> Vantage -> InboundTarget -> [String]
sshArgs cfg v t =
    maybe [] (\f -> ["-F", f]) cfg
        <> ["-o", "BatchMode=yes", "-o", "ConnectTimeout=" <> show connectSeconds, "-T", "--", Text.unpack (vantageDest v), Text.unpack (remoteCommand t)]

-- | What came back from the vantage.
data VantageAnswer
    = -- | the connection was accepted; the lines read from it, when asked to read
      Connected [Text]
    | Refused
    | -- | nothing answered within 'connectSeconds'
      TimedOut
    | -- | the vantage itself has no way to the address, in its words
      Unreachable Text
    | -- | the command ran and failed otherwise, or never ran: what was printed
      VantageFailed [Text]
    deriving (Eq, Show)

-- | Reads the output of 'remoteCommand' (and whatever @ssh@ printed around it).
parseVantageOutput :: Text -> VantageAnswer
parseVantageOutput out
    | connectedMarker `elem` ls = Connected [l | l <- drop 1 (dropWhile (/= connectedMarker) ls), not (exitMarker `Text.isPrefixOf` l)]
    | otherwise = case exitStatus of
        Nothing -> VantageFailed (take 3 ls)
        Just 124 -> TimedOut
        Just n
            | said "Connection refused" -> Refused
            | (l : _) <- [l | l <- said', any (`Text.isInfixOf` l) ["No route to host", "Network is unreachable"]] -> Unreachable l
            | otherwise -> VantageFailed (take 3 said' <> ["the vantage's command exited " <> Text.pack (show n)])
  where
    ls = filter (not . Text.null) (Text.strip <$> Text.lines out)
    said' = filter (not . Text.isPrefixOf exitMarker) ls
    said w = any (w `Text.isInfixOf`) said'
    exitStatus = case [n | l <- ls, Just x <- [Text.stripPrefix exitMarker l], Just n <- [readMaybe (Text.unpack x) :: Maybe Int]] of
        [] -> Nothing
        ns -> Just (last ns)

-------------------------------------------------------------------------------
-- Judging

inboundQuestion :: Vantage -> InboundTarget -> Text
inboundQuestion v t =
    "Does a TCP connection from " <> vantageDest v <> " to " <> renderTarget t <> " arrive" <> maybe "?" (const " at this host?") (inListen t)

inboundMethod :: Vantage -> InboundTarget -> Text
inboundMethod v t =
    "ssh "
        <> vantageDest v
        <> ", bash /dev/tcp connect to "
        <> renderTarget t
        <> maybe "" (\p -> ", one-time token served on local port " <> Text.pack (show p)) (inListen t)

{- | What the answer means. The second argument is the listener's side when
the target declared one: the token it served and the peers it accepted. The
token itself never shows in the evidence.
-}
judgeInbound :: InboundTarget -> Maybe (Text, [Text]) -> VantageAnswer -> Judgement
judgeInbound t local answer = case (answer, local) of
    (Connected _, Nothing) ->
        Judgement
            (Just True)
            ["the vantage connected to " <> target]
            (Just "Something accepted the connection at that address and port. No token was exchanged (declare HOST:PORT@LISTEN for that), so this does not show that it was this host.")
    (Connected back, Just (token, peers))
        | token `elem` back -> Judgement (Just True) (["the vantage connected to " <> target <> " and read back the token this host served"] <> arrivals peers) Nothing
        | otherwise ->
            Judgement
                (Just False)
                (["the vantage connected to " <> target <> " but did not read this host's token"] <> arrivals peers)
                (Just "Something else accepts connections at that address and port: the router itself, another host, or a mapping to another local port.")
    (Refused, _) ->
        no
            ["the connection to " <> target <> " was refused"]
            "The address answered and refused: the packet reached a machine (often the router) on which nothing forwards or listens on this port."
    (TimedOut, _) ->
        no
            ["no answer from " <> target <> " within " <> Text.pack (show connectSeconds) <> "s"]
            "Dropped on the way: no port mapping, a firewall, or an address that is not this network's (double NAT). A vantage whose own outbound traffic is filtered looks the same."
    (Unreachable why, _) -> Judgement Nothing ([why] <> arrivals') (Just "The vantage has no route to the address, which says nothing about this network.")
    (VantageFailed said, _) -> Judgement Nothing ((if null said then ["the vantage printed nothing"] else said) <> arrivals') (Just "The vantage could not be asked: check that ssh reaches it without a prompt and that it has bash and timeout.")
  where
    target = renderTarget t
    no ev meaning = Judgement (Just False) (ev <> arrivals') (Just meaning)
    arrivals' = maybe [] (arrivals . snd) local
    arrivals [] = ["no connection reached the local listener"]
    arrivals peers = ["the local listener accepted a connection from " <> Text.intercalate ", " (take 3 (nub peers))]

-------------------------------------------------------------------------------
-- The token listener

-- | A listener of this process: the token it hands out and who connected so far.
data Listener = Listener
    { listenerToken :: Text
    , listenerPort :: Int
    , listenerPeers :: IO [Text]
    }

{- | Listens on a local TCP port (every address, IPv6 and IPv4; port 0 takes
a free one) while the action runs, sending a fresh token and closing to
whoever connects. The token is random, used once and is not a credential: it
only tells this listener from whatever else may answer at the external
address. A port that cannot be bound is a reason, and the action still runs.
-}
withListener :: Int -> (Either Text Listener -> IO a) -> IO a
withListener port act = do
    opened <- try (bracketOnError open close (\s -> (,) s <$> newToken))
    case opened of
        Left (e :: SomeException) -> act (Left ("could not listen on local port " <> Text.pack (show port) <> ": " <> Text.pack (show e)))
        Right (s, token) -> (`finally` close s) $ do
            peers <- newIORef []
            actual <- fromIntegral <$> socketPort s
            let serve = forever $ do
                    (conn, peer) <- accept s
                    atomicModifyIORef' peers (\ps -> (ps <> [Text.pack (show peer)], ()))
                    void (try (sendAll conn (Char8.pack (Text.unpack token) <> "\n")) :: IO (Either SomeException ()))
                    close conn
            withAsync serve (\_ -> act (Right (Listener token actual (readIORef peers))))
  where
    open = do
        r <- try (listenOn AF_INET6 (SockAddrInet6 (fromIntegral port) 0 (0, 0, 0, 0) 0))
        case r of
            Right s -> pure s
            Left (_ :: SomeException) -> listenOn AF_INET (SockAddrInet (fromIntegral port) (tupleToHostAddress (0, 0, 0, 0)))
    listenOn :: Family -> SockAddr -> IO Socket
    listenOn family addr =
        bracketOnError (socket family Stream defaultProtocol) close $ \s -> do
            -- no child of this process (ssh, the other probes' tools) inherits the listener
            withFdSocket s setCloseOnExecIfNeeded
            setSocketOption s ReuseAddr 1
            if family == AF_INET6 then setSocketOption s IPv6Only 0 else pure ()
            bind s addr
            listen s 8
            pure s

-- | One listener per distinct port, all held while the action runs.
withListeners :: [Int] -> (Map.Map Int (Either Text Listener) -> IO a) -> IO a
withListeners ports act = go (nub ports) Map.empty
  where
    go [] acc = act acc
    go (p : ps) acc = withListener p (\l -> go ps (Map.insert p l acc))

-- | 128 random bits from the kernel, in the alphabet 'remoteCommand' lets through.
newToken :: IO Text
newToken = do
    bytes <- withBinaryFile "/dev/urandom" ReadMode (`ByteString.hGet` 16)
    pure ("salmon-token-" <> Text.pack (concatMap hex (ByteString.unpack bytes)))
  where
    hex b = (if b < 16 then "0" else "") <> showHex b ""
