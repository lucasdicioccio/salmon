{-# LANGUAGE OverloadedStrings #-}

{- | @salmon-report@'s hairpin test: does the gateway /hairpin/, that is, does
a TCP connection made from inside the LAN to the gateway's external address
and a mapped port come back in?

What is declared, and nothing else is contacted:

* @--hairpin [ADDR:]PORT[\@LISTEN]@: the external port of a mapping that
  already exists (made by a salmon declaration, by hand on the router, by
  anything). @ADDR@ is the external IPv4 address; without it the address is
  the one the gateway reports (@upnpc -l@), else the one the declared
  @--echo@ URL answers with. With @\@LISTEN@, @salmon-report@ itself listens
  on that local port for the length of the report (the listener of
  "Report.Inbound") and hands a one-time token to whoever connects: reading
  it back proves the connection came back /to this host/.

The probe makes one TCP connection and reads the gateway's mapping list. It
opens no mapping, removes none, and changes nothing on the gateway: the
mapping under test is the operator's.

What it can know is narrow, and the judgement ('judgeHairpin') says
@unknown@ for everything else:

* /yes/ needs the token read back, or (without a listener) an accepted
  connection on a port the gateway lists as mapped;
* /no/ needs the gateway to list the mapping as leading to this report's
  listener, and the connection to demonstrably not have come back;
* no external address, no mapping to test against, a timeout (which a
  firewall also produces), an address this host holds itself: @unknown@.

This module is the pure half (the declaration, the reading of the gateway's
listing, the judgement) and the one TCP connection; "Report" holds the
shell-out and the probes.
-}
module Report.Hairpin (
    -- * Declarations
    HairpinTarget (..),
    parseHairpinTarget,
    renderHairpinTarget,
    parseIPv4,

    -- * The external address
    ExternalSource (..),
    External (..),
    chooseExternal,

    -- * The gateway's listing
    GatewayView (..),
    readGatewayListing,
    MappingState (..),
    viewMapping,

    -- * Judging
    Attempt (..),
    Evidence (..),
    hairpinQuestion,
    hairpinMethod,
    hairpinBlocked,
    judgeHairpin,

    -- * The connection
    hairpinConnectSeconds,
    hairpinReadSeconds,
    tokenLines,
    connectAttempt,
    localAddressToward,
) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, when)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (nub, partition)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.C.Error (Errno (..), eCONNREFUSED, eHOSTUNREACH, eNETUNREACH, eTIMEDOUT)
import GHC.IO.Exception (IOException (ioe_errno))
import Network.Socket (
    Family (AF_INET),
    SockAddr (..),
    SocketType (Datagram, Stream),
    close,
    connect,
    defaultProtocol,
    getSocketName,
    hostAddressToTuple,
    setCloseOnExecIfNeeded,
    socket,
    tupleToHostAddress,
    withFdSocket,
 )
import Network.Socket.ByteString (recv)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import Report.Dns (Judgement (..))
import Salmon.Builtin.Nodes.PortMapping.Upnpc

-------------------------------------------------------------------------------
-- Declarations

-- | An external port to connect to, the external address when declared, and the local port to listen on, if any.
data HairpinTarget = HairpinTarget
    { hpAddress :: Maybe Text
    -- ^ a dotted IPv4 address, as 'parseIPv4' writes it
    , hpPort :: Int
    , hpListen :: Maybe Int
    }
    deriving (Eq, Ord, Show)

-- | The four octets of a dotted IPv4 address: decimal digits only, each at most 255.
ipv4Octets :: Text -> Maybe [Int]
ipv4Octets t = case traverse octet (Text.splitOn "." t) of
    Just os@[_, _, _, _] -> Just os
    _ -> Nothing
  where
    octet x
        | not (Text.null x) && Text.length x <= 3 && Text.all isDigit x
        , Just n <- readMaybe (Text.unpack x)
        , n <= 255 =
            Just n
        | otherwise = Nothing

-- | A dotted IPv4 address, rewritten from its parsed octets (@010.0.0.1@ is @10.0.0.1@); nothing else is one.
parseIPv4 :: Text -> Maybe Text
parseIPv4 t = Text.intercalate "." . fmap (Text.pack . show) <$> ipv4Octets t

{- | Reads @[ADDR:]PORT[\@LISTEN]@. The address is an IPv4 literal, never a
name: hairpinning is a property of an IPv4 NAT, and the address connected to
must be the one the evidence names.
-}
parseHairpinTarget :: Text -> Either Text HairpinTarget
parseHairpinTarget raw = case Text.splitOn "@" (Text.strip raw) of
    [target] -> build target Nothing
    [target, l] -> port "local port" l >>= build target . Just
    _ -> Left "not of the form [ADDR:]PORT[@LISTEN]"
  where
    build target l = case Text.splitOn ":" target of
        [p] -> (\n -> HairpinTarget Nothing n l) <$> port "port" p
        [a, p] -> do
            addr <- maybe (Left ("not a dotted IPv4 address: " <> orEmpty a)) Right (parseIPv4 a)
            n <- port "port" p
            pure (HairpinTarget (Just addr) n l)
        _ -> Left "the address must be a dotted IPv4 address: hairpinning is a property of an IPv4 NAT"
    port what x
        | not (Text.null x) && Text.all isDigit x && Text.length x <= 5
        , Just n <- readMaybe (Text.unpack x)
        , n >= 1 && n <= 65535 =
            Right n
        | otherwise = Left ("not a " <> what <> " between 1 and 65535: " <> orEmpty x)
    orEmpty x = if Text.null x then "(empty)" else x

-- | The declaration a target reads back from.
renderHairpinTarget :: HairpinTarget -> Text
renderHairpinTarget t =
    maybe "" (<> ":") (hpAddress t) <> showT (hpPort t) <> maybe "" (\l -> "@" <> showT l) (hpListen t)

showT :: Int -> Text
showT = Text.pack . show

-------------------------------------------------------------------------------
-- The external address

data ExternalSource = Declared | FromGateway | FromEcho
    deriving (Eq, Show)

-- | The address the connection is made to, and who said it is the external one.
data External = External
    { extAddress :: Text
    , extSource :: ExternalSource
    }
    deriving (Eq, Show)

{- | The external address of a target: the declared one, else what the
gateway reports, else what the echo URL answered. Only a dotted IPv4 address
counts; with none there is a reason instead.
-}
chooseExternal :: HairpinTarget -> Maybe Text -> Maybe Text -> Either Text External
chooseExternal t gateway echo =
    case mapMaybe pick [(hpAddress t, Declared), (gateway, FromGateway), (echo, FromEcho)] of
        (e : _) -> Right e
        [] -> Left "no external IPv4 address is known: none is declared, the gateway reported none, and no declared --echo URL answered with one"
  where
    pick (a, src) = (\x -> External x src) <$> (a >>= parseIPv4 . Text.strip)

-------------------------------------------------------------------------------
-- The gateway's listing

-- | What @upnpc -l@ printed, when a gateway answered.
data GatewayView = GatewayView
    { gvExternal :: Maybe Text
    , gvLocal :: Maybe Text
    -- ^ this host's LAN address, as the gateway's library found it
    , gvMappings :: [Mapping]
    }
    deriving (Eq, Show)

-- | Reads the output of @upnpc -l@: the gateway's addresses and its mappings, or why there are none.
readGatewayListing :: Text -> Either Text GatewayView
readGatewayListing out = case parseGateway out of
    NoDevice -> Left "no UPnP gateway answered"
    NotIgd -> Left "a UPnP device answered but it is not an Internet Gateway Device"
    Igd ext loc -> Right (GatewayView ext loc (parseMappings out))

-- | What the gateway says about the external port under test.
data MappingState
    = -- | the listing could not be read: why
      Unlisted Text
    | -- | the listing holds no TCP mapping of the port
      NotListed
    | -- | a TCP mapping of the port to this host (to the listener's port, when one is declared)
      ListedHere Mapping
    | -- | a TCP mapping of the port to something else
      ListedElsewhere Mapping
    deriving (Eq, Show)

{- | Looks the target's port up in the gateway's mappings. The second
argument is this host's addresses. A mapping is "here" when it leads to one
of them and, if the target declares a listener, to the listener's port.
-}
viewMapping :: HairpinTarget -> [Text] -> Either Text [Mapping] -> MappingState
viewMapping _ _ (Left why) = Unlisted why
viewMapping t locals (Right ms) =
    case partition here [m | m <- ms, mapProto m == TCP, mapExternalPort m == hpPort t] of
        (m : _, _) -> ListedHere m
        ([], m : _) -> ListedElsewhere m
        ([], []) -> NotListed
  where
    mine = mapMaybe parseIPv4 locals
    here m =
        maybe False (`elem` mine) (parseIPv4 (mapInternalAddress m))
            && maybe True (== mapInternalPort m) (hpListen t)

-------------------------------------------------------------------------------
-- Judging

-- | What became of the one TCP connection.
data Attempt
    = -- | the connection was accepted; the lines read from it, when asked to read
      Accepted [Text]
    | AttemptRefused
    | -- | nothing answered within 'hairpinConnectSeconds'
      AttemptTimedOut
    | -- | this host has no way to the address, in the system's words
      AttemptUnreachable Text
    | -- | the connection failed otherwise: why
      AttemptFailed Text
    deriving (Eq, Show)

-- | Everything the judgement reads. Gathered by "Report", or made up by a test.
data Evidence = Evidence
    { evExternal :: Either Text External
    -- ^ the address connected to, or why none is known
    , evMapping :: MappingState
    , evLocal :: Maybe Text
    -- ^ the address of this host its connection to the external address leaves from
    , evListener :: Maybe (Either Text (Text, [Text]))
    -- ^ for a target with a local port: why it could not be bound, or the token served and the peers accepted
    , evAttempt :: Maybe Attempt
    -- ^ 'Nothing' when no connection was made
    }
    deriving (Eq, Show)

hairpinQuestion :: HairpinTarget -> Text
hairpinQuestion t =
    "Does the gateway hairpin a TCP connection from this host to "
        <> maybe ("its external address, port " <> showT (hpPort t)) (\a -> a <> ":" <> showT (hpPort t)) (hpAddress t)
        <> maybe "" (\l -> ", back to this host's port " <> showT l) (hpListen t)
        <> "?"

hairpinMethod :: HairpinTarget -> Text
hairpinMethod t =
    "one TCP connection from this host to the external address"
        <> maybe "" (\l -> ", one-time token served on local port " <> showT l) (hpListen t)
        <> "; upnpc -l for the gateway's mappings"

{- | The reasons not to connect at all, each an @unknown@: no external
address, an address that is this host's own (the connection would never
reach the gateway), a declared local port that is not listened on. 'Nothing'
when the connection is worth making.
-}
hairpinBlocked :: HairpinTarget -> Evidence -> Maybe Judgement
hairpinBlocked t ev = case evExternal ev of
    Left why -> Just (unknown [why] (Just "Without the external address there is nothing to connect to: declare it (--hairpin ADDR:PORT), or an --echo URL."))
    Right ext
        | (127 : _) <- fromMaybe [] (ipv4Octets (extAddress ext)) ->
            Just (unknown [externalLine ext] (Just "A loopback address is this host, not a gateway's external address: nothing would be hairpinned."))
        | evLocal ev == Just (extAddress ext) ->
            Just (unknown [externalLine ext, "this host holds that address itself"] (Just "A connection to one's own address never reaches the gateway: there is no NAT between this host and that address to hairpin through."))
        | otherwise -> case (hpListen t, evListener ev) of
            (Just p, Nothing) -> Just (unknown [externalLine ext, "no listener was opened on local port " <> showT p] Nothing)
            (Just _, Just (Left why)) -> Just (unknown [externalLine ext, why] Nothing)
            _ -> Nothing
  where
    unknown = Judgement Nothing

{- | What the evidence means. The token never shows in the evidence, and a
verdict other than @unknown@ is only given when the evidence settles it: see
the module's header.
-}
judgeHairpin :: HairpinTarget -> Evidence -> Judgement
judgeHairpin t ev = case (hairpinBlocked t ev, evExternal ev) of
    (Just j, _) -> j
    (Nothing, Left why) -> Judgement Nothing [why] Nothing
    (Nothing, Right ext) ->
        let target = extAddress ext <> ":" <> showT (hpPort t)
            listener = hpListen t >> evListener ev >>= either (const Nothing) Just
            before = [externalLine ext] <> ["the connection leaves from " <> l | Just l <- [evLocal ev]] <> [mappingLine t (evMapping ev)]
            after = maybe [] (arrivals . snd) listener
            say ok what meaning = Judgement ok (before <> what <> after) meaning
            listed = case evMapping ev of
                ListedHere _ -> True
                ListedElsewhere _ -> True
                _ -> False
            here = case evMapping ev of
                ListedHere _ -> True
                _ -> False
         in case evAttempt ev of
                Nothing -> say Nothing ["no connection was attempted"] Nothing
                Just (Accepted back) -> case listener of
                    Just (token, peers)
                        | token `elem` back ->
                            say (Just True) ["this host connected to " <> target <> " and read back the token it serves"] Nothing
                        | here && null peers ->
                            say
                                (Just False)
                                ["this host connected to " <> target <> " but did not read its own token"]
                                (Just "The gateway lists the mapping as leading to this report's listener, yet something else answered the connection made from inside: the gateway does not send connections from the LAN to its external address through its mappings (it answers them itself).")
                        | otherwise ->
                            say
                                Nothing
                                ["this host connected to " <> target <> " but did not read its own token"]
                                (Just "Something accepted at that address and port and was not shown to be this report's listener: the gateway itself, or a mapping to another host or port. A missing hairpin and a missing mapping look the same from here.")
                    Nothing
                        | listed ->
                            say
                                (Just True)
                                ["this host connected to " <> target]
                                (Just "Something accepted the connection and the gateway lists a mapping of that port. No token was exchanged (declare PORT@LISTEN, with the mapping leading to LISTEN, for that), so this does not show that the mapped host is what answered.")
                        | otherwise ->
                            say
                                Nothing
                                ["this host connected to " <> target]
                                (Just "Something accepted the connection, but no mapping of that port is known and no token was exchanged: a gateway answering on its own external address (an administration page) looks the same.")
                Just AttemptRefused
                    | here, Just (_, []) <- listener ->
                        say
                            (Just False)
                            ["the connection to " <> target <> " was refused"]
                            (Just "The gateway lists the mapping as leading to this report's listener, the listener saw nothing, and the connection was refused: it did not come back in. A firewall on this host that rejects the port would look the same.")
                    | otherwise ->
                        say
                            Nothing
                            ["the connection to " <> target <> " was refused"]
                            (Just "Refused by the gateway, or by the mapped host if nothing listens there. Without a mapping the gateway lists as leading to a listener of this report (PORT@LISTEN), a missing hairpin cannot be told from a missing mapping or a closed port.")
                Just AttemptTimedOut ->
                    say
                        Nothing
                        ["no answer from " <> target <> " within " <> showT hairpinConnectSeconds <> "s"]
                        (Just "Dropped somewhere: a gateway that does not hairpin, a firewall on the gateway or on this host, or no mapping at all. A timeout does not tell these apart.")
                Just (AttemptUnreachable why) ->
                    say Nothing [why] (Just "This host has no route to the address, which says nothing about hairpinning.")
                Just (AttemptFailed why) -> say Nothing [why] Nothing
  where
    arrivals [] = ["no connection reached the local listener"]
    arrivals peers = ["the local listener accepted a connection from " <> Text.intercalate ", " (take 3 (nub peers))]

externalLine :: External -> Text
externalLine ext =
    "the external address is "
        <> extAddress ext
        <> " ("
        <> cls
        <> "), "
        <> case extSource ext of
            Declared -> "as declared"
            FromGateway -> "as the gateway reports it"
            FromEcho -> "as the declared --echo URL answers"
  where
    cls = case classifyAddress (extAddress ext) of
        _ | (127 : _) <- fromMaybe [] (ipv4Octets (extAddress ext)) -> "loopback"
        Public -> "public"
        Private -> "private: the gateway is itself behind another NAT, and only this gateway is tested"
        CGNAT -> "carrier-grade NAT: the gateway is itself behind another NAT, and only this gateway is tested"
        Unparseable -> "unparseable"

mappingLine :: HairpinTarget -> MappingState -> Text
mappingLine t st = case st of
    Unlisted why -> "the gateway's mappings could not be listed: " <> why
    NotListed -> "the gateway lists no TCP mapping of external port " <> showT (hpPort t) <> " (a forwarding configured by hand on the router may not be listed)"
    ListedHere m -> listing m <> ", " <> maybe "this host" (const "this report's listener") (hpListen t)
    ListedElsewhere m -> listing m <> ", which is not " <> maybe "this host" (const "this report's listener") (hpListen t)
  where
    listing m = "the gateway lists TCP " <> showT (mapExternalPort m) <> " -> " <> fromMaybe "(unreadable address)" (parseIPv4 (mapInternalAddress m)) <> ":" <> showT (mapInternalPort m)

-------------------------------------------------------------------------------
-- The connection

-- | How long the connection may take, and how long the token is waited for once connected.
hairpinConnectSeconds, hairpinReadSeconds :: Int
hairpinConnectSeconds = 3
hairpinReadSeconds = 2

-- | The lines of what was read, cut down to the token's alphabet (as the inbound test's vantage does).
tokenLines :: ByteString.ByteString -> [Text]
tokenLines bytes =
    take 4 [l | l <- Text.filter tokenChar . Text.pack <$> lines (Char8.unpack (ByteString.take 256 bytes)), not (Text.null l)]
  where
    tokenChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '-'

{- | One TCP connection to an IPv4 address and port, bounded by the two
waits (microseconds), reading what the other side sends when asked to.
Nothing is written to the connection, and nothing here throws: what went
wrong is the 'Attempt'.
-}
connectAttempt :: Int -> Int -> Text -> Int -> Bool -> IO Attempt
connectAttempt connectMicros readMicros host port readBack = case ipv4Octets host of
    Just [a, b, c, d] -> do
        r <- try $ bracket (socket AF_INET Stream defaultProtocol) close $ \s -> do
            -- no child of this process (the other probes' tools) inherits the connection
            withFdSocket s setCloseOnExecIfNeeded
            connected <- timeout connectMicros (connect s (SockAddrInet (fromIntegral port) (tupleToHostAddress (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d))))
            case connected of
                Nothing -> pure AttemptTimedOut
                Just ()
                    | readBack -> Accepted . tokenLines <$> readSome s
                    | otherwise -> pure (Accepted [])
        pure (either classify id r)
    _ -> pure (AttemptFailed ("not a dotted IPv4 address: " <> host))
  where
    -- what arrives until the other side closes, 256 bytes came, or the wait is over
    readSome s = do
        acc <- newIORef ByteString.empty
        let go = do
                chunk <- recv s 256
                unless (ByteString.null chunk) $ do
                    modifyIORef' acc (<> chunk)
                    n <- ByteString.length <$> readIORef acc
                    when (n < 256) go
        _ <- try (timeout readMicros go) :: IO (Either SomeException (Maybe ()))
        readIORef acc
    classify :: IOException -> Attempt
    classify e = case Errno <$> ioe_errno e of
        Just n
            | n == eCONNREFUSED -> AttemptRefused
            | n == eTIMEDOUT -> AttemptTimedOut
            | n == eHOSTUNREACH || n == eNETUNREACH -> AttemptUnreachable (Text.pack (show e))
        _ -> AttemptFailed (Text.pack (show e))

{- | This host's address on the route to an IPv4 address: a connected UDP
socket sends nothing. 'Nothing' when there is no route.
-}
localAddressToward :: Text -> Int -> IO (Maybe Text)
localAddressToward host port = case ipv4Octets host of
    Just [a, b, c, d] -> do
        r <- try $ bracket (socket AF_INET Datagram defaultProtocol) close $ \s -> do
            connect s (SockAddrInet (fromIntegral port) (tupleToHostAddress (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)))
            getSocketName s
        pure $ case r of
            Right (SockAddrInet _ h) ->
                let (w, x, y, z) = hostAddressToTuple h
                 in Just (Text.intercalate "." (Text.pack . show <$> [w, x, y, z]))
            Right _ -> Nothing
            Left (_ :: SomeException) -> Nothing
    _ -> pure Nothing
