{-# LANGUAGE OverloadedStrings #-}

{- | The pure half of @salmon-report@'s STUN probe: a declared server read
into a host and a port, the bytes of a Binding request, what the bytes of a
Binding response say (RFC 8489, and the @OTHER-ADDRESS@ of RFC 5780), and
what the mapped addresses seen from one local socket imply about the NAT's
mapping behaviour (RFC 4787). No IO here; "Report" holds the UDP exchange.

Only the servers the operator declared with @--stun HOST:PORT@ are asked:
there is no default server, and the second address a server may advertise is
reported, never contacted.

What is judged is the /mapping/ half of the NAT type: whether the address
and port the NAT gives a local socket stay the same when the destination
changes. The /filtering/ half (what the NAT lets back in) needs a server that
answers from another address on request, and is not looked at.
-}
module Report.Stun (
    -- * Declared servers
    StunServer (..),
    parseStunServer,
    renderStunServer,

    -- * Messages
    TransactionId,
    mkTransactionId,
    bindingRequest,
    StunMessage (..),
    parseStunMessage,
    BindingAnswer (..),
    parseBindingResponse,

    -- * Addresses
    Family (..),
    Endpoint (..),
    endpointV4,
    endpointV6,
    renderEndpoint,

    -- * What the answers imply
    Outcome (..),
    Observation (..),
    Mapping (..),
    classifyMapping,
    judgeMapping,
) where

import Data.Bits (complement, shiftL, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isDigit, isHexDigit, isPrint)
import Data.List (nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Data.Word (Word16, Word8)
import Numeric (showHex)
import Text.Read (readMaybe)

import Report.Dns (Judgement (..))

-------------------------------------------------------------------------------
-- Declared servers

-- | One STUN server, as declared: a name or an address, and a UDP port.
data StunServer = StunServer
    { stunHost :: Text
    , stunPort :: Int
    }
    deriving (Eq, Ord, Show)

{- | Reads @HOST:PORT@ (the last colon splits, so an IPv6 address goes in
brackets). There is no default port: what is asked is what was written.
-}
parseStunServer :: Text -> Either Text StunServer
parseStunServer raw = do
    let (h, p) = Text.breakOnEnd ":" (Text.strip raw)
    bracketed <- maybe (Left "not of the form HOST:PORT") Right (Text.stripSuffix ":" h)
    let host = Text.dropAround (`elem` ("[]" :: String)) bracketed
    port <-
        if not (Text.null p) && Text.all isDigit p && Text.length p <= 5
            then maybe (Left "not a port") Right (readMaybe (Text.unpack p))
            else Left ("not a port between 1 and 65535: " <> if Text.null p then "(empty)" else p)
    if port < 1 || port > (65535 :: Int)
        then Left ("not a port between 1 and 65535: " <> p)
        else StunServer <$> checkHost host <*> pure port
  where
    checkHost h
        | Text.null h = Left "no host before the colon"
        | Text.any (== ':') h =
            if Text.all (\c -> isHexDigit c || c == ':' || c == '.') h then Right h else Left "not an IPv6 address"
        | Text.all (\c -> (isAlphaNum c && c < '\x80') || c `elem` ("._-" :: String)) h = Right h
        | otherwise = Left "a host is one DNS name or one address"

renderStunServer :: StunServer -> Text
renderStunServer (StunServer h p)
    | Text.any (== ':') h = "[" <> h <> "]:" <> Text.pack (show p)
    | otherwise = h <> ":" <> Text.pack (show p)

-------------------------------------------------------------------------------
-- Messages

-- | The 96 bits naming one request; a response is matched to its request by them.
newtype TransactionId = TransactionId ByteString
    deriving (Eq, Ord, Show)

-- | Twelve bytes, which the caller draws at random.
mkTransactionId :: ByteString -> Maybe TransactionId
mkTransactionId b
    | BS.length b == 12 = Just (TransactionId b)
    | otherwise = Nothing

magicCookie :: ByteString
magicCookie = BS.pack [0x21, 0x12, 0xa4, 0x42]

{- | A Binding request with no attribute: the twenty bytes of the header.
Nothing about this host is in it (no @SOFTWARE@, no credentials).
-}
bindingRequest :: TransactionId -> ByteString
bindingRequest (TransactionId t) = BS.pack [0x00, 0x01, 0x00, 0x00] <> magicCookie <> t

data StunMessage = StunMessage
    { msgType :: Word16
    , msgTransaction :: TransactionId
    , msgAttributes :: [(Word16, ByteString)]
    -- ^ type and value, padding removed, in the order sent
    }
    deriving (Eq, Show)

word16 :: ByteString -> Int -> Word16
word16 b i = (fromIntegral (BS.index b i) `shiftL` 8) .|. fromIntegral (BS.index b (i + 1))

{- | Reads one datagram as a STUN message, or says why it is not one. The
attributes are split, not interpreted; integrity and fingerprint attributes
are not verified (no credential is used, and the transaction id is what ties
an answer to its request).
-}
parseStunMessage :: ByteString -> Either Text StunMessage
parseStunMessage b
    | BS.length b < 20 = Left "shorter than a STUN header"
    | BS.index b 0 .&. 0xc0 /= 0 = Left "the first two bits are set: not a STUN message"
    | BS.take 4 (BS.drop 4 b) /= magicCookie = Left "no STUN magic cookie"
    | len `mod` 4 /= 0 = Left "a message length that is not a multiple of four"
    | BS.length b - 20 /= len = Left "the message length does not match the datagram"
    | otherwise = StunMessage (word16 b 0) (TransactionId (BS.take 12 (BS.drop 8 b))) <$> attributes (BS.drop 20 b)
  where
    len = fromIntegral (word16 b 2) :: Int
    attributes bs
        | BS.null bs = Right []
        | BS.length bs < 4 = Left "a truncated attribute header"
        | BS.length body < padded = Left "a truncated attribute"
        | otherwise = ((word16 bs 0, BS.take l body) :) <$> attributes (BS.drop padded body)
      where
        l = fromIntegral (word16 bs 2) :: Int
        padded = (l + 3) .&. complement 3
        body = BS.drop 4 bs

-- | What a server answered a Binding request with.
data BindingAnswer
    = {- | the address and port the request arrived from, and the second
      address the server advertises when it has one (@OTHER-ADDRESS@)
      -}
      Mapped Endpoint (Maybe Endpoint)
    | -- | an error response: its code and reason phrase
      Refused Int Text
    deriving (Eq, Show)

{- | Reads the response to the request of this transaction.
@XOR-MAPPED-ADDRESS@ is preferred and the plain @MAPPED-ADDRESS@ of older
servers accepted. A message of another transaction, or of another type, is a
reason.
-}
parseBindingResponse :: TransactionId -> ByteString -> Either Text BindingAnswer
parseBindingResponse tid@(TransactionId t) b = do
    msg <- parseStunMessage b
    if msgTransaction msg /= tid
        then Left "a message of another transaction"
        else case msgType msg of
            0x0101 -> do
                let attr k = lookup k (msgAttributes msg)
                    key = magicCookie <> t
                mapped <- case (attr 0x0020, attr 0x0001) of
                    (Just v, _) -> address (Just key) v
                    (Nothing, Just v) -> address Nothing v
                    (Nothing, Nothing) -> Left "a success response without a mapped address"
                -- OTHER-ADDRESS (RFC 5780), or the CHANGED-ADDRESS of RFC 3489 servers
                let other = case (attr 0x802c, attr 0x0005) of
                        (Just v, _) -> either (const Nothing) Just (address Nothing v)
                        (Nothing, Just v) -> either (const Nothing) Just (address Nothing v)
                        (Nothing, Nothing) -> Nothing
                pure (Mapped mapped other)
            0x0111 -> pure $ case lookup 0x0009 (msgAttributes msg) of
                Just v
                    | BS.length v >= 4 ->
                        Refused
                            (fromIntegral (BS.index v 2 .&. 7) * 100 + fromIntegral (BS.index v 3))
                            (Text.take 80 (Text.filter isPrint (Text.decodeUtf8With Text.lenientDecode (BS.drop 4 v))))
                _ -> Refused 0 ""
            ty -> Left ("not a Binding response (type 0x" <> Text.pack (showHex ty "") <> ")")

-- | An address attribute's value; with the key, one whose port and address are XORed with it.
address :: Maybe ByteString -> ByteString -> Either Text Endpoint
address key v
    | BS.length v < 4 = Left "a truncated address attribute"
    | otherwise = case (BS.index v 1, BS.length raw) of
        (1, 4) -> Right (endpointV4 addr port)
        (2, 16) -> Right (endpointV6 [(fromIntegral hi `shiftL` 8) .|. fromIntegral lo | (hi, lo) <- pairs addr] port)
        _ -> Left "an address attribute of an unknown family or length"
  where
    raw = BS.drop 4 v
    addr = maybe (BS.unpack raw) (\k -> BS.zipWith xor k raw) key
    port = fromIntegral (maybe id (const (xor 0x2112)) key (word16 v 2)) :: Int
    pairs (a : c : rest) = (a, c) : pairs rest
    pairs _ = []

-------------------------------------------------------------------------------
-- Addresses

data Family = V4 | V6
    deriving (Eq, Ord, Show)

-- | An address as text (an IPv6 one compressed, without brackets) and a port.
data Endpoint = Endpoint
    { epFamily :: Family
    , epHost :: Text
    , epPort :: Int
    }
    deriving (Eq, Ord, Show)

-- | From the four bytes of an IPv4 address.
endpointV4 :: [Word8] -> Int -> Endpoint
endpointV4 bytes = Endpoint V4 (Text.intercalate "." (Text.pack . show <$> bytes))

-- | From the eight groups of an IPv6 address; the longest run of zero groups is written @::@.
endpointV6 :: [Word16] -> Int -> Endpoint
endpointV6 gs = Endpoint V6 rendered
  where
    hexs = Text.intercalate ":" . fmap (\g -> Text.pack (showHex g ""))
    runs = [(i, length (takeWhile (== 0) (drop i gs))) | i <- [0 .. length gs - 1], gs !! i == 0, i == 0 || gs !! (i - 1) /= 0]
    rendered = case sortOn (negate . snd) (filter ((>= 2) . snd) runs) of
        ((i, n) : _) -> hexs (take i gs) <> "::" <> hexs (drop (i + n) gs)
        [] -> hexs gs

renderEndpoint :: Endpoint -> Text
renderEndpoint (Endpoint V4 h p) = h <> ":" <> Text.pack (show p)
renderEndpoint (Endpoint V6 h p) = "[" <> h <> "]:" <> Text.pack (show p)

-------------------------------------------------------------------------------
-- What the answers imply

data Outcome
    = Answered BindingAnswer
    | -- | the request was sent and nothing usable came back
      Silent Text
    | -- | the request could not be sent at all
      NotAsked Text
    deriving (Eq, Show)

-- | One declared server, asked from the one local socket of its address family.
data Observation = Observation
    { obsServer :: StunServer
    , obsDestination :: Maybe Endpoint
    -- ^ the address the name resolved to, which the request went to
    , obsLocal :: Maybe Endpoint
    -- ^ this host's own address toward it, and the socket's port
    , obsOutcome :: Outcome
    }
    deriving (Eq, Show)

-- | The mapping behaviour the answers of one address family show.
data Mapping
    = -- | every server saw the local address and port: nothing translates
      NoTranslation
    | -- | servers at distinct addresses saw one mapped address and port
      EndpointIndependent
    | -- | the mapped address or port changed with the destination
      DestinationDependent
    | -- | one destination answered: there is nothing to compare
      OneDestination
    | -- | the destinations differ by port only, and saw one mapping
      OneAddress
    deriving (Eq, Show)

{- | From @(destination, local, mapped)@ samples taken on one socket.
Differing mappings settle the question whatever the destinations are; equal
ones only do when the destinations are distinct addresses, since a NAT may
key its mappings on the destination address and not on its port.
-}
classifyMapping :: [(Endpoint, Maybe Endpoint, Endpoint)] -> Mapping
classifyMapping samples
    | length (nub [m | (_, _, m) <- samples]) > 1 = DestinationDependent
    | not (null samples) && all (\(_, l, m) -> l == Just m) samples = NoTranslation
    | length destinations < 2 = OneDestination
    | length (nub (epHost <$> destinations)) < 2 = OneAddress
    | otherwise = EndpointIndependent
  where
    destinations = nub [d | (d, _, _) <- samples]

{- | Whether the NAT keeps one mapping whatever the destination, from every
observation that learnt a mapped address. Each address family has its own
socket and is judged apart: a dependent mapping in either is a "no", and a
"yes" needs every family that answered to say so (an IPv6 path without
translation says nothing of the IPv4 NAT).
-}
judgeMapping :: [Observation] -> Judgement
judgeMapping obs = case [judgeFamily [s | s@(d, _, _) <- samples, epFamily d == fam] | fam <- nub [epFamily d | (d, _, _) <- samples]] of
    [] -> Judgement Nothing ["no declared STUN server gave a mapped address"] Nothing
    js@(first : _) ->
        let pick ok = [j | j <- js, jOk j == ok]
            decided = case (pick (Just False), pick Nothing) of
                (j : _, _) -> j
                ([], j : _) -> j
                ([], []) -> first
         in Judgement (jOk decided) (concatMap jEvidence js) (jMeaning decided)
  where
    samples = nub [(d, obsLocal o, m) | o <- obs, Just d <- [obsDestination o], Answered (Mapped m _) <- [obsOutcome o]]
    saw ss = [renderEndpoint d <> " sees " <> renderEndpoint m <> maybe "" (\l -> " for the local " <> renderEndpoint l) l' | (d, l', m) <- ss]
    judgeFamily ss = case classifyMapping ss of
        NoTranslation ->
            Judgement (Just True) (saw ss) (Just "No address translation on the path: the server sees this host's own address and port, so there is no mapping to keep. Whether a firewall lets inbound UDP through is not tested.")
        EndpointIndependent ->
            Judgement (Just True) (saw ss) (Just "Endpoint-independent mapping: the address a STUN server sees is the one any peer sees, so it can be handed to a peer for UDP hole punching. What the NAT lets back in (its filtering) is not tested.")
        DestinationDependent ->
            Judgement (Just False) (saw ss) (Just "The mapping depends on the destination (a \"symmetric\" NAT): the address a STUN server sees is not the one a peer will see, so exchanging STUN-learnt addresses does not punch a hole; plan for a relay or an explicitly mapped port.")
        OneDestination ->
            Judgement Nothing (saw ss <> ["one server address answered: a second --stun server at another address is needed to compare mappings"]) Nothing
        OneAddress ->
            Judgement Nothing (saw ss <> ["the answers come from one address on several ports: the mapping does not depend on the destination port, and a second --stun server at another address is needed to tell whether it depends on the destination address"]) Nothing
