{-# LANGUAGE OverloadedStrings #-}

{- | Pure parsers for @upnpc@ (miniupnpc) output, and the classification of
the external address it reports.

Kept apart from "Salmon.Builtin.Nodes.PortMapping" so that anything else that
wants to know what a gateway offers (a capabilities report, a probe) can
import the parsers without importing a node.

The formats are those of miniupnpc 2.2.6. The "device answered but is not an
Internet Gateway Device" and "nothing answered" outputs were captured from a
real LAN; the listing format follows @upnpc.c@'s @-l@ and has not been seen
against a live IGD by the author of this module.
-}
module Salmon.Builtin.Nodes.PortMapping.Upnpc (
    Gateway (..),
    parseGateway,
    Mapping (..),
    Proto (..),
    protoText,
    parseMappings,
    AddressClass (..),
    classifyAddress,
) where

import Data.Char (isDigit, isSpace)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

data Proto = TCP | UDP
    deriving (Eq, Show)

protoText :: Proto -> Text
protoText TCP = "TCP"
protoText UDP = "UDP"

-- | What discovery found.
data Gateway
    = -- | nothing answered the multicast search
      NoDevice
    | -- | something answered, but it is not an Internet Gateway Device (e.g. a WPS @WFADevice.xml@)
      NotIgd
    | -- | a gateway, with the external address it reports and this host's LAN address when printed
      Igd {gwExternal :: Maybe Text, gwLocal :: Maybe Text}
    deriving (Eq, Show)

-- | Reads the output (stdout and stderr together) of @upnpc -s@ or @upnpc -l@.
parseGateway :: Text -> Gateway
parseGateway out
    | noIgd && deviceSeen = NotIgd
    | noIgd = NoDevice
    | otherwise = Igd (valueAfter "ExternalIPAddress = ") (valueAfter "Local LAN ip address : ")
  where
    ls = fmap Text.strip (Text.lines out)
    noIgd =
        any
            (\l -> "No valid UPNP Internet Gateway Device found" `Text.isInfixOf` l || "No IGD UPnP Device found" `Text.isInfixOf` l)
            ls
    deviceSeen = any ("UPnP device found. Is it an IGD" `Text.isInfixOf`) ls
    valueAfter key = case [Text.strip v | l <- ls, Just v <- [Text.stripPrefix key l]] of
        (v : _) | not (Text.null v) -> Just v
        _ -> Nothing

-- | One line of the redirection listing.
data Mapping = Mapping
    { mapProto :: Proto
    , mapExternalPort :: Int
    , mapInternalAddress :: Text
    , mapInternalPort :: Int
    , mapDescription :: Text
    , mapRemoteHost :: Text
    , mapLease :: Int
    -- ^ seconds, 0 for permanent
    }
    deriving (Eq, Show)

{- | The mappings in @upnpc -l@ output: lines of the form
@ 0 UDP 51820->192.168.1.5:51820 'description' 'remote host' 3600@.
Lines that are not of that shape are skipped.
-}
parseMappings :: Text -> [Mapping]
parseMappings = mapMaybe parseLine . Text.lines

parseLine :: Text -> Maybe Mapping
parseLine l0 = do
    let (idx, r1) = Text.break isSpace (Text.stripStart l0)
    if not (Text.null idx) && Text.all isDigit idx then Just () else Nothing
    let (protoTxt, r2) = Text.break isSpace (Text.stripStart r1)
    proto <- case protoTxt of
        "TCP" -> Just TCP
        "UDP" -> Just UDP
        _ -> Nothing
    let (spec, r3) = Text.break isSpace (Text.stripStart r2)
        (extTxt, afterArrow) = Text.breakOn "->" spec
    extPort <- readMaybe (Text.unpack extTxt)
    target <- Text.stripPrefix "->" afterArrow
    let (addrColon, portTxt) = Text.breakOnEnd ":" target
    inPort <- readMaybe (Text.unpack portTxt)
    inAddr <- Text.stripSuffix ":" addrColon
    -- the rest is  'description' 'remote host' lease
    let (quoted, leaseTxt) = Text.breakOnEnd "'" (Text.strip r3)
    lease <- readMaybe (Text.unpack (Text.strip leaseTxt))
    body <- Text.stripPrefix "'" quoted
    body' <- Text.stripSuffix "'" body
    let (beforeRemote, remote) = Text.breakOnEnd "'" body'
        desc = Text.dropWhileEnd (\c -> c == '\'' || isSpace c) beforeRemote
    pure (Mapping proto extPort inAddr inPort desc remote lease)

data AddressClass
    = Public
    | -- | RFC 1918, loopback or link-local: the gateway is itself behind a NAT
      Private
    | -- | 100.64.0.0/10, carrier-grade NAT: no mapping on this router can be reached
      CGNAT
    | Unparseable
    deriving (Eq, Show)

classifyAddress :: Text -> AddressClass
classifyAddress t = case traverse (readMaybe . Text.unpack) (Text.splitOn "." (Text.strip t)) :: Maybe [Int] of
    Just [a, b, c, d]
        | all (\x -> x >= 0 && x <= 255) [a, b, c, d] -> go a b
    _ -> Unparseable
  where
    go :: Int -> Int -> AddressClass
    go 10 _ = Private
    go 127 _ = Private
    go 172 b | b >= 16 && b <= 31 = Private
    go 192 168 = Private
    go 169 254 = Private
    go 100 b | b >= 64 && b <= 127 = CGNAT
    go _ _ = Public
