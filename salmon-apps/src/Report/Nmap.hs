{-# LANGUAGE OverloadedStrings #-}

{- | The pure half of @salmon-report@'s @nmap@ probe: a declared target read
into a host and a port list, the argument vector handed to @nmap@, and what
its grepable output (@-oG -@) says about each port. No IO here; "Report"
holds the shell-out.

What is scanned is exactly what the operator declared with @--nmap
HOST:PORTS@: one host (a name or one address, never a range, a mask or a
list) and an explicit, bounded port list. There is no default target and no
default port list, and nothing a declaration says reaches @nmap@ as an
option: the host is checked against a strict alphabet and the ports are
re-rendered from the parsed numbers.
-}
module Report.Nmap (
    -- * Declared targets
    NmapTarget (..),
    parseNmapTarget,
    checkHost,
    parsePorts,
    renderPorts,
    maxPorts,
    nmapArgs,

    -- * Grepable output
    PortState (..),
    NmapScan (..),
    parseNmapGrepable,
    portState,
    stateText,
) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit)
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- Declared targets

-- | One host and the TCP ports to look at, sorted and without duplicates.
data NmapTarget = NmapTarget
    { targetHost :: Text
    , targetPorts :: [Int]
    }
    deriving (Eq, Ord, Show)

-- | The most ports one target may declare: a report looks at named ports, it does not sweep.
maxPorts :: Int
maxPorts = 128

{- | Reads @HOST:PORTS@ (the last colon splits, so an IPv6 address goes in
brackets). The host is a DNS name, an IPv4 address or an IPv6 address; the
forms @nmap@ would expand into several hosts (@10.0.0.0\/24@, @10.0.0.1-9@,
@10.0.0.*@, a list) and anything starting with a dash are refused with a
reason.
-}
parseNmapTarget :: Text -> Either Text NmapTarget
parseNmapTarget raw = do
    let (h, p) = Text.breakOnEnd ":" (Text.strip raw)
    bracketed <- maybe (Left "not of the form HOST:PORTS") Right (Text.stripSuffix ":" h)
    host <- checkHost (Text.dropAround (`elem` ("[]" :: String)) bracketed)
    ports <- parsePorts p
    pure (NmapTarget host ports)

-- | One DNS name, IPv4 address or IPv6 address, from a strict alphabet; else the reason.
checkHost :: Text -> Either Text Text
checkHost h
    | Text.null h = Left "no host before the colon"
    | "-" `Text.isPrefixOf` h = Left "a host cannot start with a dash"
    | Text.any (== ':') h =
        if Text.all (\c -> isHexDigit c || c == ':' || c == '.') h
            then Right h
            else Left "not an IPv6 address"
    | not (Text.all (\c -> (isAlphaNum c && c < '\x80') || c `elem` ("._-" :: String)) h) =
        Left "a host is one DNS name or one address: no mask, list or wildcard"
    | Text.any isAlpha h = Right h
    | isIpv4 h = Right h
    | otherwise = Left "a host is one DNS name or one address: no address range"
  where
    isIpv4 t = case Text.splitOn "." t of
        os@[_, _, _, _] -> all octet os
        _ -> False
    octet o = not (Text.null o) && Text.all isDigit o && Text.length o <= 3 && maybe False (<= (255 :: Int)) (readMaybe (Text.unpack o))

{- | Reads a port list such as @22,80,8000-8010@ into sorted, distinct port
numbers; at most 'maxPorts' of them.
-}
parsePorts :: Text -> Either Text [Int]
parsePorts t = do
    parts <- traverse part (Text.splitOn "," (Text.strip t))
    let ports = nub (sort (concat parts))
    if length ports > maxPorts
        then Left ("more than " <> Text.pack (show maxPorts) <> " ports declared for one target")
        else Right ports
  where
    part x = case Text.splitOn "-" (Text.strip x) of
        [a] -> (: []) <$> port a
        [a, b] -> do
            lo <- port a
            hi <- port b
            if lo > hi
                then Left ("the port range " <> x <> " is backwards")
                else -- bounded before it is built: the cap is checked again on the whole list
                    if hi - lo >= maxPorts
                        then Left ("more than " <> Text.pack (show maxPorts) <> " ports declared for one target")
                        else Right [lo .. hi]
        _ -> Left ("not a port or a port range: " <> x)
    port x
        | not (Text.null x) && Text.all isDigit x && Text.length x <= 5
        , Just n <- readMaybe (Text.unpack x)
        , n >= 1 && n <= 65535 =
            Right n
        | otherwise = Left ("not a port between 1 and 65535: " <> if Text.null x then "(empty)" else x)

-- | The port list as @nmap -p@ takes it, with runs folded back into ranges.
renderPorts :: [Int] -> Text
renderPorts = Text.intercalate "," . fmap one . runs
  where
    runs [] = []
    runs (x : xs) = go x x xs
    go lo hi (y : ys) | y == hi + 1 = go lo y ys
    go lo hi rest = (lo, hi) : runs rest
    one (lo, hi)
        | lo == hi = Text.pack (show lo)
        | otherwise = Text.pack (show lo) <> "-" <> Text.pack (show hi)

{- | The arguments of the one scan a target gets: a TCP connect scan (no raw
sockets, so no privilege), no host discovery (@-Pn@: nothing but the declared
ports is touched, and a host that is away shows as filtered ports), no
reverse lookup, grepable output on stdout.
-}
nmapArgs :: NmapTarget -> [String]
nmapArgs t =
    ["-6" | Text.any (== ':') (targetHost t)]
        <> ["-sT", "-Pn", "-n", "-oG", "-", "-p", Text.unpack (renderPorts (targetPorts t)), Text.unpack (targetHost t)]

-------------------------------------------------------------------------------
-- Grepable output

data PortState
    = PortOpen
    | PortClosed
    | -- | nothing answered: a firewall dropped the attempt, or the host is away
      PortFiltered
    | -- | a state a connect scan does not normally give, as printed
      PortOther Text
    deriving (Eq, Show)

stateText :: PortState -> Text
stateText PortOpen = "open"
stateText PortClosed = "closed"
stateText PortFiltered = "filtered"
stateText (PortOther t) = t

readState :: Text -> PortState
readState "open" = PortOpen
readState "closed" = PortClosed
readState "filtered" = PortFiltered
readState t = PortOther t

{- | What one host's @Host:@ lines said. @nmap@ leaves out of the list the
ports of a state shared by many of them and prints the count instead
(@Ignored State: closed (100)@): those are 'scanIgnored'.
-}
data NmapScan = NmapScan
    { scanAddress :: Text
    , scanPorts :: Map.Map Int PortState
    -- ^ the TCP ports listed
    , scanIgnored :: [(PortState, Int)]
    }
    deriving (Eq, Show)

{- | Reads @nmap -oG -@ output for a single host. 'Nothing' when no @Host:@
line was printed (the name did not resolve, or nmap refused to start).
-}
parseNmapGrepable :: Text -> Maybe NmapScan
parseNmapGrepable out = case mapMaybe hostLine (Text.lines out) of
    [] -> Nothing
    ls@((addr, _, _) : _) ->
        -- one target is one host; lines about another address would be a surprise and are left out
        let mine = [(ps, ig) | (a, ps, ig) <- ls, a == addr]
         in Just (NmapScan addr (Map.fromList (concatMap fst mine)) (concatMap snd mine))
  where
    hostLine l = do
        rest <- Text.stripPrefix "Host: " l
        let fields = Text.splitOn "\t" rest
        addr <- case fields of
            (f : _) | (a : _) <- Text.words f -> Just a
            _ -> Nothing
        let ports = concat [mapMaybe portEntry (Text.splitOn "," v) | f <- fields, Just v <- [Text.stripPrefix "Ports:" f]]
            ignored = concat [mapMaybe ignoredEntry (Text.splitOn "," v) | f <- fields, Just v <- [Text.stripPrefix "Ignored State:" f]]
        pure (addr, ports, ignored)
    -- 22/open/tcp//ssh///
    portEntry e = case Text.splitOn "/" (Text.strip e) of
        (p : st : "tcp" : _) | Just n <- readMaybe (Text.unpack p) -> Just (n, readState st)
        _ -> Nothing
    -- closed (100)
    ignoredEntry e = case Text.words (Text.strip e) of
        [st, n] | Just k <- readMaybe (Text.unpack (Text.dropAround (`elem` ("()" :: String)) n)) -> Just (readState st, k)
        _ -> Nothing

{- | The state of one scanned port: as listed, else the one state the
unlisted ports share. With no such single state the output does not say, and
the reason is given.
-}
portState :: NmapScan -> Int -> Either Text PortState
portState scan p = case Map.lookup p (scanPorts scan) of
    Just st -> Right st
    Nothing -> case scanIgnored scan of
        [(st, _)] -> Right st
        [] -> Left "nmap printed no state for this port"
        several -> Left ("nmap did not list this port; the unlisted ones are " <> Text.intercalate ", " [stateText st <> " (" <> Text.pack (show n) <> ")" | (st, n) <- several])
