{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | @salmon-report@: a read-only report of what a network can do, built from
probes (feature 4e859358). Nothing here converges anything.

> salmon-report config --name example.org --tcp example.org:443 | salmon-report run report [--json]

A /probe/ is an ordinary 'Op' whose 'check' gathers one fact. Its @up@ throws
and is never called: the driver ('runReport') expands the graph, evaluates
every probe's @check@ concurrently with a per-probe timeout, and prints one
'Finding' per probe.

Decisions taken for v1:

* The driver is private to this binary, not a generic @run report@ in
  'Salmon.Builtin.CommandLine'.
* Evidence travels through a 'Collector' the probes write to (keyed by the
  probe's 'Ref'), not through a widened 'CheckResult'; the 'CheckResult' a
  probe returns is only the coarse verdict.
* The report states facts, and a short "meaning" for the few facts the tree's
  own designs depend on (UPnP-IGD absence, CGNAT).
* No third-party service is contacted: the external address comes from the
  gateway, or from an echo URL the operator declares with @--echo@.

DNS setup probes (@--domain@, see 'DomainSpec' and "Report.Dns") ask whether
a domain is delegated to the name servers of the zone meant to serve it,
whether those servers agree, and whether declared names resolve to what the
zone says. The expected servers come from @--ns@ or, read with @gcloud@, from
a Cloud DNS zone (@--zone PROJECT\/ZONE@). These probes send DNS queries to
the parent zone's servers, the expected servers and a resolver: that is their
subject, not a third-party service.

Port probes (@--nmap HOST:PORTS@, see "Report.Nmap") run @nmap@ against the
hosts and ports the operator declares and file one finding per port: open,
closed or filtered. There is no default target and no default port list; one
declaration is one host, and one unprivileged TCP connect scan of its ports.

Not done (follow-ups in the feature): STUN, hairpin NAT, inbound
reachability from a second vantage. The @natpmpc@ parser follows the
tool's documented output and has no captured fixture yet.
-}
module Report (
    main,

    -- * Seed and findings
    Spec (..),
    DomainSpec (..),
    Verdict (..),
    Finding (..),
    findingValue,
    renderFinding,

    -- * Driver
    Collector,
    newCollector,
    probeOp,
    reportOp,
    runReport,

    -- * Probes
    probesFor,
    probesWith,
    dnsSetupProbes,
    expectedServers,
    parseZoneRef,
    nmapProbes,
    nmapFinding,
    scanNmap,

    -- * Pure parsers (exposed for tests)
    parseDigAddresses,
    parseNatpmpc,
    NatpmpResult (..),
    parseHostPort,
    describeClass,
    crossCheck,
) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad (forM_)
import Data.Aeson (FromJSON (..), ToJSON, Value, object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString.Lazy.Char8 as LChar
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as Http
import Network.Socket (AddrInfo (..), SocketType (Stream), close, connect, defaultHints, getAddrInfo, socket)
import qualified Options.Applicative as O
import System.Exit (ExitCode (..))
import System.IO (hFlush, stdout)
import System.IO.Error (ioeGetErrorString)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import Salmon.Actions.UpDown (CheckResult (Failure, Success), expandDag)
import qualified Salmon.Actions.UpDown as UpDown
import Report.Dns
import Report.Nmap
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Gcp.CloudDns as CloudDns
import Salmon.Builtin.Nodes.Gcp.Core (Project (..))
import Salmon.Builtin.Nodes.PortMapping.Upnpc
import Salmon.Op.Dag (Dag (..), dagOrder)
import Data.Functor.Identity (runIdentity)
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Reporter (reportPrint)

-------------------------------------------------------------------------------

-- | What an operator declares: every target is named, nothing is discovered by scanning.
data Spec = Spec
    { specNames :: [Text]
    -- ^ DNS names to resolve
    , specTcp :: [Text]
    -- ^ outbound @host:port@ pairs
    , specEcho :: Maybe Text
    -- ^ an HTTP(S) URL answering with the caller's address as plain text
    , specGateway :: Maybe Text
    -- ^ gateway address, handed to @natpmpc -g@
    , specUpnp :: Bool
    , specNatpmp :: Bool
    , specDomain :: Maybe DomainSpec
    -- ^ a domain whose DNS setup is reported on
    , specNmap :: [Text]
    -- ^ @HOST:PORTS@ targets handed to @nmap@, see 'parseNmapTarget'
    }
    deriving (Eq, Show, Generic)

instance ToJSON Spec

{- | Field for field what the generic instance reads, except that a directive
written before 'specNmap' existed still reads (as no target).
-}
instance FromJSON Spec where
    parseJSON = withObject "Spec" $ \o ->
        Spec
            <$> o .: "specNames"
            <*> o .: "specTcp"
            <*> o .:? "specEcho"
            <*> o .:? "specGateway"
            <*> o .: "specUpnp"
            <*> o .: "specNatpmp"
            <*> o .:? "specDomain"
            <*> o .:? "specNmap" .!= []

{- | A domain and the hosted zone meant to serve it. The expected name
servers are 'domNameServers' when given, and otherwise read from the Cloud
DNS zone 'domZone' names.
-}
data DomainSpec = DomainSpec
    { domName :: Text
    , domNameServers :: [Text]
    , domZone :: Maybe Text
    -- ^ @PROJECT\/ZONE@, a Cloud DNS managed zone
    , domRecords :: [Text]
    -- ^ @[TYPE:]NAME[=VALUE,...]@, see 'parseRecordQuery'
    , domResolver :: Maybe Text
    -- ^ the resolver asked for what the world sees; the system's when absent
    }
    deriving (Eq, Show, Generic)

instance ToJSON DomainSpec
instance FromJSON DomainSpec

data Verdict = Yes | No | Unknown
    deriving (Eq, Show, Generic)

instance ToJSON Verdict where
    toJSON v = Aeson.String (verdictText v)

verdictText :: Verdict -> Text
verdictText Yes = "yes"
verdictText No = "no"
verdictText Unknown = "unknown"

-- | One capability: a question, a verdict, the evidence and the method.
data Finding = Finding
    { fQuestion :: Text
    , fVerdict :: Verdict
    , fEvidence :: [Text]
    , fMethod :: Text
    , fMeaning :: Maybe Text
    , fAddresses :: [Text]
    -- ^ addresses this finding learnt (external address, resolved names), for 'crossCheck'
    }
    deriving (Eq, Show)

findingValue :: Finding -> Value
findingValue f =
    object $
        [ "question" .= fQuestion f
        , "verdict" .= fVerdict f
        , "evidence" .= fEvidence f
        , "method" .= fMethod f
        ]
            <> maybe [] (\m -> ["meaning" .= m]) (fMeaning f)

renderFinding :: Finding -> Text
renderFinding f =
    Text.unlines $
        [ "[" <> verdictText (fVerdict f) <> "] " <> fQuestion f
        , "    method: " <> fMethod f
        ]
            <> fmap ("    evidence: " <>) (fEvidence f)
            <> maybe [] (\m -> ["    meaning: " <> m]) (fMeaning f)

-------------------------------------------------------------------------------
-- Driver

-- | Evidence written by probes while their @check@ runs, keyed by the probe's ref.
newtype Collector = Collector (IORef (Map.Map Ref Finding))

newCollector :: IO Collector
newCollector = Collector <$> newIORef Map.empty

-- | Marks a node as a probe, so the driver ignores the root and anything else.
data ProbeMark = ProbeMark Text
    deriving (Show)

markOf :: [Dynamic] -> Maybe Text
markOf ds = case mapMaybe fromDynamic ds of
    (ProbeMark q : _) -> Just q
    [] -> Nothing

{- | A read-only probe. The check runs the action, files the 'Finding' and
answers the coarse verdict; @up@ and @down@ throw/refuse: a report never converges.
-}
probeOp :: Collector -> Text -> IO Finding -> Op
probeOp (Collector cell) question action =
    op
        "probe"
        nodeps
        ( \e ->
            e
                { help = question
                , ref = key
                , dynamics = [toDyn (ProbeMark question)]
                , up = throwIO (userError "salmon-report probes are read-only")
                , down = pure ()
                , check = do
                    f <- action
                    atomicModifyIORef' cell (\m -> (Map.insert key f m, ()))
                    pure $ case fVerdict f of
                        Yes -> Success
                        No -> Failure (Text.intercalate "; " (fEvidence f))
                        Unknown -> UpDown.Unknown
                }
        )
  where
    key = mkRef "salmon-report-probe" question

-- | The root that hangs the probes together.
reportOp :: [Op] -> Op
reportOp ps = op "report" (deps ps) id

{- | Evaluate every probe's @check@ concurrently, each within @micros@; a
timeout or an exception becomes an @unknown@ finding naming the cause.
Findings come back in graph order.
-}
runReport :: Int -> Collector -> Op -> IO [Finding]
runReport micros (Collector cell) root = do
    dag <- expandDag reportPrint (pure . runIdentity) root
    let probes =
            [ (r, q, a.extension)
            | r <- dagOrder dag
            , Just a <- [Map.lookup r (dagNodes dag)]
            , Just q <- [markOf a.extension.dynamics]
            ]
    forConcurrently probes $ \(r, q, ext) -> do
        res <- try (timeout micros (ext.check >>= evaluate))
        found <- Map.lookup r <$> readIORef cell
        pure $ case (res, found) of
            (Right (Just _), Just f) -> f
            (Right (Just _), Nothing) -> unknownFor q "the probe filed no finding" "check"
            (Left (e :: SomeException), _) -> unknownFor q ("the probe failed: " <> Text.pack (show e)) "check"
            (Right Nothing, _) -> unknownFor q ("timed out after " <> Text.pack (show (micros `div` 1000000)) <> "s") "check"

unknownFor :: Text -> Text -> Text -> Finding
unknownFor q why method = Finding q Unknown [why] method Nothing []

-------------------------------------------------------------------------------
-- Probes

{- | The probes a 'Spec' asks for. Nothing is shared between them: each port
probe of an @--nmap@ target runs its own scan, where 'main' runs one per target.
-}
probesFor :: Collector -> Spec -> [Op]
probesFor c = probesWith c expectedServers scanNmap

{- | 'probesFor' with the read of a domain's expected name servers and the
scan of an @nmap@ target supplied, so that a caller can share one of each
between the probes (or fake them).
-}
probesWith :: Collector -> (DomainSpec -> IO (Either Text [Text])) -> (NmapTarget -> IO (Either Text NmapScan)) -> Spec -> [Op]
probesWith c expected scan spec =
    concat
        [ [probeOp c "Does the router offer UPnP-IGD port mapping?" upnpProbe | specUpnp spec]
        , [probeOp c "Does the router answer NAT-PMP / PCP?" (natpmpProbe (specGateway spec)) | specNatpmp spec]
        , [probeOp c "What is the external address, and can it be mapped to?" (externalProbe (specEcho spec)) | specUpnp spec || isJust (specEcho spec)]
        , [probeOp c ("Does " <> n <> " resolve?") (dnsProbe n) | n <- specNames spec]
        , [probeOp c ("Can this host open TCP to " <> hp <> "?") (tcpProbe hp) | hp <- specTcp spec]
        , [probeOp c "Does this host have a global IPv6 address?" ipv6Probe]
        , maybe [] (\d -> dnsSetupProbes c (expected d) d) (specDomain spec)
        , concatMap (nmapProbes c scan) (specNmap spec)
        ]

-- | Runs a command, treating a missing binary as a reason rather than a crash.
run :: FilePath -> [String] -> IO (Either Text (ExitCode, Text))
run cmd args = do
    r <- try (readProcessWithExitCode cmd args "")
    pure $ case r of
        Left (e :: IOError) -> Left (Text.pack cmd <> " could not run: " <> Text.pack (ioeGetErrorString e))
        Right (code, out, err) -> Right (code, Text.pack (out <> err))

upnpProbe :: IO Finding
upnpProbe = do
    r <- run "upnpc" ["-s"]
    pure $ case r of
        Left why -> mk Unknown [why] Nothing
        Right (_, out) -> case parseGateway out of
            NoDevice -> mk No ["nothing answered the multicast search"] (Just "No UPnP device on this LAN, so a mapping cannot be requested; configure the port map by hand on the router.")
            NotIgd -> mk No ["a UPnP device answered but it is not an Internet Gateway Device"] (Just "A device answered (often a WPS description) but cannot map ports: enable UPnP in the router's settings, or map by hand.")
            Igd ext loc -> mk Yes (maybe [] (\e -> ["external address " <> e]) ext <> maybe [] (\l -> ["local address " <> l]) loc) Nothing
  where
    mk v ev m = Finding "Does the router offer UPnP-IGD port mapping?" v ev "upnpc -s" m []

data NatpmpResult = NatpmpPublic Text | NatpmpNoGateway | NatpmpNoAnswer
    deriving (Eq, Show)

-- | Reads @natpmpc@ output: a @Public IP address : A@ line is an answer.
parseNatpmpc :: Text -> NatpmpResult
parseNatpmpc out
    | (a : _) <- [Text.strip v | l <- ls, Just v <- [Text.stripPrefix "Public IP address :" l]], not (Text.null a) = NatpmpPublic a
    | any ("Cannot get default gateway" `Text.isInfixOf`) ls || any ("Cannot get gateway" `Text.isInfixOf`) ls = NatpmpNoGateway
    | otherwise = NatpmpNoAnswer
  where
    ls = fmap Text.strip (Text.lines out)

natpmpProbe :: Maybe Text -> IO Finding
natpmpProbe gw = do
    r <- run "natpmpc" (maybe [] (\g -> ["-g", Text.unpack g]) gw)
    pure $ case r of
        Left why -> mk Unknown [why]
        Right (_, out) -> case parseNatpmpc out of
            NatpmpPublic a -> (mk Yes ["public address " <> a]){fAddresses = [a]}
            NatpmpNoGateway -> mk Unknown ["natpmpc found no gateway; declare one with --gateway"]
            NatpmpNoAnswer -> mk No ["no NAT-PMP answer"]
  where
    mk v ev = Finding "Does the router answer NAT-PMP / PCP?" v ev "natpmpc" Nothing []

describeClass :: AddressClass -> Text
describeClass Public = "public"
describeClass Private = "private"
describeClass CGNAT = "carrier-grade NAT (100.64.0.0/10)"
describeClass Unparseable = "unparseable"

externalProbe :: Maybe Text -> IO Finding
externalProbe echo = do
    fromGw <- do
        r <- run "upnpc" ["-s"]
        pure $ case r of
            Right (_, out) | Igd (Just e) _ <- parseGateway out -> Just (e, "upnpc -s (the gateway's own report)")
            _ -> Nothing
    found <- case fromGw of
        Just x -> pure (Just x)
        Nothing -> case echo of
            Nothing -> pure Nothing
            Just url -> do
                r <- try (fetch url) :: IO (Either SomeException Text)
                pure $ case r of
                    Right body | not (Text.null body) -> Just (body, "GET " <> url <> " (declared echo)")
                    _ -> Nothing
    pure $ case found of
        Nothing -> Finding q Unknown ["no gateway reported an address and no working --echo URL was declared"] "upnpc -s, --echo" Nothing []
        Just (addr, how) ->
            let cls = classifyAddress addr
                (v, meaning) = case cls of
                    Public -> (Yes, Nothing)
                    Private -> (No, Just "The address is private: this router is itself behind another NAT, so no mapping on it is reachable from the Internet.")
                    CGNAT -> (No, Just "Carrier-grade NAT: double NAT, no port mapping will be reachable from the Internet.")
                    Unparseable -> (Unknown, Nothing)
             in Finding q v [addr <> " is " <> describeClass cls] how meaning [addr]
  where
    q = "What is the external address, and can it be mapped to?"
    fetch :: Text -> IO Text
    fetch url = do
        mgr <- Http.newManager Http.defaultManagerSettings
        req <- Http.parseRequest (Text.unpack url)
        resp <- Http.httpLbs req{Http.responseTimeout = Http.responseTimeoutMicro 5000000} mgr
        pure (Text.strip (Text.pack (LChar.unpack (LChar.take 64 (Http.responseBody resp)))))

-- | Addresses on the lines of @dig +short@ output (CNAME targets are skipped).
parseDigAddresses :: Text -> [Text]
parseDigAddresses = filter isAddr . fmap Text.strip . Text.lines
  where
    isAddr t = classifyAddress t /= Unparseable || (":" `Text.isInfixOf` t && Text.all (`elem` ("0123456789abcdefABCDEF:." :: String)) t && not (Text.null t))

dnsProbe :: Text -> IO Finding
dnsProbe n = do
    a <- run "dig" ["+short", "+time=3", "+tries=1", "A", Text.unpack n]
    aaaa <- run "dig" ["+short", "+time=3", "+tries=1", "AAAA", Text.unpack n]
    pure $ case (a, aaaa) of
        (Left why, _) -> mk Unknown [why] []
        (Right (_, o1), r2) ->
            let addrs = parseDigAddresses o1 <> either (const []) (parseDigAddresses . snd) r2
             in if null addrs
                    then mk No ["no A or AAAA record"] []
                    else mk Yes ((\x -> n <> " -> " <> x <> " (" <> (if ":" `Text.isInfixOf` x then "IPv6" else describeClass (classifyAddress x)) <> ")") <$> addrs) addrs
  where
    mk v ev as = Finding ("Does " <> n <> " resolve?") v ev "dig +short A/AAAA" Nothing as

-- | Splits @host:port@ (the last colon wins, so a bare IPv6 needs brackets).
parseHostPort :: Text -> Maybe (Text, Int)
parseHostPort t = do
    let (h, p) = Text.breakOnEnd ":" t
    host <- Text.stripSuffix ":" h
    port <- readMaybe (Text.unpack p)
    if Text.null host || port < 1 || port > 65535 then Nothing else Just (Text.dropAround (`elem` ("[]" :: String)) host, port)

tcpProbe :: Text -> IO Finding
tcpProbe hp = case parseHostPort hp of
    Nothing -> pure (mk Unknown ["not of the form host:port"])
    Just (h, p) -> do
        r <- try $ do
            addrs <- getAddrInfo (Just defaultHints{addrSocketType = Stream}) (Just (Text.unpack h)) (Just (show p))
            case addrs of
                [] -> throwIO (userError "no address")
                (ai : _) -> do
                    s <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
                    (connect s (addrAddress ai) >> pure (addrAddress ai)) `finallyClose` s
        pure $ case r of
            Right sa -> mk Yes ["connected to " <> Text.pack (show sa)]
            Left (e :: SomeException) -> mk No [Text.pack (show e)]
  where
    mk v ev = Finding ("Can this host open TCP to " <> hp <> "?") v ev "TCP connect" Nothing []
    finallyClose act s = do
        x <- try act
        close s
        either (\(e :: SomeException) -> throwIO e) pure x

ipv6Probe :: IO Finding
ipv6Probe = do
    r <- run "ip" ["-6", "addr", "show", "scope", "global"]
    pure $ case r of
        Left why -> mk Unknown [why]
        Right (_, out) ->
            let as = [w | l <- Text.lines out, ("inet6" : w : _) <- [Text.words (Text.strip l)]]
             in if null as then mk No ["no global-scope IPv6 address"] else mk Yes (("address " <>) <$> as)
  where
    mk v ev = Finding "Does this host have a global IPv6 address?" v ev "ip -6 addr show scope global" Nothing []

-------------------------------------------------------------------------------
-- nmap

{- | The probes of one declared @HOST:PORTS@ target: one per port, all
reading the same scan. A declaration that does not parse is one probe
saying why, and scans nothing.
-}
nmapProbes :: Collector -> (NmapTarget -> IO (Either Text NmapScan)) -> Text -> [Op]
nmapProbes c scan raw = case parseNmapTarget raw of
    Left why ->
        let q = "Which ports of the nmap target " <> raw <> " are open, seen from this host?"
         in [probeOp c q (pure (Finding q Unknown [why <> " (expected HOST:PORTS, as in example.org:22,443)"] "nmap, not run" Nothing []))]
    Right t -> [probeOp c (nmapQuestion t p) (nmapFinding t p <$> scan t) | p <- targetPorts t]

nmapQuestion :: NmapTarget -> Int -> Text
nmapQuestion t p = "Is TCP port " <> Text.pack (show p) <> " of " <> targetHost t <> " open, seen from this host?"

{- | One scan of one target: @nmap@ with 'nmapArgs', its output read by
'parseNmapGrepable'. A scan that printed no host is a reason, made of what
nmap said instead.
-}
scanNmap :: NmapTarget -> IO (Either Text NmapScan)
scanNmap t = do
    r <- run "nmap" (nmapArgs t)
    pure $ case r of
        Left why -> Left why
        Right (code, out) -> case parseNmapGrepable out of
            Just s -> Right s
            Nothing ->
                let said = [l | l <- Text.strip <$> Text.lines out, not (Text.null l), not ("#" `Text.isPrefixOf` l)]
                 in Left (Text.intercalate "; " (take 3 said <> ["nmap reported no host" <> exited code]))
  where
    exited ExitSuccess = ""
    exited (ExitFailure n) = " (exit " <> Text.pack (show n) <> ")"

-- | What a scan (or the reason there is none) says about one port of its target.
nmapFinding :: NmapTarget -> Int -> Either Text NmapScan -> Finding
nmapFinding t p res = case res of
    Left why -> mk Unknown [why] Nothing
    Right s -> case portState s p of
        Left why -> mk Unknown [why] Nothing
        Right st ->
            let ev = [Text.pack (show p) <> "/tcp is " <> stateText st <> " on " <> scanAddress s]
             in case st of
                    PortOpen -> mk Yes ev Nothing
                    PortClosed -> mk No ev (Just "Closed: the host answered and refused, so it is reachable and nothing listens on this port.")
                    PortFiltered -> mk Unknown ev (Just "Filtered: nothing answered, so a firewall drops the attempt or the host is away; whether a service listens cannot be told from here.")
                    PortOther _ -> mk Unknown ev Nothing
  where
    mk v ev m = Finding (nmapQuestion t p) v ev (Text.pack (unwords ("nmap" : nmapArgs t))) m []

-------------------------------------------------------------------------------
-- DNS setup

-- | Splits @PROJECT\/ZONE@.
parseZoneRef :: Text -> Maybe (Text, Text)
parseZoneRef t = case Text.splitOn "/" (Text.strip t) of
    [p, z] | not (Text.null p), not (Text.null z) -> Just (p, z)
    _ -> Nothing

{- | The name servers the domain should be delegated to: the declared ones,
else those Cloud DNS assigned to the declared zone (a read-only
@gcloud dns managed-zones describe@), else a reason there are none.
-}
expectedServers :: DomainSpec -> IO (Either Text [Text])
expectedServers d
    | not (null (domNameServers d)) = pure (Right (normalizeName <$> domNameServers d))
    | Just z <- domZone d = case parseZoneRef z of
        Nothing -> pure (Left ("--zone " <> z <> " is not of the form PROJECT/ZONE"))
        Just (p, zn) -> do
            r <- try (CloudDns.readNameServers (CloudDns.ManagedZone zn (Project p) (domName d) ""))
            pure $ case r of
                Right (Just ns) -> Right (normalizeName <$> ns)
                Right Nothing -> Left ("gcloud could not describe the Cloud DNS zone " <> z <> " (absent, or not readable by this account)")
                Left (e :: SomeException) -> Left ("gcloud could not run: " <> Text.pack (show e))
    | otherwise = pure (Left "no expected name servers: declare them with --ns, or a Cloud DNS zone with --zone")

-- | Runs an action at most once, handing every caller the first result.
once :: IO a -> IO (IO a)
once act = do
    cell <- newMVar Nothing
    pure $ modifyMVar cell $ \m -> case m of
        Just x -> pure (m, x)
        Nothing -> act >>= \x -> pure (Just x, x)

-- | @dig@ printing its header and the answer section, plus whatever the flags add.
dig :: [String] -> Text -> Text -> Maybe Text -> IO (Either Text DigAnswer)
dig flags ty name server = do
    r <- run "dig" (["+time=2", "+tries=1", "+noall", "+comments", "+answer"] <> flags <> [Text.unpack ty, Text.unpack name] <> maybe [] (\s -> ["@" <> Text.unpack s]) server)
    pure (parseDig . snd <$> r)

-- | The two DNS setup probes for one domain, plus one per declared record.
dnsSetupProbes :: Collector -> IO (Either Text [Text]) -> DomainSpec -> [Op]
dnsSetupProbes c expected d =
    [ probeOp c delegationQ (delegationProbe delegationQ expected domain)
    , probeOp c zoneQ (zoneProbe zoneQ expected domain)
    ]
        <> [probeOp c (recordQ r) (recordProbe (recordQ r) expected (domResolver d) r) | r <- domRecords d]
  where
    domain = normalizeName (domName d)
    delegationQ = "Is " <> domain <> " delegated to the expected name servers?"
    zoneQ = "Do the expected name servers of " <> domain <> " agree?"
    recordQ r = "Does " <> r <> " resolve to what the zone says?"

-- | The first of these servers that answers at all, with its answer.
firstAnswer :: [Text] -> (Text -> IO (Either Text DigAnswer)) -> IO (Maybe (Text, DigAnswer))
firstAnswer [] _ = pure Nothing
firstAnswer (s : rest) ask = do
    r <- ask s
    case r of
        Right a | isJust (digStatus a) -> pure (Just (s, a))
        _ -> firstAnswer rest ask

delegationProbe :: Text -> IO (Either Text [Text]) -> Text -> IO Finding
delegationProbe q expected domain = do
    parent <- findParent (parentCandidates domain)
    case parent of
        Left why -> pure (mk Unknown [why] Nothing)
        Right (zone, servers) -> do
            answer <- firstAnswer (take 3 servers) (dig ["+authority", "+norecurse"] "NS" domain . Just)
            case answer of
                Nothing -> pure (mk Unknown ["none of the name servers of " <> zone <> " answered"] Nothing)
                Just (server, a) -> do
                    j <- (\e -> judgeDelegation domain e a) <$> expected
                    pure (mk (verdictOf j) (jEvidence j <> ["asked " <> server <> ", a name server of " <> zone]) (jMeaning j))
  where
    mk v ev m = Finding q v ev "dig +norecurse NS, asked of the parent zone's servers" m []
    findParent [] = pure (Left ("no parent zone with name servers was found for " <> domain))
    findParent (z : zs) = do
        r <- run "dig" ["+short", "+time=2", "+tries=1", "NS", Text.unpack z]
        case r of
            Left why -> pure (Left why)
            Right (_, out) -> case parseDigNames out of
                [] -> findParent zs
                ns -> pure (Right (z, ns))

zoneProbe :: Text -> IO (Either Text [Text]) -> Text -> IO Finding
zoneProbe q expected domain = do
    e <- expected
    case e of
        Left why -> pure (mk Unknown [why] Nothing)
        Right servers -> do
            views <- forConcurrently servers $ \s -> do
                ns <- dig ["+norecurse"] "NS" domain (Just s)
                soa <- dig ["+norecurse"] "SOA" domain (Just s)
                pure (s, serverView domain ns soa)
            let j = judgeZone servers views
            pure (mk (verdictOf j) (jEvidence j) (jMeaning j))
  where
    mk v ev m = Finding q v ev "dig +norecurse NS and SOA, asked of each expected server" m []

recordProbe :: Text -> IO (Either Text [Text]) -> Maybe Text -> Text -> IO Finding
recordProbe q expected resolver raw = case parseRecordQuery raw of
    Left why -> pure (mk Unknown [why] Nothing)
    Right rq -> do
        e <- expected
        fromZone <- case e of
            Left _ -> pure Nothing
            Right servers -> firstAnswer servers $ \s -> do
                r <- dig ["+norecurse"] (rqType rq) (rqName rq) (Just s)
                -- a server that does not hold the zone is not the zone speaking
                pure (r >>= \a -> if digAuthoritative a then Right a else Left "not authoritative")
        fromResolver <- dig [] (rqType rq) (rqName rq) resolver
        let seen = either (const Nothing) (\a -> if isJust (digStatus a) then Just a else Nothing) fromResolver
            (zs, rs) = case (fromZone, seen) of
                (Just (_, z), Just r) -> let (a, b) = answerValues rq z r in (Just a, Just b)
                (Just (_, z), Nothing) -> (Just (fst (answerValues rq z z)), Nothing)
                (Nothing, Just r) -> (Nothing, Just (snd (answerValues rq r r)))
                (Nothing, Nothing) -> (Nothing, Nothing)
            j = judgeRecord rq zs rs
            who = maybe [] (\(s, _) -> ["asked " <> s <> " for the zone"]) fromZone <> either (\why -> [why]) (const []) e
        pure (mk (verdictOf j) (jEvidence j <> who) (jMeaning j))
  where
    mk v ev m = Finding q v ev ("dig, asked of the zone's servers and of " <> maybe "the system resolver" ("the resolver " <>) resolver) m []

verdictOf :: Judgement -> Verdict
verdictOf j = case jOk j of
    Just True -> Yes
    Just False -> No
    Nothing -> Unknown

{- | Joins what separate probes learnt: for each resolved name, whether it
points at the external address. Appended to the evidence of the DNS findings.
-}
crossCheck :: [Finding] -> [Finding]
crossCheck fs = fmap annotate fs
  where
    external = case [a | f <- fs, "external address" `Text.isInfixOf` fQuestion f, a <- fAddresses f] of
        (a : _) -> Just a
        [] -> Nothing
    annotate f = case external of
        Just e
            | "resolve?" `Text.isSuffixOf` fQuestion f
            , not (null (fAddresses f)) ->
                f{fEvidence = fEvidence f <> [if e `elem` fAddresses f then "points at the external address " <> e else "does not point at the external address " <> e]}
        _ -> f

-------------------------------------------------------------------------------
-- Command line

data Command = Config Spec | RunReport Bool Int

main :: IO ()
main = do
    cmd <- O.execParser (O.info (parser O.<**> O.helper) (O.fullDesc <> O.progDesc "A read-only report of what this network can do" <> O.header "salmon-report"))
    case cmd of
        Config spec -> LChar.putStrLn (Aeson.encode spec)
        RunReport asJson secs -> do
            input <- LByteString.getContents
            spec <- either (\e -> ioError (userError ("bad directive: " <> e))) pure (Aeson.eitherDecode input)
            c <- newCollector
            -- one read of the expected name servers, shared by the DNS setup probes
            expected <- traverse (once . expectedServers) (specDomain spec)
            -- one scan per declared nmap target, shared by its port probes
            scans <- Map.fromList <$> traverse (\t -> (\shared -> (t, shared)) <$> once (scanNmap t)) [t | Right t <- parseNmapTarget <$> specNmap spec]
            let probes = probesWith c (\d -> maybe (expectedServers d) id expected) (\t -> Map.findWithDefault (scanNmap t) t scans) spec
            fs <- crossCheck <$> runReport (secs * 1000000) c (reportOp probes)
            forM_ fs $ \f ->
                if asJson
                    then LChar.putStrLn (Aeson.encode (findingValue f))
                    else Text.putStr (renderFinding f)
            hFlush stdout
  where
    parser =
        O.hsubparser
            ( O.command "config" (O.info (Config <$> specParser) (O.progDesc "print the JSON directive for these targets"))
                <> O.command "run" (O.info (O.hsubparser (O.command "report" (O.info (RunReport <$> O.switch (O.long "json" <> O.help "one JSON object per finding") <*> O.option O.auto (O.long "timeout" <> O.value 10 <> O.showDefault <> O.help "seconds allowed per probe")) (O.progDesc "read a directive on stdin and print the report")))) (O.progDesc "run a directive"))
            )
    specParser =
        Spec
            <$> many' (O.strOption (O.long "name" <> O.metavar "DNSNAME" <> O.help "resolve this name (repeatable)"))
            <*> many' (O.strOption (O.long "tcp" <> O.metavar "HOST:PORT" <> O.help "try an outbound TCP connection (repeatable)"))
            <*> O.optional (O.strOption (O.long "echo" <> O.metavar "URL" <> O.help "an HTTP URL that answers with the caller's address; no default is assumed"))
            <*> O.optional (O.strOption (O.long "gateway" <> O.metavar "ADDR" <> O.help "gateway address for NAT-PMP"))
            <*> (not <$> O.switch (O.long "no-upnp" <> O.help "skip UPnP discovery"))
            <*> (not <$> O.switch (O.long "no-natpmp" <> O.help "skip NAT-PMP discovery"))
            <*> O.optional domainParser
            <*> many' (O.strOption (O.long "nmap" <> O.metavar "HOST:PORTS" <> O.help "scan these TCP ports of this one host with nmap, as in example.org:22,443 or [2001:db8::1]:8000-8010 (repeatable); nothing is scanned unless declared"))
    domainParser =
        DomainSpec
            <$> O.strOption (O.long "domain" <> O.metavar "DOMAIN" <> O.help "report on this domain's DNS setup: its delegation, and the servers it should be delegated to")
            <*> many' (O.strOption (O.long "ns" <> O.metavar "SERVER" <> O.help "a name server the domain should be delegated to (repeatable)"))
            <*> O.optional (O.strOption (O.long "zone" <> O.metavar "PROJECT/ZONE" <> O.help "a Cloud DNS zone to read the expected name servers from with gcloud, when no --ns is given"))
            <*> many' (O.strOption (O.long "record" <> O.metavar "[TYPE:]NAME[=VALUE,...]" <> O.help "a name of the domain to compare between the zone's servers and a resolver; TYPE defaults to A (repeatable)"))
            <*> O.optional (O.strOption (O.long "resolver" <> O.metavar "ADDR" <> O.help "the resolver to ask what the world sees; the system's when absent"))
    many' = O.many

