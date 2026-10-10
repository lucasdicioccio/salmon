{-# LANGUAGE OverloadedStrings #-}

-- | Layer 0 coverage for @salmon-report@: parsers over captured output, and the
-- driver against fake probes. No network.
module Test.ReportSpec (tests) where

import Control.Concurrent (threadDelay)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified Data.Aeson as Aeson
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Report
import Report.Dns
import Report.Nmap

-- nmap -sT -Pn -n -oG - -p PORTS HOST, captured on 2026-10-10 from nmap
-- 7.94SVN scanning the loopback address of the capturing machine, where one
-- throwaway listener held port 28471. Only the dates are replaced.

-- | Two declared ports: one listening, one not.
nmapOpenClosed :: Text.Text
nmapOpenClosed =
    Text.unlines
        [ "# Nmap 7.94SVN scan initiated Thu Jan  1 00:00:00 1970 as: nmap -sT -Pn -n -oG - -p 28471,28472 127.0.0.1"
        , "Host: 127.0.0.1 ()\tStatus: Up"
        , "Host: 127.0.0.1 ()\tPorts: 28471/open/tcp/////, 28472/closed/tcp/////"
        , "# Nmap done at Thu Jan  1 00:00:00 1970 -- 1 IP address (1 host up) scanned in 0.03 seconds"
        ]

-- | A range of 101 ports: the hundred closed ones are counted, not listed.
nmapRange :: Text.Text
nmapRange =
    Text.unlines
        [ "# Nmap 7.94SVN scan initiated Thu Jan  1 00:00:00 1970 as: nmap -sT -Pn -n -oG - -p 28400-28500 127.0.0.1"
        , "Host: 127.0.0.1 ()\tStatus: Up"
        , "Host: 127.0.0.1 ()\tPorts: 28471/open/tcp/////\tIgnored State: closed (100)"
        , "# Nmap done at Thu Jan  1 00:00:00 1970 -- 1 IP address (1 host up) scanned in 0.03 seconds"
        ]

-- | The IPv6 loopback, where nothing listened.
nmapV6 :: Text.Text
nmapV6 =
    Text.unlines
        [ "# Nmap 7.94SVN scan initiated Thu Jan  1 00:00:00 1970 as: nmap -6 -sT -Pn -n -oG - -p 28471 ::1"
        , "Host: ::1 ()\tStatus: Up"
        , "Host: ::1 ()\tPorts: 28471/closed/tcp/////"
        , "# Nmap done at Thu Jan  1 00:00:00 1970 -- 1 IP address (1 host up) scanned in 0.03 seconds"
        ]

-- | A name that does not resolve: no @Host:@ line at all, and exit code 0.
nmapUnresolved :: Text.Text
nmapUnresolved =
    Text.unlines
        [ "# Nmap 7.94SVN scan initiated Thu Jan  1 00:00:00 1970 as: nmap -sT -Pn -n -oG - -p 80 nosuchhost.invalid"
        , "Failed to resolve \"nosuchhost.invalid\"."
        , "WARNING: No targets were specified, so 0 hosts scanned."
        , "# Nmap done at Thu Jan  1 00:00:00 1970 -- 0 IP addresses (0 hosts up) scanned in 0.04 seconds"
        ]

{- | NOT captured: a filtered port cannot be produced on loopback without a
firewall rule. Written by hand in the shape of the captures above, with a
service name in the fifth field as nmap prints for well-known ports.
-}
nmapFiltered :: Text.Text
nmapFiltered =
    Text.unlines
        [ "Host: 192.0.2.7 ()\tStatus: Up"
        , "Host: 192.0.2.7 ()\tPorts: 22/filtered/tcp//ssh///, 443/open/tcp//https///"
        ]

-- dig +norecurse +noall +comments +answer +authority, in the shapes captured
-- from real servers on 2026-10-02 (names and addresses replaced).

-- | A TLD server's referral: the delegation sits in the authority section, without @aa@.
referral :: Text.Text
referral =
    Text.unlines
        [ ";; Got answer:"
        , ";; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 42919"
        , ";; flags: qr; QUERY: 1, ANSWER: 0, AUTHORITY: 2, ADDITIONAL: 13"
        , ""
        , ";; OPT PSEUDOSECTION:"
        , "; EDNS: version: 0, flags:; udp: 4096"
        , ";; AUTHORITY SECTION:"
        , "example.com.\t\t172800\tIN\tNS\tns1.parking.example."
        , "example.com.\t\t172800\tIN\tNS\tNS2.parking.example."
        , ""
        ]

-- | A TLD server about a name it does not know.
nxdomain :: Text.Text
nxdomain =
    Text.unlines
        [ ";; Got answer:"
        , ";; ->>HEADER<<- opcode: QUERY, status: NXDOMAIN, id: 55928"
        , ";; flags: qr aa; QUERY: 1, ANSWER: 0, AUTHORITY: 1, ADDITIONAL: 1"
        , ""
        , ";; AUTHORITY SECTION:"
        , "com.\t\t\t900\tIN\tSOA\ta.gtld-servers.net. nstld.verisign-grs.com. 1790971634 1800 900 604800 900"
        ]

-- | A server holding the zone, asked for its NS set.
zoneNs :: Text.Text
zoneNs =
    Text.unlines
        [ ";; Got answer:"
        , ";; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 21219"
        , ";; flags: qr aa; QUERY: 1, ANSWER: 2, AUTHORITY: 0, ADDITIONAL: 1"
        , ""
        , ";; OPT PSEUDOSECTION:"
        , "; EDNS: version: 0, flags:; udp: 512"
        , "; COOKIE: 9523dfaf0a7ff2850169727a2ff9da28877f249eb8a7701e7a1edc161a (good)"
        , ";; ANSWER SECTION:"
        , "example.com.\t\t21600\tIN\tNS\tns-b.zone.example."
        , "example.com.\t\t21600\tIN\tNS\tns-a.zone.example."
        ]

zoneSoa :: Text.Text -> Text.Text
zoneSoa serial =
    Text.unlines
        [ ";; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 2060"
        , ";; flags: qr aa; QUERY: 1, ANSWER: 1, AUTHORITY: 0, ADDITIONAL: 1"
        , ";; ANSWER SECTION:"
        , "example.com.\t\t1800\tIN\tSOA\tns-a.zone.example. admin.example.com. " <> serial <> " 10000 2400 604800 1800"
        ]

-- | A server that does not hold the zone.
refused :: Text.Text
refused =
    Text.unlines
        [ ";; Got answer:"
        , ";; ->>HEADER<<- opcode: QUERY, status: REFUSED, id: 7"
        , ";; flags: qr; QUERY: 1, ANSWER: 0, AUTHORITY: 0, ADDITIONAL: 1"
        ]

-- | Nothing at that address.
silent :: Text.Text
silent = ";; communications error to 192.0.2.1#53: timed out\n;; no servers could be reached\n"

answerA :: [Text.Text] -> Text.Text
answerA addrs =
    Text.unlines $
        [ ";; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 55850"
        , ";; flags: qr rd ra; QUERY: 1, ANSWER: 2, AUTHORITY: 0, ADDITIONAL: 1"
        , ";; ANSWER SECTION:"
        ]
            <> ["www.example.com.\t300\tIN\tA\t" <> a | a <- addrs]

expectedNs :: [Text.Text]
expectedNs = ["ns-a.zone.example.", "NS-B.zone.example"]

view :: Text.Text -> Text.Text -> ServerView
view ns soa = serverView "example.com" (Right (parseDig ns)) (Right (parseDig soa))

fake :: Verdict -> Finding
fake v = Finding "q" v ["e"] "fake" Nothing []

tests :: TestTree
tests =
    testGroup
        "salmon-report"
        [ testCase "dig keeps addresses and drops CNAME targets" $
            assertEqual "" ["192.0.2.1", "2001:db8::1"] (parseDigAddresses "alias.example.org.\n192.0.2.1\n2001:db8::1\n")
        , testCase "natpmpc public address" $
            assertEqual "" (NatpmpPublic "203.0.113.4") (parseNatpmpc "initnatpmp() returned 0 (SUCCESS)\nPublic IP address : 203.0.113.4\n")
        , testCase "natpmpc without gateway" $
            assertEqual "" NatpmpNoGateway (parseNatpmpc "Cannot get default gateway ip address")
        , testCase "host:port" $ do
            assertEqual "" (Just ("example.org", 443)) (parseHostPort "example.org:443")
            assertEqual "" (Just ("::1", 80)) (parseHostPort "[::1]:80")
            assertEqual "" Nothing (parseHostPort "example.org")
        , testCase "driver: success, failure, throw and timeout each print one finding" $ do
            c <- newCollector
            let ok = probeOp c "ok" (pure (fake Yes))
                no = probeOp c "no" (pure (fake No))
                boom = probeOp c "boom" (ioError (userError "bang"))
                slow = probeOp c "slow" (threadDelay 5000000 >> pure (fake Yes))
            fs <- runReport 200000 c (reportOp [ok, no, boom, slow])
            assertEqual "verdicts" [Yes, No, Unknown, Unknown] (fVerdict <$> fs)
            assertEqual "timeout cause" True (any (any (Text.isPrefixOf "timed out") . fEvidence) fs)
        , testCase "a probe's up is never called by the report and throws if it were" $ do
            c <- newCollector
            fs <- runReport 200000 c (reportOp [probeOp c "ok" (pure (fake Yes))])
            assertEqual "" 1 (length fs)
        , testCase "cross-check marks a name pointing at the external address" $ do
            let ext = Finding "What is the external address, and can it be mapped to?" Yes [] "m" Nothing ["203.0.113.4"]
                dns = Finding "Does a.example resolve?" Yes ["x"] "m" Nothing ["203.0.113.4"]
            assertEqual "" ["x", "points at the external address 203.0.113.4"] (fEvidence (crossCheck [ext, dns] !! 1))
        , testGroup
            "dns setup"
            [ testCase "dig: status, the aa flag and the records of a referral" $ do
                let a = parseDig referral
                assertEqual "status" (Just "NOERROR") (digStatus a)
                assertEqual "aa" False (digAuthoritative a)
                assertEqual "names" ["ns1.parking.example", "ns2.parking.example"] (nsNames "Example.com." a)
                assertEqual "aa on a zone's own answer" True (digAuthoritative (parseDig zoneNs))
            , testCase "dig: no header means nothing answered" $
                assertEqual "" (DigAnswer Nothing False []) (parseDig silent)
            , testCase "dig +short NS keeps names only" $
                assertEqual "" ["a.gtld-servers.net", "b.gtld-servers.net"] (parseDigNames "a.gtld-servers.net.\nB.gtld-servers.net.\n;; communications error\n")
            , testCase "parent candidates stop before the root" $ do
                assertEqual "" ["example.com", "com"] (parentCandidates "www.example.com.")
                assertEqual "" [] (parentCandidates "com")
            , testCase "delegation: the registrar's parking servers are not delegated" $ do
                assertEqual "" (NotDelegated ["ns1.parking.example", "ns2.parking.example"]) (classifyDelegation "example.com" expectedNs (parseDig referral))
                assertEqual "verdict" (Just False) (jOk (judgeDelegation "example.com" (Right expectedNs) (parseDig referral)))
            , testCase "delegation: differing sets are partly delegated, naming both sides" $
                assertEqual
                    ""
                    (PartlyDelegated ["ns-c.zone.example"] ["ns2.parking.example"])
                    (classifyDelegation "example.com" ["ns1.parking.example", "ns-c.zone.example"] (parseDig referral))
            , testCase "delegation: equal sets, whatever the order, case and dots" $ do
                assertEqual "" (Delegated ["ns-a.zone.example", "ns-b.zone.example"]) (classifyDelegation "example.com" expectedNs (parseDig zoneNs))
                assertEqual "verdict" (Just True) (jOk (judgeDelegation "example.com" (Right expectedNs) (parseDig zoneNs)))
            , testCase "delegation: NXDOMAIN and an empty answer are told apart" $ do
                assertEqual "" Unregistered (classifyDelegation "example.com" expectedNs (parseDig nxdomain))
                assertEqual "" NoDelegation (classifyDelegation "example.com" expectedNs (parseDig refused))
            , testCase "delegation: without an expectation the handed-out set is evidence, not a verdict" $ do
                let j = judgeDelegation "example.com" (Left "no expected name servers") (parseDig referral)
                assertEqual "" Nothing (jOk j)
                assertEqual "" ["the parent hands out: ns1.parking.example, ns2.parking.example", "no expected name servers"] (jEvidence j)
            , testCase "zone: servers that hold it and agree" $ do
                let v = view zoneNs (zoneSoa "7")
                assertEqual "" (Serving ["ns-a.zone.example", "ns-b.zone.example"] (Just ("ns-a.zone.example", "7"))) v
                assertEqual "" (Just True) (jOk (judgeZone expectedNs [("ns-a.zone.example", v), ("ns-b.zone.example", v)]))
            , testCase "zone: a recreated zone shows as an expected server that does not hold it" $ do
                let j = judgeZone expectedNs [("ns-a.zone.example", view refused refused), ("ns-b.zone.example", view zoneNs (zoneSoa "7"))]
                assertEqual "" (Just False) (jOk j)
                assertEqual "" True (any (Text.isInfixOf "ns-a.zone.example: it answers REFUSED") (jEvidence j))
            , testCase "zone: an answer without authority is not the zone speaking" $
                assertEqual "" (NotServing "it answers without authority for the domain") (view referral referral)
            , testCase "zone: an NS set other than the expected one, and differing SOA serials" $ do
                let v n = view zoneNs (zoneSoa n)
                assertEqual "stale expectation" (Just False) (jOk (judgeZone ["ns-a.zone.example", "ns-z.zone.example"] [("ns-a.zone.example", v "7")]))
                assertEqual "serials" (Just False) (jOk (judgeZone expectedNs [("ns-a.zone.example", v "7"), ("ns-b.zone.example", v "8")]))
            , testCase "zone: a silent server leaves the question open" $
                assertEqual "" Nothing (jOk (judgeZone expectedNs [("ns-a.zone.example", view silent silent), ("ns-b.zone.example", view zoneNs (zoneSoa "7"))]))
            , testCase "record queries" $ do
                assertEqual "" (Right (RecordQuery "A" "www.example.com" [])) (parseRecordQuery "www.Example.com.")
                assertEqual "" (Right (RecordQuery "TXT" "example.com" ["v=spf1 -all"])) (parseRecordQuery "txt:example.com=v=spf1 -all")
                assertEqual "" (Right (RecordQuery "A" "www.example.com" ["192.0.2.1", "192.0.2.2"])) (parseRecordQuery "A:www.example.com=192.0.2.2,192.0.2.1")
                assertEqual "" True (either (const True) (const False) (parseRecordQuery "A:"))
            , testCase "records: the zone and the resolver agree" $ do
                let q = RecordQuery "A" "www.example.com" []
                    (zs, rs) = answerValues q (parseDig (answerA ["192.0.2.2", "192.0.2.1"])) (parseDig (answerA ["192.0.2.1", "192.0.2.2"]))
                assertEqual "" (Just True) (jOk (judgeRecord q (Just zs) (Just rs)))
            , testCase "records: an alias out of the zone is compared as an alias" $ do
                let q = RecordQuery "A" "www.example.com" []
                    hdr = ";; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 1\n"
                    cname = "www.example.com.\t300\tIN\tCNAME\tlb.example.net.\n"
                    zone = parseDig (hdr <> cname)
                    resolver = parseDig (hdr <> cname <> "lb.example.net.\t60\tIN\tA\t192.0.2.9\n")
                assertEqual "" (["lb.example.net"], ["lb.example.net"]) (answerValues q zone resolver)
            , testCase "records: not propagated is told from a wrong record" $ do
                let q = RecordQuery "A" "www.example.com" ["192.0.2.1"]
                    meaning z r = maybe "" (Text.takeWhile (/= ':')) (jMeaning (judgeRecord q z r))
                assertEqual "resolver behind" "Not propagated" (meaning (Just ["192.0.2.1"]) (Just ["198.51.100.7"]))
                assertEqual "resolver empty" "Not propagated" (meaning (Just ["192.0.2.1"]) (Just []))
                assertEqual "zone differs from the declaration" "Wrong record" (meaning (Just ["192.0.2.66"]) (Just ["192.0.2.66"]))
                assertEqual "zone holds nothing" "Wrong record" (meaning (Just []) (Just []))
                assertEqual "zone unreachable" Nothing (jOk (judgeRecord q Nothing (Just ["192.0.2.1"])))
            , testCase "expected servers: declared ones win, and no source is a reason" $ do
                let d = DomainSpec "example.com" [] Nothing [] Nothing
                a <- expectedServers d{domNameServers = ["NS-A.zone.example."]}
                assertEqual "" (Right ["ns-a.zone.example"]) a
                b <- expectedServers d
                assertEqual "" True (either (Text.isPrefixOf "no expected name servers") (const False) b)
                assertEqual "" (Just ("acme-project", "example-zone")) (parseZoneRef "acme-project/example-zone")
                assertEqual "" Nothing (parseZoneRef "example-zone")
            , testCase "a declared domain adds two probes and one per record, with the expectation supplied" $ do
                c <- newCollector
                let d = DomainSpec "example.com" [] Nothing ["not a record="] Nothing
                    ps = dnsSetupProbes c (pure (Left "none")) d
                assertEqual "" 3 (length ps)
            ]
        , testGroup
            "nmap"
            [ testCase "a target is one host and a bounded port list" $ do
                assertEqual "" (Right (NmapTarget "example.org" [22, 443])) (parseNmapTarget "example.org:443,22,443")
                assertEqual "" (Right (NmapTarget "192.0.2.7" [8000, 8001, 8002])) (parseNmapTarget "192.0.2.7:8000-8002")
                assertEqual "" (Right (NmapTarget "2001:db8::1" [80])) (parseNmapTarget "[2001:db8::1]:80")
                assertEqual "the cap" (Right 128) (length . targetPorts <$> parseNmapTarget "example.org:1-128")
            , testCase "what nmap would expand into several hosts, or read as an option, is refused" $ do
                let mustRefuse t = assertEqual (Text.unpack t) True (isLeft (parseNmapTarget t))
                mapM_
                    mustRefuse
                    [ "192.0.2.0/24:22"
                    , "192.0.2.1-9:22"
                    , "192.0.2.*:22"
                    , "192.0.2.1,192.0.2.2:22"
                    , "192.0.2:22"
                    , "-iL:22"
                    , "--script=x:22"
                    , "a b:22"
                    , "example.org"
                    , ":22"
                    , "example.org:"
                    , "example.org:0"
                    , "example.org:65536"
                    , "example.org:22-"
                    , "example.org:90-80"
                    , "example.org:1-129"
                    , "example.org:1-65535"
                    , "example.org:T:22"
                    , "example.org:-p-"
                    ]
            , testCase "the argument vector: a connect scan of the declared ports, nothing else" $ do
                assertEqual "" (Right ["-sT", "-Pn", "-n", "-oG", "-", "-p", "22,80-82,443", "example.org"]) (nmapArgs <$> parseNmapTarget "example.org:80,81,82,22,443")
                assertEqual "" (Right ["-6", "-sT", "-Pn", "-n", "-oG", "-", "-p", "80", "::1"]) (nmapArgs <$> parseNmapTarget "[::1]:80")
            , testCase "captured: an open and a closed port" $
                assertEqual "" (Just (NmapScan "127.0.0.1" (Map.fromList [(28471, PortOpen), (28472, PortClosed)]) [])) (parseNmapGrepable nmapOpenClosed)
            , testCase "captured: unlisted ports take the one ignored state" $ do
                let scan = parseNmapGrepable nmapRange
                assertEqual "" (Just (NmapScan "127.0.0.1" (Map.fromList [(28471, PortOpen)]) [(PortClosed, 100)])) scan
                assertEqual "listed" (Just (Right PortOpen)) ((`portState` 28471) <$> scan)
                assertEqual "unlisted" (Just (Right PortClosed)) ((`portState` 28400) <$> scan)
            , testCase "captured: IPv6" $
                assertEqual "" (Just (NmapScan "::1" (Map.fromList [(28471, PortClosed)]) [])) (parseNmapGrepable nmapV6)
            , testCase "captured: a name that does not resolve prints no host" $
                assertEqual "" Nothing (parseNmapGrepable nmapUnresolved)
            , testCase "a port nmap says nothing about is not guessed" $ do
                let two = NmapScan "192.0.2.7" Map.empty [(PortClosed, 30), (PortFiltered, 40)]
                assertEqual "" True (isLeft (portState two 22))
                assertEqual "" True (isLeft (portState (NmapScan "192.0.2.7" Map.empty []) 22))
            , testCase "findings: open is yes, closed is no, filtered is unknown with its meaning" $ do
                let t = NmapTarget "example.org" [22, 443, 8080]
                    scan = maybe (Left "no host") Right (parseNmapGrepable nmapFiltered)
                    f p = nmapFinding t p scan
                assertEqual "" [Unknown, Yes, Unknown] (fVerdict . f <$> [22, 443, 8080])
                assertEqual "" ["22/tcp is filtered on 192.0.2.7"] (fEvidence (f 22))
                assertEqual "" (Just "Filtered") (Text.takeWhile (/= ':') <$> fMeaning (f 22))
                assertEqual "" "Is TCP port 443 of example.org open, seen from this host?" (fQuestion (f 443))
                assertEqual "" "nmap -sT -Pn -n -oG - -p 22,443,8080 example.org" (fMethod (f 443))
                let closed = nmapFinding (NmapTarget "127.0.0.1" [28472]) 28472 (maybe (Left "no host") Right (parseNmapGrepable nmapOpenClosed))
                assertEqual "" (No, ["28472/tcp is closed on 127.0.0.1"]) (fVerdict closed, fEvidence closed)
                assertEqual "a failed scan" (Unknown, ["nmap could not run"]) ((\x -> (fVerdict x, fEvidence x)) (nmapFinding t 22 (Left "nmap could not run")))
            , testCase "one probe per declared port; a bad declaration is one probe and no scan" $ do
                c <- newCollector
                calls <- newIORef (0 :: Int)
                let scan _ = do
                        atomicModifyIORef' calls (\n -> (n + 1, ()))
                        pure (maybe (Left "no host") Right (parseNmapGrepable nmapOpenClosed))
                    good = nmapProbes c scan "127.0.0.1:28471-28472"
                    bad = nmapProbes c scan "192.0.2.0/24:22"
                assertEqual "" (2, 1) (length good, length bad)
                fs <- runReport 2000000 c (reportOp (good <> bad))
                assertEqual "" [Yes, No, Unknown] (fVerdict <$> fs)
                assertEqual "the bad declaration says why" True (any (Text.isInfixOf "HOST:PORTS") (concatMap fEvidence fs))
                n <- readIORef calls
                assertEqual "only the good target's probes scanned" 2 n
            , testCase "nothing is scanned unless declared, and an older directive still reads" $ do
                c <- newCollector
                let old = "{\"specNames\":[],\"specTcp\":[],\"specEcho\":null,\"specGateway\":null,\"specUpnp\":false,\"specNatpmp\":false,\"specDomain\":null}"
                    spec = Aeson.eitherDecode old :: Either String Spec
                assertEqual "" (Right []) (specNmap <$> spec)
                assertEqual "round trip" (Right ["example.org:22"]) (specNmap <$> (Aeson.eitherDecode . Aeson.encode . (\s -> s{specNmap = ["example.org:22"]}) =<< spec))
                -- the IPv6 probe is the only one a bare spec has
                assertEqual "" (Right 1) (length . probesWith c expectedServers (\_ -> ioError (userError "scanned")) <$> spec)
            ]
        ]
