{-# LANGUAGE OverloadedStrings #-}

-- | Layer 0 coverage for @salmon-report@: parsers over captured output, and the
-- driver against fake probes. No network, except the STUN exchange, which is
-- run against stand-in responders on loopback addresses.
module Test.ReportSpec (tests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified Data.Aeson as Aeson
import Data.Bits (shiftR, xor)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Word (Word8)
import qualified Network.Socket as Net
import Network.Socket.ByteString (recvFrom, sendAllTo)
import Numeric (readHex)

import Report
import Report.Dns
import Report.Nmap
import Report.Stun

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

-- STUN. The two responses are the test vectors of RFC 5769 (sections 2.2 and
-- 2.3), byte for byte: their FINGERPRINT and MESSAGE-INTEGRITY values were
-- recomputed when they were typed in, though the parser verifies neither. No
-- fixture was captured from a real server: none was contacted.

hex :: String -> BS.ByteString
hex s = BS.pack [b | w <- words s, [(b, "")] <- [readHex w]]

vectorTidBytes :: BS.ByteString
vectorTidBytes = hex "b7 e7 a7 01 bc 34 d6 86 fa 87 df ae"

vectorTid :: TransactionId
vectorTid = maybe (error "twelve bytes") id (mkTransactionId vectorTidBytes)

rfc5769v4 :: BS.ByteString
rfc5769v4 =
    hex $
        unwords
            [ "01 01 00 3c 21 12 a4 42 b7 e7 a7 01 bc 34 d6 86 fa 87 df ae"
            , "80 22 00 0b 74 65 73 74 20 76 65 63 74 6f 72 20"
            , "00 20 00 08 00 01 a1 47 e1 12 a6 43"
            , "00 08 00 14 2b 91 f5 99 fd 9e 90 c3 8c 74 89 f9 2a f9 ba 53 f0 6b e7 d7"
            , "80 28 00 04 c0 7d 4c 96"
            ]

rfc5769v6 :: BS.ByteString
rfc5769v6 =
    hex $
        unwords
            [ "01 01 00 48 21 12 a4 42 b7 e7 a7 01 bc 34 d6 86 fa 87 df ae"
            , "80 22 00 0b 74 65 73 74 20 76 65 63 74 6f 72 20"
            , "00 20 00 14 00 02 a1 47 01 13 a9 fa a5 d3 f1 79 bc 25 f4 b5 be d2 b9 d9"
            , "00 08 00 14 a3 82 95 4e 4b e6 7b f1 17 84 c9 7c 82 92 c2 75 bf e3 ed 41"
            , "80 28 00 04 c8 fb 0b 4c"
            ]

be16 :: Int -> BS.ByteString
be16 n = BS.pack [fromIntegral (n `shiftR` 8), fromIntegral n]

-- | A STUN message of this type and transaction, its attributes padded to four bytes.
stunMessage :: Int -> BS.ByteString -> [(Int, BS.ByteString)] -> BS.ByteString
stunMessage ty tid attrs = be16 ty <> be16 (BS.length body) <> hex "21 12 a4 42" <> tid <> body
  where
    body = BS.concat [be16 t <> be16 (BS.length v) <> v <> BS.replicate ((4 - BS.length v `mod` 4) `mod` 4) 0 | (t, v) <- attrs]

-- | The value of an XOR-MAPPED-ADDRESS naming this IPv4 address and port.
xorMapped :: (Word8, Word8, Word8, Word8) -> Int -> BS.ByteString
xorMapped (a, b, c, d) port = hex "00 01" <> be16 (port `xor` 0x2112) <> BS.pack (zipWith xor [a, b, c, d] [0x21, 0x12, 0xa4, 0x42])

{- | A stand-in STUN responder on a loopback address (@127.0.0.N@, any free
port), alive for the action, which is handed its port. For each datagram it
is given the count of those before it, the sender and the bytes, and sends
back what it answers, if anything.
-}
withResponder :: Word8 -> (Int -> Net.SockAddr -> BS.ByteString -> Maybe BS.ByteString) -> (Int -> IO a) -> IO a
withResponder n answer = withResponderIO n (\sock k from bytes -> mapM_ (\b -> sendAllTo sock b from) (answer k from bytes))

-- | 'withResponder', the responder doing what it likes with its socket and each datagram.
withResponderIO :: Word8 -> (Net.Socket -> Int -> Net.SockAddr -> BS.ByteString -> IO ()) -> (Int -> IO a) -> IO a
withResponderIO n react act =
    bracket open Net.close $ \sock -> do
        port <- fromIntegral <$> Net.socketPort sock
        bracket (forkIO (loop sock 0)) killThread (\_ -> act port)
  where
    open = do
        s <- Net.socket Net.AF_INET Net.Datagram Net.defaultProtocol
        -- the suite spawns children elsewhere: they do not inherit this socket
        Net.withFdSocket s Net.setCloseOnExecIfNeeded
        Net.bind s (Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, n)))
        pure s
    loop sock k = do
        (bytes, from) <- recvFrom sock 2048
        react sock (k :: Int) from bytes
        loop sock (k + 1)

-- | Answers a Binding request with the address and port it came from.
honest :: Int -> Net.SockAddr -> BS.ByteString -> Maybe BS.ByteString
honest _ from req = case (parseStunMessage req, from) of
    (Right (StunMessage 0x0001 _ []), Net.SockAddrInet p h) ->
        Just (stunMessage 0x0101 (BS.take 12 (BS.drop 8 req)) [(0x0020, xorMapped (Net.hostAddressToTuple h) (fromIntegral p))])
    _ -> Nothing

-- | Answers a Binding request with this mapped address, as a server beyond a NAT would see one.
claiming :: (Word8, Word8, Word8, Word8) -> Int -> Int -> Net.SockAddr -> BS.ByteString -> Maybe BS.ByteString
claiming addr port _ _ req = case parseStunMessage req of
    Right (StunMessage 0x0001 _ []) -> Just (stunMessage 0x0101 (BS.take 12 (BS.drop 8 req)) [(0x0020, xorMapped addr port)])
    _ -> Nothing

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
                assertEqual "" (Right 1) (length . probesWith c expectedServers (\_ -> ioError (userError "scanned")) (\_ -> ioError (userError "asked")) <$> spec)
            ]
        , testGroup
            "stun"
            [ testCase "a server is one host and one port, and there is no default port" $ do
                assertEqual "" (Right (StunServer "stun.example.org" 3478)) (parseStunServer " stun.example.org:3478 ")
                assertEqual "" (Right (StunServer "192.0.2.7" 19302)) (parseStunServer "192.0.2.7:19302")
                assertEqual "" (Right (StunServer "2001:db8::1" 3478)) (parseStunServer "[2001:db8::1]:3478")
                assertEqual "" (Right "[2001:db8::1]:3478") (renderStunServer <$> parseStunServer "[2001:db8::1]:3478")
                let mustRefuse t = assertEqual (Text.unpack t) True (isLeft (parseStunServer t))
                mapM_ mustRefuse ["stun.example.org", "stun.example.org:", ":3478", "stun.example.org:0", "stun.example.org:65536", "stun.example.org:stun", "a b:3478", "192.0.2.0/24:3478", "[2001:db8::g]:3478"]
            , testCase "the request is the twenty bytes of a Binding header, and nothing else" $ do
                assertEqual "" (hex "00 01 00 00 21 12 a4 42 b7 e7 a7 01 bc 34 d6 86 fa 87 df ae") (bindingRequest vectorTid)
                assertEqual "it reads back" (Right (StunMessage 0x0001 vectorTid [])) (parseStunMessage (bindingRequest vectorTid))
                assertEqual "twelve bytes or nothing" Nothing (mkTransactionId (BS.replicate 11 0))
            , testCase "RFC 5769: the sample IPv4 and IPv6 responses" $ do
                assertEqual "" (Right (Mapped (Endpoint V4 "192.0.2.1" 32853) Nothing)) (parseBindingResponse vectorTid rfc5769v4)
                assertEqual "" (Right (Mapped (Endpoint V6 "2001:db8:1234:5678:11:2233:4455:6677" 32853) Nothing)) (parseBindingResponse vectorTid rfc5769v6)
                assertEqual "attributes are split with their padding removed" (Right [0x8022, 0x0020, 0x0008, 0x8028]) (fmap fst . msgAttributes <$> parseStunMessage rfc5769v4)
                assertEqual "the SOFTWARE value" (Right (Just "test vector")) (lookup 0x8022 . msgAttributes <$> parseStunMessage rfc5769v4)
            , testCase "an older server's plain MAPPED-ADDRESS, and the second address a server advertises" $ do
                let plain = stunMessage 0x0101 vectorTidBytes [(0x0001, hex "00 01 9c 41 c6 33 64 07"), (0x802c, hex "00 01 0d 97 c6 33 64 08")]
                assertEqual "" (Right (Mapped (Endpoint V4 "198.51.100.7" 40001) (Just (Endpoint V4 "198.51.100.8" 3479)))) (parseBindingResponse vectorTid plain)
                let both = stunMessage 0x0101 vectorTidBytes [(0x0001, hex "00 01 00 01 00 00 00 00"), (0x0020, xorMapped (203, 0, 113, 4) 40001)]
                assertEqual "the XORed one wins" (Right (Mapped (Endpoint V4 "203.0.113.4" 40001) Nothing)) (parseBindingResponse vectorTid both)
            , testCase "an error response is an answer, with its code and reason" $ do
                let refusal = stunMessage 0x0111 vectorTidBytes [(0x0009, hex "00 00 04 01" <> "Unauthorized")]
                assertEqual "" (Right (Refused 401 "Unauthorized")) (parseBindingResponse vectorTid refusal)
            , testCase "what is not the answer to this request is a reason, never an address" $ do
                let refuses name b = assertEqual name True (isLeft (parseBindingResponse vectorTid b))
                refuses "short" (BS.take 19 rfc5769v4)
                refuses "the datagram is longer than the message" (rfc5769v4 <> "\0\0\0\0")
                refuses "the datagram is shorter than the message" (BS.take 56 rfc5769v4)
                refuses "no magic cookie" (BS.take 4 rfc5769v4 <> hex "00 00 00 00" <> BS.drop 8 rfc5769v4)
                refuses "the first bits" (BS.cons 0x81 (BS.drop 1 rfc5769v4))
                refuses "another transaction" (stunMessage 0x0101 (BS.replicate 12 7) [(0x0020, xorMapped (203, 0, 113, 4) 40001)])
                refuses "a request" (bindingRequest vectorTid)
                refuses "no mapped address" (stunMessage 0x0101 vectorTidBytes [(0x8022, "x")])
                refuses "an attribute running past the end" (hex "01 01 00 08" <> hex "21 12 a4 42" <> vectorTidBytes <> hex "00 20 00 08 00 01 a1 47")
                refuses "an address of an unknown family" (stunMessage 0x0101 vectorTidBytes [(0x0020, hex "00 03 a1 47 e1 12 a6 43")])
            , testCase "IPv6 addresses are written with their longest zero run folded" $ do
                let host gs = epHost (endpointV6 gs 1)
                assertEqual "" "::1" (host [0, 0, 0, 0, 0, 0, 0, 1])
                assertEqual "" "::" (host [0, 0, 0, 0, 0, 0, 0, 0])
                assertEqual "" "2001:db8::1:0:0:1" (host [0x2001, 0xdb8, 0, 0, 1, 0, 0, 1])
                assertEqual "" "2001:db8:0:1::" (host [0x2001, 0xdb8, 0, 1, 0, 0, 0, 0])
                assertEqual "" "[2001:db8::1]:3478" (renderEndpoint (endpointV6 [0x2001, 0xdb8, 0, 0, 0, 0, 0, 1] 3478))
            , testCase "mapping: differing answers settle it, equal ones only across distinct addresses" $ do
                let srvA = Endpoint V4 "198.51.100.2" 3478
                    srvA' = Endpoint V4 "198.51.100.2" 3479
                    srvB = Endpoint V4 "198.51.100.3" 3478
                    local = Just (Endpoint V4 "192.168.1.10" 51000)
                    ext p = Endpoint V4 "203.0.113.4" p
                assertEqual "" EndpointIndependent (classifyMapping [(srvA, local, ext 40001), (srvB, local, ext 40001)])
                assertEqual "" DestinationDependent (classifyMapping [(srvA, local, ext 40001), (srvB, local, ext 40002)])
                assertEqual "a port is a destination too" DestinationDependent (classifyMapping [(srvA, local, ext 40001), (srvA', local, ext 40002)])
                assertEqual "" OneAddress (classifyMapping [(srvA, local, ext 40001), (srvA', local, ext 40001)])
                assertEqual "" OneDestination (classifyMapping [(srvA, local, ext 40001)])
                assertEqual "" NoTranslation (classifyMapping [(srvA, local, Endpoint V4 "192.168.1.10" 51000)])
                assertEqual "a preserved port is still a translation" OneDestination (classifyMapping [(srvA, local, ext 51000)])
                assertEqual "an unknown local address is not assumed to be the mapped one" OneDestination (classifyMapping [(srvA, Nothing, ext 40001)])
            , testCase "mapping judgement: yes, no and unknown, each saying what was seen" $ do
                let server :: Int -> StunServer
                    server n = StunServer ("stun" <> Text.pack (show n) <> ".example.org") 3478
                    local = Just (Endpoint V4 "192.168.1.10" 51000)
                    saw :: Int -> Text.Text -> Int -> Observation
                    saw n dest port = Observation (server n) (Just (Endpoint V4 dest 3478)) local (Answered (Mapped (Endpoint V4 "203.0.113.4" port) Nothing))
                    independent = judgeMapping [saw 1 "198.51.100.2" 40001, saw 2 "198.51.100.3" 40001]
                    dependent = judgeMapping [saw 1 "198.51.100.2" 40001, saw 2 "198.51.100.3" 40002]
                    lone = judgeMapping [saw 1 "198.51.100.2" 40001, Observation (server 2) (Just (Endpoint V4 "198.51.100.3" 3478)) local (Silent "no answer")]
                assertEqual "" (Just True) (jOk independent)
                assertEqual "" (Just "Endpoint-independent mapping") (Text.takeWhile (/= ':') <$> jMeaning independent)
                assertEqual "" ["198.51.100.2:3478 sees 203.0.113.4:40001 for the local 192.168.1.10:51000", "198.51.100.3:3478 sees 203.0.113.4:40001 for the local 192.168.1.10:51000"] (jEvidence independent)
                assertEqual "" (Just False) (jOk dependent)
                assertEqual "" (Nothing, Nothing) (jOk lone, jMeaning lone)
                assertEqual "the lone answer says what to declare" True (any (Text.isInfixOf "a second --stun server") (jEvidence lone))
                assertEqual "nothing answered" (Judgement Nothing ["no declared STUN server gave a mapped address"] Nothing) (judgeMapping [Observation (server 1) Nothing Nothing (NotAsked "x")])
                -- an IPv6 path without translation does not hide an IPv4 NAT that is destination-dependent
                let v6 = Endpoint V6 "2001:db8::10" 51001
                    native = Observation (server 3) (Just (Endpoint V6 "2001:db8::1" 3478)) (Just v6) (Answered (Mapped v6 Nothing))
                assertEqual "families are judged apart" (Just False) (jOk (judgeMapping [native, saw 1 "198.51.100.2" 40001, saw 2 "198.51.100.3" 40002]))
                assertEqual "nor one whose mapping could not be compared" Nothing (jOk (judgeMapping [native, saw 1 "198.51.100.2" 40001]))
                assertEqual "every family that answered agrees" (Just True) (jOk (judgeMapping [native, saw 1 "198.51.100.2" 40001, saw 2 "198.51.100.3" 40001]))
                assertEqual "one family alone" (Just True) (jOk (judgeMapping [native]))
            , testCase "findings: a mapped address is yes, silence and an error are no, not asked is unknown" $ do
                let s = StunServer "stun.example.org" 3478
                    dest = Just (Endpoint V4 "198.51.100.2" 3478)
                    local = Just (Endpoint V4 "192.168.1.10" 51000)
                    f outcome = stunFinding s [Observation s dest local outcome]
                    yes = f (Answered (Mapped (Endpoint V4 "203.0.113.4" 40001) (Just (Endpoint V4 "198.51.100.3" 3479))))
                assertEqual "" "Does the STUN server stun.example.org:3478 tell this host its mapped address?" (fQuestion yes)
                assertEqual
                    ""
                    ( Yes
                    ,
                        [ "mapped address 203.0.113.4:40001 (public)"
                        , "asked 198.51.100.2:3478 from 192.168.1.10:51000"
                        , "the mapped address is not the local one: a NAT is on the path to this server"
                        , "the server advertises a second address, 198.51.100.3:3479: it is not contacted unless declared with --stun"
                        ]
                    )
                    (fVerdict yes, fEvidence yes)
                assertEqual "carrier-grade NAT shows in the evidence" ["mapped address 100.64.3.4:40001 (carrier-grade NAT (100.64.0.0/10))"] (take 1 (fEvidence (f (Answered (Mapped (Endpoint V4 "100.64.3.4" 40001) Nothing)))))
                assertEqual "" (No, ["no answer within 3.5s (3 requests sent)", "asked 198.51.100.2:3478 from 192.168.1.10:51000"]) ((\x -> (fVerdict x, fEvidence x)) (f (Silent "no answer within 3.5s (3 requests sent)")))
                assertEqual "" (No, ["the server answered with the error 401 Unauthorized"]) ((\x -> (fVerdict x, take 1 (fEvidence x))) (f (Answered (Refused 401 "Unauthorized"))))
                assertEqual "" (Unknown, ["stun.example.org could not be resolved"]) ((\x -> (fVerdict x, fEvidence x)) (f (NotAsked "stun.example.org could not be resolved")))
                assertEqual "a server the exchange does not hold" Unknown (fVerdict (stunFinding s []))
            , testCase "one probe per declared server and one on the mapping; a bad declaration is asked nothing" $ do
                c <- newCollector
                asked <- newIORef []
                let exchange servers = do
                        atomicModifyIORef' asked (\l -> (servers : l, ()))
                        pure [Observation s (Just (Endpoint V4 "198.51.100.2" (stunPort s))) Nothing (Answered (Mapped (Endpoint V4 "203.0.113.4" 40001) Nothing)) | s <- servers]
                    ps = stunProbes c exchange ["stun.example.org:3478", "stun.example.org:3479", "stun.example.org:3478", "stun.example.org"]
                assertEqual "the repeated declaration is one probe" 4 (length ps)
                fs <- runReport 2000000 c (reportOp ps)
                assertEqual "" [Yes, Yes, Unknown, Unknown] (fVerdict <$> fs)
                assertEqual "the bad declaration says why" True (any (Text.isInfixOf "HOST:PORT") (fEvidence (fs !! 2)))
                assertEqual "one address on two ports leaves the mapping open" True (any (Text.isInfixOf "one address on several ports") (fEvidence (fs !! 3)))
                seen <- readIORef asked
                assertEqual "only the two well-formed servers were handed to the exchange" [[StunServer "stun.example.org" 3478, StunServer "stun.example.org" 3479]] (nub seen)
            , testCase "no server is asked unless declared, and an older directive still reads" $ do
                c <- newCollector
                let old = "{\"specNames\":[],\"specTcp\":[],\"specEcho\":null,\"specGateway\":null,\"specUpnp\":false,\"specNatpmp\":false,\"specDomain\":null,\"specNmap\":[]}"
                    spec = Aeson.eitherDecode old :: Either String Spec
                assertEqual "" (Right []) (specStun <$> spec)
                assertEqual "round trip" (Right ["stun.example.org:3478"]) (specStun <$> (Aeson.eitherDecode . Aeson.encode . (\s -> s{specStun = ["stun.example.org:3478"]}) =<< spec))
                assertEqual "no STUN probe in a bare spec" (Right 1) (length . probesWith c expectedServers (\_ -> ioError (userError "scanned")) (\_ -> ioError (userError "asked")) <$> spec)
                assertEqual "" 0 (length (stunProbes c (\_ -> ioError (userError "asked")) []))
                none <- scanStunWith [1] []
                assertEqual "an exchange with no server opens nothing and answers nothing" [] none
            , testGroup
                "loopback stand-in"
                [ testCase "honest responders see the socket's own address: nothing translates" $
                    withResponder 1 honest $ \p1 -> withResponder 1 honest $ \p2 -> do
                        let servers = [StunServer "127.0.0.1" p1, StunServer "127.0.0.1" p2]
                        obs <- scanStunWith [2000000] servers
                        assertEqual "asked in order" servers (obsServer <$> obs)
                        assertEqual "each saw the local address and port" [True, True] [obsOutcome o == maybe (Silent "no local address") (\l -> Answered (Mapped l Nothing)) (obsLocal o) | o <- obs]
                        assertEqual "one socket: one local port" 1 (length (nub (fmap epPort . obsLocal <$> obs)))
                        let finding = stunMappingFinding obs
                        assertEqual "" (Yes, Just "No address translation on the path") (fVerdict finding, Text.takeWhile (/= ':') <$> fMeaning finding)
                        assertEqual "" [Yes, Yes] (fVerdict . (`stunFinding` obs) <$> servers)
                , testCase "responders at two addresses reporting one mapping: endpoint-independent" $
                    withResponder 1 (claiming (203, 0, 113, 4) 40001) $ \p1 -> withResponder 2 (claiming (203, 0, 113, 4) 40001) $ \p2 -> do
                        obs <- scanStunWith [2000000] [StunServer "127.0.0.1" p1, StunServer "127.0.0.2" p2]
                        assertEqual "" [Answered (Mapped (Endpoint V4 "203.0.113.4" 40001) Nothing), Answered (Mapped (Endpoint V4 "203.0.113.4" 40001) Nothing)] (obsOutcome <$> obs)
                        assertEqual "" Yes (fVerdict (stunMappingFinding obs))
                , testCase "responders reporting two mappings: destination-dependent" $
                    withResponder 1 (claiming (203, 0, 113, 4) 40001) $ \p1 -> withResponder 2 (claiming (203, 0, 113, 4) 40002) $ \p2 -> do
                        obs <- scanStunWith [2000000] [StunServer "127.0.0.1" p1, StunServer "127.0.0.2" p2]
                        let finding = stunMappingFinding obs
                        assertEqual "" (No, Just "The mapping depends on the destination (a \"symmetric\" NAT)") (fVerdict finding, Text.takeWhile (/= ':') <$> fMeaning finding)
                , testCase "a lost request is sent again" $
                    withResponder 1 (\n from req -> if n == 0 then Nothing else honest n from req) $ \p -> do
                        obs <- scanStunWith [100000, 2000000] [StunServer "127.0.0.1" p]
                        assertEqual "" [True] [obsOutcome o == maybe (Silent "no local address") (\l -> Answered (Mapped l Nothing)) (obsLocal o) | o <- obs]
                , testCase "silence, and an answer to another transaction, are no answer" $
                    withResponder 1 (\_ _ _ -> Nothing) $ \quiet -> withResponder 1 (\_ _ _ -> Just (stunMessage 0x0101 (BS.replicate 12 7) [(0x0020, xorMapped (203, 0, 113, 4) 40001)])) $ \stray -> do
                        let servers = [StunServer "127.0.0.1" quiet, StunServer "127.0.0.1" stray]
                        obs <- scanStunWith [100000, 100000] servers
                        assertEqual "" [Silent "no answer within 0.2s (2 requests sent)", Silent "no answer within 0.2s (2 requests sent)"] (obsOutcome <$> obs)
                        assertEqual "" [No, No] (fVerdict . (`stunFinding` obs) <$> servers)
                        assertEqual "" Unknown (fVerdict (stunMappingFinding obs))
                , testCase "the right transaction from another address than the one asked is not an answer" $
                    withResponderIO 1 (\_ k from req -> bracket (Net.socket Net.AF_INET Net.Datagram Net.defaultProtocol) Net.close (\other -> mapM_ (\b -> sendAllTo other b from) (honest k from req))) $ \p -> do
                        obs <- scanStunWith [200000] [StunServer "127.0.0.1" p]
                        assertEqual "" [Silent "no answer within 0.2s (1 request sent)"] (obsOutcome <$> obs)
                , testCase "an error response is reported, not waited out" $
                    withResponder 1 (\_ _ req -> Just (stunMessage 0x0111 (BS.take 12 (BS.drop 8 req)) [(0x0009, hex "00 00 04 01" <> "Unauthorized")])) $ \p -> do
                        obs <- scanStunWith [2000000] [StunServer "127.0.0.1" p]
                        assertEqual "" [Answered (Refused 401 "Unauthorized")] (obsOutcome <$> obs)
                ]
            ]
        ]
