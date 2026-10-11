{-# LANGUAGE OverloadedStrings #-}

{- | Coverage for @salmon-report@'s hairpin test ("Report.Hairpin").

Layer 0: declarations, the reading of the gateway's listing, the choice of
the external address, the judgement (unit cases and QuickCheck properties),
the probes' shape with every action faked.

Loopback: the real TCP connection ('connectAttempt') is made against
listeners this suite opens on free loopback ports. A gateway is /stood in
for/: the "external address" the judgement is told about is a documentation
address, and the connection is dialled to the loopback listener that plays
the mapped port (or the gateway's own service). No gateway, no NAT and no
@upnpc@ is involved, so what a real router does with such a connection is
not covered; the @upnpc -l@ listing is written from the tool's format.
-}
module Test.ReportHairpinSpec (tests) where

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import Data.Either (isLeft, isRight)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as Text
import qualified Network.Socket as Net

import Report
import Report.Dns (Judgement (..))
import Report.Hairpin
import Report.Inbound (Listener (..), withListener)
import Salmon.Builtin.Nodes.PortMapping.Upnpc (Mapping (..), Proto (..))

-------------------------------------------------------------------------------
-- Fixtures

{- | Hand-written after @upnpc.c@'s @-l@ output (miniupnpc 2.2.6), as the
fixtures of @Test.PortMappingSpec@ are: not captured from a live gateway.
-}
listing :: Text.Text
listing =
    Text.unlines
        [ "upnpc : miniupnpc library test client, version 2.2.6."
        , "List of UPNP devices found on the network :"
        , " desc: http://192.168.1.1:5000/rootDesc.xml"
        , "Found valid IGD : http://192.168.1.1:5000/ctl/IPConn"
        , "Local LAN ip address : 192.168.1.5"
        , "Connection Type : IP_Routed"
        , "ExternalIPAddress = 203.0.113.7"
        , " i protocol exPort->inAddr:inPort description remoteHost leaseTime"
        , " 0 TCP  8443->192.168.1.5:9443  'salmon' '' 0"
        , " 1 UDP 51820->192.168.1.5:51820 'wg' '' 3600"
        , " 2 TCP    80->192.168.1.9:8080  'web' '' 0"
        ]

noGateway :: Text.Text
noGateway = "upnpc : miniupnpc library test client, version 2.2.6.\nNo IGD UPnP Device found on the network !\n"

toHere, toOther :: Mapping
toHere = Mapping TCP 8443 "192.168.1.5" 9443 "salmon" "" 0
toOther = Mapping TCP 80 "192.168.1.9" 8080 "web" "" 0

token :: Text.Text
token = "salmon-token-0123abcd"

declared :: External
declared = External "203.0.113.7" Declared

-- | A target with a listener, and the evidence of a gateway that lists its mapping.
withToken :: HairpinTarget
withToken = HairpinTarget (Just "203.0.113.7") 8443 (Just 9443)

noToken :: HairpinTarget
noToken = HairpinTarget (Just "203.0.113.7") 8443 Nothing

evidence :: MappingState -> Maybe [Text.Text] -> Attempt -> Evidence
evidence m peers a = Evidence (Right declared) m (Just "192.168.1.5") ((\ps -> Right (token, ps)) <$> peers) (Just a)

verdict :: Judgement -> Verdict
verdict j = case jOk j of
    Just True -> Yes
    Just False -> No
    Nothing -> Unknown

said :: Judgement -> Text.Text
said j = Text.unlines (jEvidence j <> maybe [] pure (jMeaning j))

-- | A directive from before the hairpin test existed.
oldDirective :: Either String Spec
oldDirective = Aeson.eitherDecode "{\"specNames\":[],\"specTcp\":[],\"specEcho\":null,\"specGateway\":null,\"specUpnp\":false,\"specNatpmp\":false,\"specDomain\":null,\"specNmap\":[],\"specStun\":[]}"

-- | Actions that fail the test when called, to be overridden field by field.
untouched :: HairpinIO
untouched =
    HairpinIO
        { hioGateway = assertFailure "the gateway was asked" >> pure (Left "asked")
        , hioEcho = assertFailure "the echo URL was asked" >> pure Nothing
        , hioLocal = \_ _ -> assertFailure "a route was looked up" >> pure Nothing
        , hioConnect = \_ _ _ -> assertFailure "a connection was made" >> pure (AttemptFailed "asked")
        }

-- | A gateway that lists external port 8443 as leading to this host's given local port.
gatewayListing :: Int -> HairpinIO -> HairpinIO
gatewayListing local hio =
    hio
        { hioGateway = pure (Right (GatewayView (Just "203.0.113.7") (Just "192.168.1.5") [Mapping TCP 8443 "192.168.1.5" local "salmon" "" 0]))
        , hioLocal = \_ _ -> pure (Just "192.168.1.5")
        }

{- | The stand-in for a gateway: whatever address and port the probe means
to connect to, the real connection goes to this loopback port.
-}
dialling :: Int -> HairpinIO -> HairpinIO
dialling port hio = hio{hioConnect = \_ _ readBack -> connectAttempt 2000000 300000 "127.0.0.1" port readBack}

-- | A loopback port that accepts (the kernel's backlog does) and never says anything.
withMute :: (Int -> IO a) -> IO a
withMute act = bracket open Net.close (\s -> Net.socketPort s >>= act . fromIntegral)
  where
    open = do
        s <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
        Net.withFdSocket s Net.setCloseOnExecIfNeeded
        Net.bind s (Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1)))
        Net.listen s 4
        pure s

-- | A loopback port nothing listens on: bound, read, and closed again.
closedPort :: IO Int
closedPort = withMute pure

-------------------------------------------------------------------------------
-- Generators

genPort :: Gen Int
genPort = frequency [(3, elements [80, 443, 8443, 9443]), (1, choose (1, 65535))]

-- | A small pool, so that "the same address" happens.
genAddress :: Gen Text.Text
genAddress = elements ["203.0.113.7", "198.51.100.4", "192.168.1.5", "192.168.1.9", "10.0.0.2", "100.64.0.9", "127.0.0.1"]

genTarget :: Gen HairpinTarget
genTarget = HairpinTarget <$> frequency [(1, pure Nothing), (2, Just <$> genAddress)] <*> genPort <*> frequency [(1, pure Nothing), (1, Just <$> genPort)]

genMapping :: Gen Mapping
genMapping = Mapping <$> elements [TCP, UDP] <*> genPort <*> genAddress <*> genPort <*> elements ["salmon", "", "it's"] <*> pure "" <*> elements [0, 3600]

genMappingState :: Gen MappingState
genMappingState = oneof [Unlisted <$> elements ["no UPnP gateway answered", "upnpc could not run"], pure NotListed, ListedHere <$> genMapping, ListedElsewhere <$> genMapping]

genToken :: Gen Text.Text
genToken = ("salmon-token-" <>) . Text.pack <$> vectorOf 32 (elements "0123456789abcdef")

genAttempt :: Text.Text -> Gen Attempt
genAttempt tok =
    oneof
        [ Accepted <$> elements [[], [tok], ["SSH-20-x"], ["HTTP11", tok]]
        , pure AttemptRefused
        , pure AttemptTimedOut
        , pure (AttemptUnreachable "connect: does not exist (No route to host)")
        , pure (AttemptFailed "connect: failed")
        ]

-- | Evidence and the token it holds (a token is always drawn, used or not).
genEvidence :: Gen (Evidence, Text.Text)
genEvidence = do
    tok <- genToken
    ext <- frequency [(1, pure (Left "no external IPv4 address is known")), (5, Right <$> (External <$> genAddress <*> elements [Declared, FromGateway, FromEcho]))]
    m <- genMappingState
    local <- frequency [(1, pure Nothing), (3, Just <$> genAddress)]
    listener <-
        frequency
            [ (1, pure Nothing)
            , (1, pure (Just (Left "could not listen on local port 9443: in use")))
            , (4, (\ps -> Just (Right (tok, ps))) <$> elements [[], ["192.168.1.1:50000"], ["192.168.1.5:50000", "192.168.1.5:50001"]])
            ]
    a <- frequency [(1, pure Nothing), (6, Just <$> genAttempt tok)]
    pure (Evidence ext m local listener a, tok)

{- | A target and evidence that lets the connection be made, weighted towards
what settles a verdict: an address that is not this host's, the listener the
target declares, a mapping that leads to it.
-}
genSettling :: Gen (HairpinTarget, Evidence, Text.Text)
genSettling = do
    tok <- genToken
    t <- genTarget
    ext <- External <$> elements ["203.0.113.7", "198.51.100.4", "100.64.0.9"] <*> elements [Declared, FromGateway, FromEcho]
    m <- frequency [(3, ListedHere <$> genMapping), (1, genMappingState)]
    peers <- frequency [(2, pure []), (1, pure ["192.168.1.1:50000"])]
    a <- genAttempt tok
    pure (t, Evidence (Right ext) m (Just "192.168.1.5") (Just (Right (tok, peers))) (Just a), tok)

-- | Any evidence at all, or evidence that settles more often.
genJudged :: Gen (HairpinTarget, Evidence, Text.Text)
genJudged = oneof [(\t (ev, tok) -> (t, ev, tok)) <$> genTarget <*> genEvidence, genSettling]

-- | Declarations near the grammar, right and wrong.
genDeclaration :: Gen Text.Text
genDeclaration =
    oneof
        [ renderHairpinTarget <$> genTarget
        , Text.pack <$> listOf (elements "0123456789.:@ -;$a[]/")
        , Text.pack <$> arbitrary
        ]

-------------------------------------------------------------------------------

tests :: TestTree
tests =
    testGroup
        "salmon-report hairpin"
        [ testGroup
            "declarations"
            [ testCase "a target is a port, maybe an IPv4 address, maybe a local port" $ do
                assertEqual "" (Right (HairpinTarget Nothing 8443 Nothing)) (parseHairpinTarget "8443")
                assertEqual "" (Right (HairpinTarget Nothing 443 (Just 8443))) (parseHairpinTarget " 443@8443 ")
                assertEqual "" (Right (HairpinTarget (Just "203.0.113.7") 443 Nothing)) (parseHairpinTarget "203.0.113.7:443")
                assertEqual "" (Right (HairpinTarget (Just "203.0.113.7") 443 (Just 8443))) (parseHairpinTarget "203.0.113.7:443@8443")
                assertEqual "an address is rewritten from its octets" (Right "10.0.0.1:443") (renderHairpinTarget <$> parseHairpinTarget "010.000.0.1:443")
            , testCase "a name, an IPv6 address, a range or anything else is refused" $ do
                let mustRefuse t = assertEqual (Text.unpack t) True (isLeft (parseHairpinTarget t))
                mapM_
                    mustRefuse
                    [ ""
                    , "0"
                    , "65536"
                    , "443@"
                    , "443@0"
                    , "443@80@81"
                    , "22-25"
                    , "80,443"
                    , "example.org:443"
                    , "[2001:db8::1]:443"
                    , "2001:db8::1:443"
                    , "203.0.113.7"
                    , "203.0.113.7:"
                    , "203.0.113.256:443"
                    , "203.0.113:443"
                    , "203.0.113.7.1:443"
                    , "203.0.113.0/24:443"
                    , "-443"
                    , "$(id):443"
                    , "443;id"
                    , "+443"
                    , "4 43"
                    ]
            , testCase "an older directive reads as no hairpin test, and the field round-trips" $ do
                assertEqual "" (Right []) (specHairpin <$> oldDirective)
                assertEqual "" (Right ["203.0.113.7:443@8443"]) (specHairpin <$> (Aeson.eitherDecode . Aeson.encode . (\s -> s{specHairpin = ["203.0.113.7:443@8443"]}) =<< oldDirective))
            , testCase "the question and the method name the target, and nothing more" $ do
                assertEqual "" "Does the gateway hairpin a TCP connection from this host to its external address, port 8443?" (hairpinQuestion (HairpinTarget Nothing 8443 Nothing))
                assertEqual "" "Does the gateway hairpin a TCP connection from this host to 203.0.113.7:8443, back to this host's port 9443?" (hairpinQuestion withToken)
                assertEqual "" "one TCP connection from this host to the external address; upnpc -l for the gateway's mappings" (hairpinMethod noToken)
                assertEqual "" "one TCP connection from this host to the external address, one-time token served on local port 9443; upnpc -l for the gateway's mappings" (hairpinMethod withToken)
            , testProperty "a target reads back from what it renders to" $
                forAll genTarget $ \t -> parseHairpinTarget (renderHairpinTarget t) === Right t
            , testProperty "whatever is accepted is in range, re-renders to digits, dots, one colon and one @, and reads back" $
                forAll genDeclaration $ \raw -> case parseHairpinTarget raw of
                    Left why -> property (not (Text.null why))
                    Right t ->
                        let r = renderHairpinTarget t
                         in conjoin
                                [ counterexample "port" (hpPort t >= 1 && hpPort t <= 65535)
                                , counterexample "local port" (maybe True (\l -> l >= 1 && l <= 65535) (hpListen t))
                                , counterexample "alphabet" (Text.all (`elem` ("0123456789.:@" :: String)) r)
                                , counterexample "address" (maybe True (\a -> parseIPv4 a == Just a) (hpAddress t))
                                , parseHairpinTarget r === Right t
                                ]
            , testProperty "an IPv4 address is rewritten once and for all, whatever its leading zeros" $
                forAll (vectorOf 4 (choose (0, 255 :: Int))) $ \os ->
                    forAll (vectorOf 4 (elements ["", "0", "00"])) $ \pads ->
                        let plain = Text.intercalate "." (Text.pack . show <$> os)
                            padded = Text.intercalate "." [if Text.length x <= 3 then x else Text.pack (show o) | (o, pad) <- zip os pads, let x = pad <> Text.pack (show o)]
                         in conjoin [parseIPv4 padded === Just plain, (parseIPv4 plain >>= parseIPv4) === Just plain]
            , testProperty "distinct targets ask distinct questions (so distinct probe refs)" $
                forAll genTarget $ \a -> forAll genTarget $ \b -> (hairpinQuestion a == hairpinQuestion b) === (a == b)
            ]
        , testGroup
            "the external address"
            [ testCase "the declared one, else the gateway's, else the echo's; only IPv4 counts" $ do
                let anyTarget = HairpinTarget Nothing 8443 Nothing
                assertEqual "" (Right (External "203.0.113.7" Declared)) (chooseExternal noToken (Just "198.51.100.4") (Just "192.0.2.1"))
                assertEqual "" (Right (External "198.51.100.4" FromGateway)) (chooseExternal anyTarget (Just "198.51.100.4") (Just "192.0.2.1"))
                assertEqual "" (Right (External "192.0.2.1" FromEcho)) (chooseExternal anyTarget Nothing (Just " 192.0.2.1\n"))
                assertEqual "an echo that answers a page is no address" (Right (External "192.0.2.1" FromEcho)) (chooseExternal anyTarget (Just "<html>") (Just "192.0.2.1"))
                assertBool "" (isLeft (chooseExternal anyTarget Nothing (Just "2001:db8::1")))
                assertBool "" (isLeft (chooseExternal anyTarget Nothing Nothing))
            , testProperty "a declared address always wins, and what is chosen is an IPv4 address" $
                forAll genTarget $ \t -> forAll (liftArbitrary genAddress) $ \gw -> forAll (liftArbitrary (oneof [genAddress, pure "<html>"])) $ \echo ->
                    case chooseExternal t gw echo of
                        Right e -> conjoin [parseIPv4 (extAddress e) === Just (extAddress e), (extSource e == Declared) === isJust (hpAddress t)]
                        Left _ -> conjoin [hpAddress t === Nothing, gw === Nothing, counterexample "echo" (maybe True (isNothing . parseIPv4) echo)]
            ]
        , testGroup
            "the gateway's listing"
            [ testCase "addresses and mappings of a listing; no gateway is a reason" $ do
                assertEqual "" (Right (GatewayView (Just "203.0.113.7") (Just "192.168.1.5") [toHere, Mapping UDP 51820 "192.168.1.5" 51820 "wg" "" 3600, toOther])) (readGatewayListing listing)
                assertEqual "" (Left "no UPnP gateway answered") (readGatewayListing noGateway)
            , testCase "a mapping is here when it leads to this host, and to the listener if there is one" $ do
                let ms = gvMappings <$> readGatewayListing listing
                    view t = viewMapping t ["192.168.1.5"] ms
                assertEqual "" (ListedHere toHere) (view withToken)
                assertEqual "" (ListedHere toHere) (view noToken)
                assertEqual "another local port than the mapping's" (ListedElsewhere toHere) (view withToken{hpListen = Just 9000})
                assertEqual "another host" (ListedElsewhere toOther) (view (HairpinTarget Nothing 80 Nothing))
                assertEqual "a UDP mapping of the port is not one" NotListed (view (HairpinTarget Nothing 51820 Nothing))
                assertEqual "" NotListed (view (HairpinTarget Nothing 22 Nothing))
                assertEqual "this host's address unknown" (ListedElsewhere toHere) (viewMapping withToken [] ms)
                assertEqual "" (Unlisted "no UPnP gateway answered") (viewMapping withToken ["192.168.1.5"] (gvMappings <$> readGatewayListing noGateway))
            , testProperty "the view is about TCP mappings of the target's port, and nothing else" $
                forAll genTarget $ \t -> forAll (listOf genMapping) $ \ms -> forAll (listOf genAddress) $ \locals ->
                    let relevant = [m | m <- ms, mapProto m == TCP, mapExternalPort m == hpPort t]
                     in case viewMapping t locals (Right ms) of
                            NotListed -> relevant === []
                            ListedHere m ->
                                conjoin
                                    [ counterexample "relevant" (m `elem` relevant)
                                    , counterexample "local" (mapInternalAddress m `elem` locals)
                                    , counterexample "listener" (maybe True (== mapInternalPort m) (hpListen t))
                                    ]
                            ListedElsewhere m ->
                                conjoin
                                    [ counterexample "relevant" (m `elem` relevant)
                                    , counterexample "none is here" (not (any (\x -> mapInternalAddress x `elem` locals && maybe True (== mapInternalPort x) (hpListen t)) relevant))
                                    ]
                            Unlisted _ -> property False
            , testProperty "mappings of other ports or of UDP change nothing" $
                forAll genTarget $ \t -> forAll (listOf genMapping) $ \ms -> forAll (listOf genMapping) $ \noise ->
                    let others = [m | m <- noise, mapProto m /= TCP || mapExternalPort m /= hpPort t]
                     in viewMapping t ["192.168.1.5"] (Right (others <> ms <> others)) === viewMapping t ["192.168.1.5"] (Right ms)
            ]
        , testGroup
            "judging"
            [ testCase "the token read back is a yes, whatever the gateway lists, and it stays out of the evidence" $ do
                let j = judgeHairpin withToken (evidence NotListed (Just ["192.168.1.1:50000"]) (Accepted [token]))
                assertEqual "" Yes (verdict j)
                assertEqual
                    ""
                    [ "the external address is 203.0.113.7 (public), as declared"
                    , "the connection leaves from 192.168.1.5"
                    , "the gateway lists no TCP mapping of external port 8443 (a forwarding configured by hand on the router may not be listed)"
                    , "this host connected to 203.0.113.7:8443 and read back the token it serves"
                    , "the local listener accepted a connection from 192.168.1.1:50000"
                    ]
                    (jEvidence j)
                assertBool "" (not (token `Text.isInfixOf` said j))
            , testCase "something else answering is a no only when the listed mapping leads to the listener" $ do
                let other m = judgeHairpin withToken (evidence m (Just []) (Accepted ["SSH-20-x"]))
                assertEqual "" No (verdict (other (ListedHere toHere)))
                assertBool "" ("the gateway lists TCP 8443 -> 192.168.1.5:9443, this report's listener" `elem` jEvidence (other (ListedHere toHere)))
                assertBool "" ("no connection reached the local listener" `elem` jEvidence (other (ListedHere toHere)))
                assertEqual "" [Unknown, Unknown, Unknown] (verdict . other <$> [NotListed, ListedElsewhere toOther, Unlisted "no UPnP gateway answered"])
                -- the listener saw a connection and the token was not read: not settled
                assertEqual "" Unknown (verdict (judgeHairpin withToken (evidence (ListedHere toHere) (Just ["192.168.1.1:50000"]) (Accepted []))))
            , testCase "without a token, accepted is a yes only on a listed mapping, and says what it does not show" $ do
                let j m = judgeHairpin noToken (evidence m Nothing (Accepted []))
                assertEqual "" [Yes, Yes, Unknown, Unknown] (verdict . j <$> [ListedHere toHere, ListedElsewhere toOther, NotListed, Unlisted "upnpc could not run"])
                assertBool "" (maybe False (Text.isInfixOf "does not show that the mapped host is what answered") (jMeaning (j (ListedHere toHere))))
                assertBool "" (maybe False (Text.isInfixOf "an administration page") (jMeaning (j NotListed)))
            , testCase "refused is a no only with the listed mapping, the listener, and nothing arriving" $ do
                assertEqual "" No (verdict (judgeHairpin withToken (evidence (ListedHere toHere) (Just []) AttemptRefused)))
                assertEqual "" Unknown (verdict (judgeHairpin withToken (evidence (ListedHere toHere) (Just ["192.168.1.1:50000"]) AttemptRefused)))
                assertEqual "" Unknown (verdict (judgeHairpin withToken (evidence NotListed (Just []) AttemptRefused)))
                assertEqual "no listener: the mapped host may be what refused" Unknown (verdict (judgeHairpin noToken (evidence (ListedHere toHere) Nothing AttemptRefused)))
            , testCase "a timeout, no route and a failure are unknown, with their evidence" $ do
                let j = judgeHairpin withToken . evidence (ListedHere toHere) (Just [])
                assertEqual "" [Unknown, Unknown, Unknown] (verdict . j <$> [AttemptTimedOut, AttemptUnreachable "no route", AttemptFailed "failed"])
                assertBool "" ("no answer from 203.0.113.7:8443 within 3s" `elem` jEvidence (j AttemptTimedOut))
                assertBool "" (maybe False (Text.isInfixOf "A timeout does not tell these apart") (jMeaning (j AttemptTimedOut)))
            , testCase "no address, this host's own address, loopback and an unbound local port are unknown before any connection" $ do
                let blocked t ev = (verdict <$> hairpinBlocked t ev, jEvidence (judgeHairpin t ev))
                    base = evidence (ListedHere toHere) (Just []) (Accepted [token])
                assertEqual "" (Just Unknown, ["none known"]) (blocked withToken base{evExternal = Left "none known"})
                assertEqual "" (Just Unknown, ["the external address is 203.0.113.7 (public), as declared", "this host holds that address itself"]) (blocked withToken base{evLocal = Just "203.0.113.7"})
                assertEqual "" (Just Unknown) (fst (blocked withToken base{evExternal = Right (External "127.0.0.1" FromEcho)}))
                assertEqual "" (Just Unknown, ["the external address is 203.0.113.7 (public), as declared", "no listener was opened on local port 9443"]) (blocked withToken base{evListener = Nothing})
                assertEqual "" (Just Unknown, ["the external address is 203.0.113.7 (public), as declared", "could not listen on local port 9443: in use"]) (blocked withToken base{evListener = Just (Left "could not listen on local port 9443: in use")})
                assertEqual "" Nothing (hairpinBlocked withToken base)
                assertEqual "an address behind another NAT is said so, and still tested" Yes (verdict (judgeHairpin withToken base{evExternal = Right (External "100.64.0.9" FromGateway)}))
                assertBool "" (any (Text.isInfixOf "carrier-grade NAT") (jEvidence (judgeHairpin withToken base{evExternal = Right (External "100.64.0.9" FromGateway)})))
            , testProperty "a yes is a token read back, or an accepted connection on a listed mapping" $
                checkCoverage . forAll genJudged $ \(t, ev, tok) ->
                    let yes = verdict (judgeHairpin t ev) == Yes
                     in cover 5 yes "yes" $
                            not yes .||. case (evAttempt ev, hpListen t, evListener ev) of
                                (Just (Accepted back), Just _, Just (Right (served, _))) -> served === tok .&&. counterexample "token" (tok `elem` back)
                                (Just (Accepted _), Nothing, _) -> counterexample "listed" (listed (evMapping ev))
                                _ -> property False
            , testProperty "a no is a listed mapping to the listener, nothing arriving, and a connection that did not come back" $
                checkCoverage . forAll genJudged $ \(t, ev, tok) ->
                    let no = verdict (judgeHairpin t ev) == No
                     in cover 2 no "no" $
                            not no
                                .||. conjoin
                                    [ counterexample "listener declared" (isJust (hpListen t))
                                    , counterexample "listener, nothing arrived" (evListener ev == Just (Right (tok, [])))
                                    , counterexample "mapping here" (here (evMapping ev))
                                    , counterexample "attempt" (case evAttempt ev of Just AttemptRefused -> True; Just (Accepted back) -> tok `notElem` back; _ -> False)
                                    ]
            , testProperty "nothing but an accepted or a refused connection settles anything" $
                forAll genTarget $ \t -> forAll genEvidence $ \(ev, _) ->
                    forAll (elements [Nothing, Just AttemptTimedOut, Just (AttemptUnreachable "no route"), Just (AttemptFailed "failed")]) $ \a ->
                        verdict (judgeHairpin t ev{evAttempt = a}) === Unknown
            , testProperty "what forbids the connection is unknown, whatever a connection would have said" $
                forAll genTarget $ \t -> forAll genEvidence $ \(ev, tok) -> forAll (genAttempt tok) $ \a ->
                    case hairpinBlocked t ev of
                        Nothing -> property (isRight (evExternal ev))
                        Just j -> conjoin [verdict j === Unknown, judgeHairpin t ev{evAttempt = Just a} === j, judgeHairpin t ev === j]
            , testProperty "no external address, the host's own address or a loopback one is never settled" $
                forAll genTarget $ \t -> forAll genEvidence $ \(ev, _) ->
                    let own = either (const True) (\e -> Just (extAddress e) == evLocal ev || "127." `Text.isPrefixOf` extAddress e) (evExternal ev)
                     in own ==> verdict (judgeHairpin t ev) === Unknown
            , testProperty "the token is in no evidence and no meaning, and there is always evidence" $
                forAll genTarget $ \t -> forAll genEvidence $ \(ev, tok) ->
                    let j = judgeHairpin t ev
                     in conjoin
                            [ counterexample (Text.unpack (said j)) (not (tok `Text.isInfixOf` said j))
                            , counterexample "no evidence" (not (null (jEvidence j)))
                            , counterexample "no token prefix" (not ("salmon-token" `Text.isInfixOf` said j))
                            ]
            , testProperty "without a declared local port, a listener's state changes nothing" $
                forAll genTarget $ \t -> forAll genEvidence $ \(ev, _) -> forAll genEvidence $ \(other, _) ->
                    let t' = t{hpListen = Nothing}
                     in judgeHairpin t' ev === judgeHairpin t' ev{evListener = evListener other}
            ]
        , testGroup
            "reading the token"
            [ testCase "a token line, a banner, and noise" $ do
                assertEqual "" ["salmon-token-0123abcd"] (tokenLines "salmon-token-0123abcd\n")
                assertEqual "" ["SSH-20-OpenSSH_96"] (tokenLines "SSH-2.0-OpenSSH_9.6\r\n")
                assertEqual "" [] (tokenLines "")
                assertEqual "" [] (tokenLines "\n\n $;\n")
            , testProperty "a served token is read back as itself" $
                forAll genToken $ \tok -> tokenLines (Char8.pack (Text.unpack tok) <> "\n") === [tok]
            , testProperty "whatever arrives, at most four non-empty lines in the token's alphabet" $
                forAll (Char8.pack <$> arbitrary) $ \bytes ->
                    let ls = tokenLines bytes
                     in conjoin
                            [ counterexample "count" (length ls <= 4)
                            , counterexample "empty" (not (any Text.null ls))
                            , counterexample "alphabet" (all (Text.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_-"))) ls)
                            ]
            ]
        , testGroup
            "probes"
            [ testCase "one probe per distinct target; a bad declaration is one probe and connects to nothing" $ do
                c <- newCollector
                connects <- newIORef ([] :: [(Text.Text, Int, Bool)])
                let hio =
                        (gatewayListing 9443 untouched)
                            { hioConnect = \a p r -> atomicModifyIORef' connects (\xs -> (xs <> [(a, p, r)], ())) >> pure AttemptTimedOut
                            }
                    probes = hairpinProbes c hio Map.empty ["8443", " 8443 ", "198.51.100.4:443", "example.org:443", "22-25"]
                assertEqual "" 4 (length probes)
                fs <- runReport 2000000 c (reportOp probes)
                assertEqual "" [Unknown, Unknown, Unknown, Unknown] (fVerdict <$> fs)
                assertEqual "the gateway's address when none is declared, the declared one otherwise" [("198.51.100.4", 443, False), ("203.0.113.7", 8443, False)] . Map.keys . Map.fromList . fmap (\x -> (x, ())) =<< readIORef connects
                assertBool "the grammar is named" (any (Text.isInfixOf "[ADDR:]PORT[@LISTEN]") (concatMap fEvidence fs))
                assertEqual "" ["hairpin test, not run", "hairpin test, not run"] [fMethod f | f <- fs, fMethod f == "hairpin test, not run"]
                assertEqual "no address for the cross-check to pick up" [] (concatMap fAddresses fs)
            , testCase "nothing is declared, nothing is probed" $ do
                c <- newCollector
                assertEqual "" 0 (length (hairpinProbes c untouched Map.empty []))
            , testCase "without an external address nothing is connected to, and the echo is only asked then" $ do
                let silent = untouched{hioGateway = pure (Left "no UPnP gateway answered")}
                a <- hairpinFinding silent{hioEcho = pure Nothing} Map.empty (HairpinTarget Nothing 8443 Nothing)
                assertEqual "" Unknown (fVerdict a)
                assertBool "" (any (Text.isInfixOf "no external IPv4 address is known") (fEvidence a))
                -- a declared address: the echo URL is not asked ('untouched' would fail)
                b <- hairpinFinding silent{hioLocal = \_ _ -> pure (Just "192.168.1.5"), hioConnect = \_ _ _ -> pure AttemptRefused} Map.empty noToken
                assertEqual "" Unknown (fVerdict b)
                assertBool "" ("the gateway's mappings could not be listed: no UPnP gateway answered" `elem` fEvidence b)
                -- the echo's answer is used when the gateway gives none
                e <- hairpinFinding silent{hioEcho = pure (Just "198.51.100.4"), hioLocal = \_ _ -> pure (Just "192.168.1.5"), hioConnect = \_ _ _ -> pure AttemptTimedOut} Map.empty (HairpinTarget Nothing 8443 Nothing)
                assertBool "" ("the external address is 198.51.100.4 (public), as the declared --echo URL answers" `elem` fEvidence e)
            , testCase "a declared local port with no listener, or one that could not be bound, is not connected to" $ do
                let hio = gatewayListing 9443 untouched
                a <- hairpinFinding hio Map.empty withToken
                b <- hairpinFinding hio (Map.fromList [(9443, Left "could not listen on local port 9443: in use")]) withToken
                assertEqual "" (Unknown, "no listener was opened on local port 9443") (fVerdict a, last (fEvidence a))
                assertEqual "" (Unknown, "could not listen on local port 9443: in use") (fVerdict b, last (fEvidence b))
            , testCase "--no-upnp runs no upnpc, and says the mappings were not listed" $ do
                spec <- either assertFailure pure oldDirective
                r <- hioGateway (hairpinIO spec)
                assertEqual "" (Left "UPnP discovery is off (--no-upnp)") r
                e <- hioEcho (hairpinIO spec)
                assertEqual "no echo URL declared, none asked" Nothing e
            , testCase "the listeners of a directive are those of its inbound and hairpin targets, each port once" $ do
                spec <- either assertFailure pure oldDirective
                p <- closedPort
                let at = "@" <> Text.pack (show p)
                n <- withReportListeners spec{specHairpin = ["8443" <> at, "443" <> at, "22-25" <> at, "80"]} (\ls -> pure [(k, isRight l) | (k, l) <- Map.toList ls])
                assertEqual "" [(p, True)] n
                none <- withReportListeners spec{specHairpin = ["8443"]} (pure . Map.size)
                assertEqual "" 0 none
            ]
        , testGroup
            "loopback (a loopback port stands in for the gateway's external port)"
            [ testCase "the connection reaches this report's listener and reads its token: yes" $
                withListener 0 $ \l -> case l of
                    Left why -> assertFailure (Text.unpack why)
                    Right listener -> do
                        let p = listenerPort listener
                            t = HairpinTarget (Just "203.0.113.7") 8443 (Just p)
                        f <- hairpinFinding (dialling p (gatewayListing p untouched)) (Map.fromList [(p, l)]) t
                        assertEqual (show f) Yes (fVerdict f)
                        assertBool "the listener saw the connection" (any (Text.isInfixOf "the local listener accepted a connection from ") (fEvidence f))
                        assertBool "no token in the finding" (not (Text.isInfixOf (listenerToken listener) (Text.pack (show f))))
                        assertBool "nor in what is printed" (not (Text.isInfixOf "salmon-token" (renderFinding f <> Text.pack (show (findingValue f)))))
            , testCase "another service answers where the listed mapping should lead: no" $
                withListener 0 $ \mine -> withListener 0 $ \theirs -> case (mine, theirs) of
                    (Right listener, Right other) -> do
                        let p = listenerPort listener
                            t = HairpinTarget (Just "203.0.113.7") 8443 (Just p)
                            hio = dialling (listenerPort other) (gatewayListing p untouched)
                        f <- hairpinFinding hio (Map.fromList [(p, mine)]) t
                        assertEqual (show f) No (fVerdict f)
                        assertBool "" ("no connection reached the local listener" `elem` fEvidence f)
                        -- the same, when the gateway does not list the mapping: not settled
                        g <- hairpinFinding hio{hioGateway = pure (Left "no UPnP gateway answered")} (Map.fromList [(p, mine)]) t
                        assertEqual (show g) Unknown (fVerdict g)
                    _ -> assertFailure "could not listen on loopback"
            , testCase "a refused connection: no with the listed mapping and the listener, unknown without" $
                withListener 0 $ \l -> case l of
                    Left why -> assertFailure (Text.unpack why)
                    Right listener -> do
                        gone <- closedPort
                        let p = listenerPort listener
                            hio = dialling gone (gatewayListing p untouched)
                        f <- hairpinFinding hio (Map.fromList [(p, l)]) (HairpinTarget (Just "203.0.113.7") 8443 (Just p))
                        assertEqual (show f) No (fVerdict f)
                        assertBool "" ("the connection to 203.0.113.7:8443 was refused" `elem` fEvidence f)
                        g <- hairpinFinding hio Map.empty noToken
                        assertEqual (show g) Unknown (fVerdict g)
            , testCase "something that accepts and says nothing is waited for, then read as no token" $
                withMute $ \mute -> do
                    a <- connectAttempt 2000000 200000 "127.0.0.1" mute True
                    assertEqual "" (Accepted []) a
                    b <- connectAttempt 2000000 200000 "127.0.0.1" mute False
                    assertEqual "" (Accepted []) b
            , testCase "the real route lookup and connection, to this host's own address: nothing is connected to" $
                withListener 0 $ \l -> case l of
                    Left why -> assertFailure (Text.unpack why)
                    Right listener -> do
                        let p = listenerPort listener
                        local <- localAddressToward "127.0.0.1" p
                        assertEqual "" (Just "127.0.0.1") local
                        spec <- either assertFailure pure oldDirective
                        -- the real actions of a directive with --no-upnp and no --echo; the address is this host's
                        f <- hairpinFinding (hairpinIO spec) (Map.fromList [(p, l)]) (HairpinTarget (Just "127.0.0.1") p (Just p))
                        assertEqual (show f) Unknown (fVerdict f)
                        peers <- listenerPeers listener
                        assertEqual "the listener was not connected to" [] peers
            , testCase "what is not an IPv4 address is not connected to" $ do
                a <- connectAttempt 200000 200000 "example.org" 80 False
                assertEqual "" (AttemptFailed "not a dotted IPv4 address: example.org") a
                l <- localAddressToward "example.org" 80
                assertEqual "" Nothing l
            ]
        ]
  where
    listed m = case m of
        ListedHere _ -> True
        ListedElsewhere _ -> True
        _ -> False
    here m = case m of
        ListedHere _ -> True
        _ -> False
