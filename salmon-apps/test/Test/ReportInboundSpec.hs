{-# LANGUAGE OverloadedStrings #-}

{- | Coverage for @salmon-report@'s inbound test ("Report.Inbound").

Layer 0: declarations, the @ssh@ argument vector, the vantage's answer read
from captured output, the judgement, the probes' shape.

Loopback: the command a vantage would run is run /here/ (through @sh -c@
instead of @ssh@) against listeners this suite opens on free loopback ports.
No @ssh@ is started and nothing leaves the machine, so what a real second
vantage point adds (the login, the remote shell, the way in through a
router) is not covered.
-}
module Test.ReportInboundSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Data.Aeson as Aeson
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Exit (ExitCode (..))

import Report
import Report.Dns (Judgement (..))
import Report.Inbound

-- The output of 'remoteCommand', captured on 2026-10-10 by running it with
-- @sh -c@ (bash 5.2, coreutils 9.4) against throwaway listeners on the
-- loopback address of the capturing machine. Only the ports are replaced.

-- | Something listened; no token asked for.
outConnected :: Text.Text
outConnected = "salmon-inbound:connected\n\nsalmon-inbound:exit:0\n"

-- | The listener handed out a token.
outToken :: Text.Text
outToken = "salmon-inbound:connected\nsalmon-token-0123abcd\nsalmon-inbound:exit:0\n"

-- | Nothing listened.
outRefused :: Text.Text
outRefused = "bash: connect: Connection refused\nbash: line 1: /dev/tcp/127.0.0.1/28472: Connection refused\n\nsalmon-inbound:exit:1\n"

-- | Something accepted and said nothing: the read ran into @timeout@.
outSilent :: Text.Text
outSilent = "salmon-inbound:connected\n\nsalmon-inbound:exit:124\n"

-- | Something else answered (an SSH-like banner, filtered to the token's alphabet).
outOtherService :: Text.Text
outOtherService = "salmon-inbound:connected\nSSH-20-x\nsalmon-inbound:exit:0\n"

-- | A name the vantage could not resolve.
outUnresolved :: Text.Text
outUnresolved = "bash: line 1: nosuchhost.invalid: Name or service not known\nbash: line 1: /dev/tcp/nosuchhost.invalid/80: Invalid argument\n\nsalmon-inbound:exit:1\n"

{- | Hand-written: a connection nothing answers cannot be made on loopback.
@timeout@ kills the connecting shell, which prints nothing.
-}
outTimedOut :: Text.Text
outTimedOut = "\nsalmon-inbound:exit:124\n"

-- | Hand-written, after bash's wording for @EHOSTUNREACH@.
outNoRoute :: Text.Text
outNoRoute = "bash: connect: No route to host\nbash: line 1: /dev/tcp/192.0.2.7/443: No route to host\n\nsalmon-inbound:exit:1\n"

-- | Hand-written, after OpenSSH's wording: the login failed, the command never ran.
outSshDenied :: Text.Text
outSshDenied = "probe@vantage.example.org: Permission denied (publickey).\n"

-- | The vantage's command, run on this machine: the last argument, through @sh -c@.
askHere :: AskVantage
askHere args = askWith "sh" ["-c", last args]

vantage :: Vantage
vantage = Vantage "probe@vantage.example.org"

verdictAndEvidence :: Finding -> (Verdict, [Text.Text])
verdictAndEvidence f = (fVerdict f, fEvidence f)

tests :: TestTree
tests =
    testGroup
        "salmon-report inbound"
        [ testGroup
            "declarations"
            [ testCase "a target is one host, one port and maybe a local port" $ do
                assertEqual "" (Right (InboundTarget "192.0.2.7" 443 Nothing)) (parseInboundTarget "192.0.2.7:443")
                assertEqual "" (Right (InboundTarget "example.org" 443 (Just 8443))) (parseInboundTarget " example.org:443@8443 ")
                assertEqual "" (Right (InboundTarget "2001:db8::1" 22 (Just 2222))) (parseInboundTarget "[2001:db8::1]:22@2222")
                assertEqual "" (Right "[2001:db8::1]:22") (renderTarget <$> parseInboundTarget "[2001:db8::1]:22@2222")
                assertEqual "" (Right "192.0.2.7:443") (renderTarget <$> parseInboundTarget "192.0.2.7:443")
            , testCase "what the remote shell or ssh could read as something else is refused" $ do
                let mustRefuse t = assertEqual (Text.unpack t) True (isLeft (parseInboundTarget t))
                mapM_
                    mustRefuse
                    [ "192.0.2.7"
                    , "192.0.2.7:"
                    , ":443"
                    , "192.0.2.7:0"
                    , "192.0.2.7:65536"
                    , "192.0.2.7:443@"
                    , "192.0.2.7:443@0"
                    , "192.0.2.7:443@80@81"
                    , "192.0.2.7:22-25"
                    , "192.0.2.0/24:22"
                    , "-oProxyCommand=x:22"
                    , "example.org;id:22"
                    , "$(id):22"
                    , "example.org/../x:22"
                    , "a b:22"
                    , "probe@example.org:22"
                    ]
            , testCase "a vantage point is [USER@]HOST and nothing else" $ do
                assertEqual "" (Right (Vantage "vantage.example.org")) (parseVantage "vantage.example.org")
                assertEqual "" (Right (Vantage "probe@198.51.100.4")) (parseVantage " probe@198.51.100.4 ")
                let mustRefuse t = assertEqual (Text.unpack t) True (isLeft (parseVantage t))
                mapM_
                    mustRefuse
                    [ ""
                    , "-oProxyCommand=id"
                    , "-l@vantage.example.org"
                    , "@vantage.example.org"
                    , "probe@"
                    , "probe@a@b"
                    , "probe@vantage.example.org:2222"
                    , "probe@vantage.example.org -p 2222"
                    , "probe;id@vantage.example.org"
                    , "probe@vantage.example.org;id"
                    , "$(id)@vantage.example.org"
                    , "ssh://probe@vantage.example.org"
                    ]
            , testCase "an older directive reads as no inbound test, and the fields round-trip" $ do
                let old = "{\"specNames\":[],\"specTcp\":[],\"specEcho\":null,\"specGateway\":null,\"specUpnp\":false,\"specNatpmp\":false,\"specDomain\":null,\"specNmap\":[]}"
                    spec = Aeson.eitherDecode old :: Either String Spec
                    declared = InboundSpec ["192.0.2.7:443@8443"] ["probe@vantage.example.org"] (Just "/etc/salmon/vantage.ssh")
                assertEqual "" (Right noInbound) (specInbound <$> spec)
                assertEqual "" (Right declared) (specInbound <$> (Aeson.eitherDecode . Aeson.encode . (\s -> s{specInbound = declared}) =<< spec))
            ]
        , testGroup
            "asking the vantage"
            [ testCase "the remote command varies only in host, port and whether it reads back" $ do
                assertEqual
                    ""
                    "bash -c 'r=0; timeout 4 bash -c \"exec 3<>/dev/tcp/192.0.2.7/443 && echo salmon-inbound:connected\" 2>&1 || r=$?; echo; echo salmon-inbound:exit:$r'"
                    (remoteCommand (InboundTarget "192.0.2.7" 443 Nothing))
                assertEqual
                    ""
                    "bash -c 'r=0; timeout 4 bash -c \"exec 3<>/dev/tcp/2001:db8::1/22 && echo salmon-inbound:connected && head -c 128 <&3 | tr -cd a-zA-Z0-9_-\" 2>&1 || r=$?; echo; echo salmon-inbound:exit:$r'"
                    (remoteCommand (InboundTarget "2001:db8::1" 22 (Just 2222)))
            , testCase "ssh runs in batch mode, bounded, with the destination after --" $ do
                let t = InboundTarget "192.0.2.7" 443 Nothing
                assertEqual
                    ""
                    ["-o", "BatchMode=yes", "-o", "ConnectTimeout=4", "-T", "--", "probe@vantage.example.org", Text.unpack (remoteCommand t)]
                    (sshArgs Nothing vantage t)
                assertEqual "" ["-F", "/etc/salmon/vantage.ssh", "-o"] (take 3 (sshArgs (Just "/etc/salmon/vantage.ssh") vantage t))
            , testCase "the vantage's answer is read from what it printed" $ do
                assertEqual "" (Connected []) (parseVantageOutput outConnected)
                assertEqual "" (Connected ["salmon-token-0123abcd"]) (parseVantageOutput outToken)
                assertEqual "" Refused (parseVantageOutput outRefused)
                assertEqual "" (Connected []) (parseVantageOutput outSilent)
                assertEqual "" (Connected ["SSH-20-x"]) (parseVantageOutput outOtherService)
                assertEqual "" TimedOut (parseVantageOutput outTimedOut)
                assertEqual "" (Unreachable "bash: connect: No route to host") (parseVantageOutput outNoRoute)
                assertEqual
                    ""
                    (VantageFailed ["bash: line 1: nosuchhost.invalid: Name or service not known", "bash: line 1: /dev/tcp/nosuchhost.invalid/80: Invalid argument", "the vantage's command exited 1"])
                    (parseVantageOutput outUnresolved)
                assertEqual "" (VantageFailed ["probe@vantage.example.org: Permission denied (publickey)."]) (parseVantageOutput outSshDenied)
                assertEqual "" (VantageFailed []) (parseVantageOutput "")
                -- a banner printed by the login before the command does not hide the answer
                assertEqual "" Refused (parseVantageOutput ("Welcome to the vantage\n" <> outRefused))
            ]
        , testGroup
            "judging"
            [ testCase "connected without a token is a yes that says what it does not show" $ do
                let j = judgeInbound (InboundTarget "192.0.2.7" 443 Nothing) Nothing (Connected [])
                assertEqual "" (Just True, ["the vantage connected to 192.0.2.7:443"]) (jOk j, jEvidence j)
                assertBool "" (maybe False (Text.isInfixOf "does not show that it was this host") (jMeaning j))
            , testCase "with a listener, only the token read back is a yes, and it stays out of the evidence" $ do
                let t = InboundTarget "192.0.2.7" 443 (Just 8443)
                    token = "salmon-token-0123abcd"
                    yes = judgeInbound t (Just (token, ["198.51.100.4:50000"])) (parseVantageOutput outToken)
                    other = judgeInbound t (Just (token, [])) (parseVantageOutput outOtherService)
                    silent = judgeInbound t (Just (token, [])) (parseVantageOutput outSilent)
                assertEqual "" (Just True) (jOk yes)
                assertEqual "" ["the vantage connected to 192.0.2.7:443 and read back the token this host served", "the local listener accepted a connection from 198.51.100.4:50000"] (jEvidence yes)
                assertEqual "" (Just False, Just False) (jOk other, jOk silent)
                assertEqual "" ["the vantage connected to 192.0.2.7:443 but did not read this host's token", "no connection reached the local listener"] (jEvidence other)
                assertBool "no token in the evidence" (not (any (Text.isInfixOf "salmon-token") (concatMap jEvidence [yes, other, silent])))
            , testCase "refused and unanswered are a no; a vantage that could not try is unknown" $ do
                let t = InboundTarget "192.0.2.7" 443 Nothing
                    j = judgeInbound t Nothing
                assertEqual "" (Just False, ["the connection to 192.0.2.7:443 was refused"]) (jOk (j Refused), jEvidence (j Refused))
                assertEqual "" (Just False, ["no answer from 192.0.2.7:443 within 4s"]) (jOk (j TimedOut), jEvidence (j TimedOut))
                assertEqual "" Nothing (jOk (j (Unreachable "bash: connect: No route to host")))
                assertEqual "" (Nothing, ["the vantage printed nothing"]) (jOk (j (VantageFailed [])), jEvidence (j (VantageFailed [])))
                assertEqual "" Nothing (jOk (j (parseVantageOutput outSshDenied)))
            ]
        , testGroup
            "probes"
            [ testCase "one probe per target and vantage; bad or missing declarations run nothing" $ do
                c <- newCollector
                calls <- newIORef ([] :: [[String]])
                let ask args = do
                        atomicModifyIORef' calls (\xs -> (xs <> [args], ()))
                        pure (Right (ExitSuccess, outRefused))
                    good = inboundProbes c ask Map.empty (InboundSpec ["192.0.2.7:443", "192.0.2.7:443", "192.0.2.7:22"] ["probe@vantage.example.org", "other.example.org"] Nothing)
                    bad = inboundProbes c ask Map.empty (InboundSpec ["192.0.2.0/24:22"] ["-oProxyCommand=id"] Nothing)
                    alone = inboundProbes c ask Map.empty (InboundSpec ["198.51.100.9:80"] [] Nothing)
                    none = inboundProbes c ask Map.empty (InboundSpec [] ["probe@vantage.example.org"] Nothing)
                assertEqual "" (4, 2, 1, 0) (length good, length bad, length alone, length none)
                fs <- runReport 2000000 c (reportOp (good <> bad <> alone))
                assertEqual "" [No, No, No, No, Unknown, Unknown, Unknown] (fVerdict <$> fs)
                asked <- readIORef calls
                assertEqual "only the good declarations asked" 4 (length asked)
                assertBool "always in batch mode" (all (elem "BatchMode=yes") asked)
                assertBool "the missing vantage is named" (any (Text.isInfixOf "--vantage-ssh") (concatMap fEvidence fs))
            , testCase "a declared local port with no listener, or one that could not be bound, is not asked about" $ do
                let ask _ = assertFailure "asked" >> pure (Left "asked")
                    t = InboundTarget "192.0.2.7" 443 (Just 8443)
                a <- inboundFinding ask Map.empty Nothing vantage t
                b <- inboundFinding ask (Map.fromList [(8443, Left "could not listen on local port 8443: in use")]) Nothing vantage t
                assertEqual "" (Unknown, ["no listener was opened on local port 8443"]) (verdictAndEvidence a)
                assertEqual "" (Unknown, ["could not listen on local port 8443: in use"]) (verdictAndEvidence b)
            , testCase "an ssh that cannot run or cannot log in is unknown, with its exit code" $ do
                let t = InboundTarget "192.0.2.7" 443 Nothing
                a <- inboundFinding (\_ -> pure (Left "ssh could not run: does not exist")) Map.empty Nothing vantage t
                b <- inboundFinding (\_ -> pure (Right (ExitFailure 255, outSshDenied))) Map.empty Nothing vantage t
                assertEqual "" (Unknown, ["ssh could not run: does not exist"]) (verdictAndEvidence a)
                assertEqual "" (Unknown, ["probe@vantage.example.org: Permission denied (publickey).", "ssh exited 255"]) (verdictAndEvidence b)
                assertEqual "" "ssh probe@vantage.example.org, bash /dev/tcp connect to 192.0.2.7:443" (fMethod b)
            ]
        , testGroup
            "loopback (the vantage's command run here, no ssh)"
            [ testCase "the token served by this process is read back: yes" $
                withListener 0 $ \l -> case l of
                    Left why -> assertFailure (Text.unpack why)
                    Right listener -> do
                        let p = listenerPort listener
                            t = InboundTarget "127.0.0.1" p (Just p)
                        f <- inboundFinding askHere (Map.fromList [(p, l)]) Nothing vantage t
                        assertEqual (show f) Yes (fVerdict f)
                        assertBool "the listener saw the connection" (any (Text.isInfixOf "the local listener accepted a connection from") (fEvidence f))
                        assertBool "no token in the finding" (not (Text.isInfixOf (listenerToken listener) (Text.pack (show f))))
            , testCase "another listener answering at the address is a no" $
                withListeners [0] $ \ours -> withListener 0 $ \theirs -> case (Map.elems ours, theirs) of
                    ([Right mine], Right other) -> do
                        -- the "external" port leads to the other listener, not to ours
                        let t = InboundTarget "127.0.0.1" (listenerPort other) (Just 0)
                        f <- inboundFinding askHere (Map.fromList [(0, Right mine)]) Nothing vantage t
                        assertEqual (show f) No (fVerdict f)
                        assertBool "" ("no connection reached the local listener" `elem` fEvidence f)
                    _ -> assertFailure "could not listen on loopback"
            , testCase "something listening is a yes without a token; nothing listening is a no" $ do
                gone <- withListener 0 $ \l -> case l of
                    Left why -> assertFailure (Text.unpack why) >> pure 0
                    Right listener -> do
                        let p = listenerPort listener
                        f <- inboundFinding askHere Map.empty Nothing vantage (InboundTarget "127.0.0.1" p Nothing)
                        assertEqual (show f) Yes (fVerdict f)
                        -- the same port again cannot be bound while this one is held
                        withListener p $ \second -> assertBool "a second bind is a reason" (either (Text.isPrefixOf "could not listen on local port") (const False) second)
                        pure p
                -- the listener is closed: its port now refuses
                f <- inboundFinding askHere Map.empty Nothing vantage (InboundTarget "127.0.0.1" gone Nothing)
                assertEqual (show f) (No, ["the connection to 127.0.0.1:" <> Text.pack (show gone) <> " was refused"]) (verdictAndEvidence f)
            ]
        ]
