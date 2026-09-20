{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "SreBox.PostgresPair": what the machines are
(@parseObserved@) and what to do about it (@nextStep@).

The whole design rests on this being decidable from an observation alone --
no progress file, so a switchover killed half-way is finished by the next
pass. That makes the table below the specification: every row is a state two
machines can genuinely be found in, including the ones where the answer is
to refuse.

The refusals are the cases worth staring at. Two primaries, a peer that
cannot be reached, a standby that is ahead of the machine we were told to
promote: each is a state where acting would lose somebody's writes, and each
becomes /allowed/ only when the operator has said so through
'PostgresPair.pair_may_discard'.
-}
module Test.PostgresPairSpec (tests) where

import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified SreBox.PostgresPair as Pair

tests :: TestTree
tests =
    testGroup
        "SreBox.PostgresPair"
        [ testGroup "parseLsn" lsnTests
        , testGroup "parseObserved" observedTests
        , testGroup "the probe script" probeTests
        , testGroup "nextStep, with the primary declared on B" stepTests
        , testGroup "nextStep, with the primary declared on A" mirrorTests
        ]

-------------------------------------------------------------------------------

pair :: Pair.Pair
pair =
    Pair.Pair
        { Pair.pair_name = "app"
        , Pair.pair_a = member "10.0.0.1"
        , Pair.pair_b = member "10.0.0.2"
        , Pair.pair_primary = Pair.B
        , Pair.pair_may_discard = Nothing
        }
  where
    member host = Pair.Member "root" host "main" 5432

lsn :: Text -> Pair.Lsn
lsn t = maybe (error ("bad lsn in test: " <> Text.unpack t)) id (Pair.parseLsn t)

-- | A standby streaming from the machine it should be streaming from.
streamingFrom :: Text -> Text -> Pair.Observed
streamingFrom host at = Pair.Standby "7000" 1 (Just host) (lsn at) (lsn at)

primaryAt :: Text -> Pair.Observed
primaryAt at = Pair.Primary "7000" 1 (lsn at)

stoppedAt :: Text -> Pair.Observed
stoppedAt at = Pair.Stopped "7000" 1 (lsn at)

-- | Bouncers doing what they should: pointed at B, nobody held.
settled :: [Pair.BouncerState]
settled = [Pair.BouncerState "bouncer-1" (Just "10.0.0.2") False]

paused :: [Pair.BouncerState]
paused = [Pair.BouncerState "bouncer-1" (Just "10.0.0.1") True]

atOldPrimary :: [Pair.BouncerState]
atOldPrimary = [Pair.BouncerState "bouncer-1" (Just "10.0.0.1") False]

step :: Pair.Observed -> Pair.Observed -> [Pair.BouncerState] -> Pair.Step
step = Pair.nextStep pair

-------------------------------------------------------------------------------

lsnTests :: [TestTree]
lsnTests =
    [ testCase "positions are compared as numbers, not as text" $
        assertBool "" (lsn "0/9000000" < lsn "1/1000000")
    , testCase "the low half is hexadecimal" $
        assertBool "" (lsn "0/A000000" > lsn "0/9FFFFFF")
    , testCase "equal positions compare equal" $
        assertEqual "" (lsn "2/3000028") (lsn "2/3000028")
    , testCase "anything else is not a position" $ do
        assertEqual "" Nothing (Pair.parseLsn "")
        assertEqual "" Nothing (Pair.parseLsn "0")
        assertEqual "" Nothing (Pair.parseLsn "0/zzz")
    ]

observedTests :: [TestTree]
observedTests =
    [ testCase "a primary" $
        assertEqual
            ""
            (Pair.Primary "7412" 3 (lsn "0/3000028"))
            (Pair.parseObserved "status=running\nsysid=7412\ntimeline=3\nin_recovery=f\nlsn=0/3000028\nreplayed=\nupstream=\n")
    , testCase "a standby, with where it streams from" $
        assertEqual
            ""
            (Pair.Standby "7412" 3 (Just "10.0.0.1") (lsn "0/4000000") (lsn "0/3FFFFFF"))
            (Pair.parseObserved "status=running\nsysid=7412\ntimeline=3\nin_recovery=t\nlsn=0/4000000\nreplayed=0/3FFFFFF\nupstream=10.0.0.1\n")
    , testCase "a standby streaming from nowhere" $
        assertEqual
            ""
            (Pair.Standby "7412" 3 Nothing (lsn "0/4000000") (lsn "0/4000000"))
            (Pair.parseObserved "status=running\nsysid=7412\ntimeline=3\nin_recovery=t\nlsn=0/4000000\nreplayed=\nupstream=\n")
    , testCase "a stopped cluster, read off pg_controldata" $
        assertEqual
            ""
            (Pair.Stopped "7412" 3 (lsn "0/2000060"))
            (Pair.parseObserved "status=stopped\nsysid=7412\ncheckpoint=0/2000060\ntimeline=3\n")
    , testCase "no cluster there at all" $
        assertEqual "" Pair.Absent (Pair.parseObserved "status=absent\n")
    , -- half an answer is not an answer: acting on it is acting on a guess.
      testCase "a truncated answer is not a state" $ do
        assertBool "" (unreachable (Pair.parseObserved "status=running\nsysid=7412\n"))
        assertBool "" (unreachable (Pair.parseObserved "status=stopped\nsysid=7412\n"))
        assertBool "" (unreachable (Pair.parseObserved ""))
        assertBool "" (unreachable (Pair.parseObserved "ssh: connect to host 10.0.0.1 port 22: No route to host"))
    ]
  where
    unreachable (Pair.Unreachable _) = True
    unreachable _ = False

probeTests :: [TestTree]
probeTests =
    [ testCase "asks a running cluster, reads a stopped one off disk" $ do
        assertBool script ("pg_is_in_recovery()" `isInfixOf` script)
        assertBool script ("pg_controldata" `isInfixOf` script)
    , testCase "a running cluster reports every field the parser needs" $
        mapM_ (\k -> assertBool (k <> " missing from the probe") ((k <> "=") `isInfixOf` script)) ["sysid", "timeline", "in_recovery", "lsn", "replayed", "upstream"]
    , testCase "a stopped cluster reports the checkpoint the promotion turns on" $
        assertBool script ("checkpoint=" `isInfixOf` script)
    , testCase "it reads, and never writes" $
        mapM_ (\verb -> assertBool (verb <> " has no business in a probe") (not (verb `isInfixOf` script))) ["rm ", "promote", "pg_rewind", "DROP", "pg_ctlcluster \"$version\" main stop"]
    ]
  where
    script = Pair.probeScript (Pair.pair_b pair)

-------------------------------------------------------------------------------

stepTests :: [TestTree]
stepTests =
    [ testCase "B primary, A streaming from it, clients on B: nothing to do" $
        assertEqual "" Pair.Done (step (streamingFrom "10.0.0.2" "0/5000000") (primaryAt "0/5000000") settled)
    , testCase "arrived, but the clients are still held: let them go first" $
        assertEqual "" (Pair.RepointBouncers Pair.B) (step (streamingFrom "10.0.0.2" "0/5") (primaryAt "0/5") paused)
    , testCase "arrived, but the clients are still sent to the old primary" $
        assertEqual "" (Pair.RepointBouncers Pair.B) (step (streamingFrom "10.0.0.2" "0/5") (primaryAt "0/5") atOldPrimary)
    , testCase "the peer streams from the wrong machine: rejoin it" $
        assertEqual "" (Pair.Rejoin Pair.A) (step (streamingFrom "10.0.0.9" "0/5") (primaryAt "0/5") settled)
    , testCase "the peer streams from nobody: rejoin it" $
        assertEqual "" (Pair.Rejoin Pair.A) (step (Pair.Standby "7000" 1 Nothing (lsn "0/5") (lsn "0/5")) (primaryAt "0/5") settled)
    , testCase "the peer is stopped: rejoin it" $
        assertEqual "" (Pair.Rejoin Pair.A) (step (stoppedAt "0/4") (primaryAt "0/5") settled)
    , testCase "the peer is unreachable: serve, and say the pair is one machine short" $
        assertBool "" (degraded (step (Pair.Unreachable "no route to host") (primaryAt "0/5") settled))
    , testCase "the peer has no cluster: seeding is not this node's job" $
        assertBool "" (degraded (step Pair.Absent (primaryAt "0/5") settled))
    , testCase "two primaries, nothing said: refuse" $
        assertBool "" (refuses (step (primaryAt "0/6") (primaryAt "0/5") settled))
    , testCase "two primaries, and the operator accepted losing A's writes" $
        assertEqual
            ""
            (Pair.StopMember Pair.A)
            (Pair.nextStep pair{Pair.pair_may_discard = Just Pair.A} (primaryAt "0/6") (primaryAt "0/5") settled)
    , -- the ordinary switchover, step by step
      testCase "switchover: hold the clients before stopping anything" $
        assertEqual "" Pair.PauseBouncers (step (primaryAt "0/5") (streamingFrom "10.0.0.1" "0/5") atOldPrimary)
    , testCase "switchover: clients held, so stop the old primary cleanly" $
        assertEqual "" (Pair.StopMember Pair.A) (step (primaryAt "0/5") (streamingFrom "10.0.0.1" "0/5") paused)
    , testCase "switchover: the old primary stopped and the new one has its last checkpoint" $
        assertEqual "" (Pair.Promote Pair.B) (step (stoppedAt "0/5000060") (streamingFrom "10.0.0.1" "0/5000060") paused)
    , testCase "switchover: not caught up yet, so wait rather than lose the tail" $
        assertEqual
            ""
            (Pair.AwaitCatchUp Pair.B (lsn "0/5000060"))
            (step (stoppedAt "0/5000060") (streamingFrom "10.0.0.1" "0/4000000") paused)
    , testCase "failover: the peer cannot be confirmed stopped, so refuse" $
        assertBool "" (refuses (step (Pair.Unreachable "timed out") (streamingFrom "10.0.0.1" "0/5") atOldPrimary))
    , testCase "failover: with the writes declared expendable, hold the clients first" $
        assertEqual
            ""
            Pair.PauseBouncers
            (Pair.nextStep pair{Pair.pair_may_discard = Just Pair.A} (Pair.Unreachable "timed out") (streamingFrom "10.0.0.1" "0/5") atOldPrimary)
    , testCase "failover: clients held, promote" $
        assertEqual
            ""
            (Pair.Promote Pair.B)
            (Pair.nextStep pair{Pair.pair_may_discard = Just Pair.A} (Pair.Unreachable "timed out") (streamingFrom "10.0.0.1" "0/5") paused)
    , testCase "two standbys: promote the declared one, it is not behind" $
        assertEqual "" (Pair.Promote Pair.B) (step (streamingFrom "10.0.0.9" "0/4") (streamingFrom "10.0.0.9" "0/5") paused)
    , testCase "two standbys, and the peer is ahead: refuse rather than lose its tail" $
        assertBool "" (refuses (step (streamingFrom "10.0.0.9" "0/6") (streamingFrom "10.0.0.9" "0/5") paused))
    , testCase "both stopped: start the declared primary first" $
        assertEqual "" (Pair.StartMember Pair.B) (step (stoppedAt "0/5") (stoppedAt "0/5") paused)
    , testCase "the declared primary is stopped while the peer serves: start it" $
        assertEqual "" (Pair.StartMember Pair.B) (step (primaryAt "0/5") (stoppedAt "0/4") atOldPrimary)
    , testCase "the declared primary is unreachable: refuse, whatever the peer is" $
        assertBool "" (refuses (step (primaryAt "0/5") (Pair.Unreachable "timed out") atOldPrimary))
    , testCase "the declared primary has no cluster: refuse" $
        assertBool "" (refuses (step (primaryAt "0/5") Pair.Absent atOldPrimary))
    , -- an identifier apart is two clusters, whatever the names say, and
      -- every step below would then be applied to a stranger's data.
      testCase "different clusters: refuse before anything else" $
        assertBool
            ""
            (refuses (step (Pair.Standby "7000" 1 (Just "10.0.0.2") (lsn "0/5") (lsn "0/5")) (Pair.Primary "9999" 1 (lsn "0/5")) settled))
    , testCase "no bouncers declared: their state cannot hold a pass back" $
        assertEqual "" Pair.Done (step (streamingFrom "10.0.0.2" "0/5") (primaryAt "0/5") [])
    ]

{- | The same pair with the declaration the other way round. The table is
written in terms of "the declared primary" and "the peer", and these say so:
a rule that reached for @pair_a@ by accident would show up here.
-}
mirrorTests :: [TestTree]
mirrorTests =
    [ testCase "A primary, B streaming from it, clients on A: nothing to do" $
        assertEqual "" Pair.Done (mirror (primaryAt "0/5") (streamingFrom "10.0.0.1" "0/5") [Pair.BouncerState "b" (Just "10.0.0.1") False])
    , testCase "switchover the other way: stop B once the clients are held" $
        assertEqual "" (Pair.StopMember Pair.B) (mirror (streamingFrom "10.0.0.2" "0/5") (primaryAt "0/5") paused)
    , testCase "promote A once B has stopped and A has its checkpoint" $
        assertEqual "" (Pair.Promote Pair.A) (mirror (streamingFrom "10.0.0.2" "0/5000060") (stoppedAt "0/5000060") paused)
    , testCase "clients are sent to A now, not to B" $
        assertEqual "" (Pair.RepointBouncers Pair.A) (mirror (primaryAt "0/5") (streamingFrom "10.0.0.1" "0/5") settled)
    ]
  where
    mirror = Pair.nextStep pair{Pair.pair_primary = Pair.A}

refuses :: Pair.Step -> Bool
refuses (Pair.Refuse _) = True
refuses _ = False

degraded :: Pair.Step -> Bool
degraded (Pair.Degraded _) = True
degraded _ = False
