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

import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import qualified SreBox.PostgresPair as Pair

tests :: TestTree
tests =
    testGroup
        "SreBox.PostgresPair"
        [ testGroup "slot names" slotNameTests
        , testGroup "parseLsn" lsnTests
        , testGroup "parseObserved" observedTests
        , testGroup "the probe script" probeTests
        , testGroup "nextStep, with the primary declared on B" stepTests
        , testGroup "nextStep, with the primary declared on A" mirrorTests
        , testGroup "stepCommand" commandTests
        ]

{- | What a step actually does to a machine. Pure, so the destructive half of
this recipe is readable without a machine to destroy.
-}
commandTests :: [TestTree]
commandTests =
    [ testCase "stopping the old primary is clean, so the standby gets the last checkpoint" $
        assertBool (script (Pair.StopMember Pair.A)) ("stop -m fast" `isInfixOf` script (Pair.StopMember Pair.A))
    , -- that same clean shutdown checkpoints, and a checkpoint recycles the
      -- WAL a later rewind reads back to where the histories parted.
      testCase "stopping a member keeps the WAL a rewind of it would need" $ do
        let s' = script (Pair.StopMember Pair.A)
        assertBool s' ("wal_keep_size" `isInfixOf` s')
        assertBool s' ("pg_reload_conf" `isInfixOf` s')
        assertBool s' (at "wal_keep_size" s' < at "stop -m fast" s')
    , testCase "and the rejoin takes that pin off again" $
        assertBool (script (Pair.Rejoin Pair.A)) ("/^wal_keep_size/d" `isInfixOf` script (Pair.Rejoin Pair.A))
    , testCase "each step runs on the machine it names" $ do
        assertEqual "" (Just "10.0.0.1") (host (Pair.StopMember Pair.A))
        assertEqual "" (Just "10.0.0.2") (host (Pair.Promote Pair.B))
    , testCase "promotion waits for the server to say it promoted" $
        assertBool (script (Pair.Promote Pair.B)) ("pg_promote(true, 60)" `isInfixOf` script (Pair.Promote Pair.B))
    , testCase "starting is a no-op on a cluster already running" $
        assertBool (script (Pair.StartMember Pair.A)) ("status" `isInfixOf` script (Pair.StartMember Pair.A))
    , testCase "rejoining rewinds onto the peer, and never re-clones" $ do
        let s = script (Pair.Rejoin Pair.A)
        assertBool s ("pg_rewind" `isInfixOf` s)
        assertBool s ("-R" `isInfixOf` s)
        assertBool s ("host=10.0.0.2" `isInfixOf` s)
        assertBool s ("user=rewinder" `isInfixOf` s)
        assertBool s (not ("pg_basebackup" `isInfixOf` s))
        assertBool s (not ("rm -rf" `isInfixOf` s))
    , -- pg_rewind finishes a crashed target's recovery by running
      -- `postgres --single -D <datadir>`, which looks for the configuration
      -- in the data directory. Debian keeps it in /etc/postgresql, so that
      -- step fails on exactly the machine a failover is about.
      testCase "a crashed member is recovered before the rewind, with a config file it can find" $ do
        let s' = script (Pair.Rejoin Pair.A)
        assertBool s' ("pg_controldata" `isInfixOf` s')
        assertBool s' ("--single" `isInfixOf` s')
        assertBool s' ("config_file=" `isInfixOf` s')
        -- and that recovery must not recycle the WAL the rewind then reads
        assertBool s' ("wal_keep_size=" `isInfixOf` s')
        -- and never a start: a server that listens is a second primary
        assertBool s' (not ("main start" `isInfixOf` takeWhile' "pg_rewind" s'))
    , testCase "a member that shut down cleanly is not recovered twice" $
        assertBool (script (Pair.Rejoin Pair.A)) ("'shut down'" `isInfixOf` script (Pair.Rejoin Pair.A))
    , -- nothing else creates it, and a standby naming a slot that is not
      -- there retries forever while looking healthy. The quoting these
      -- assertions avoid is the shell's: a slot name arrives inside SQL
      -- inside a single-quoted script, so it is spelled three ways at once.
      testCase "a rejoining member creates the slot it will stream with, and checks" $ do
        let s' = script (Pair.Rejoin Pair.A)
        assertBool s' ("CREATE_REPLICATION_SLOT salmon_pair_app_a PHYSICAL" `isInfixOf` s')
        assertBool s' ("replication=true" `isInfixOf` s')
        assertBool s' ("pg_replication_slots WHERE slot_name = " `isInfixOf` s')
        assertBool s' ("primary_slot_name = " `isInfixOf` s')
        assertBool s' (at "CREATE_REPLICATION_SLOT" s' < at "primary_slot_name = " s')
    , testCase "and drops the one it held for the peer, which nothing consumes here" $ do
        let s' = script (Pair.Rejoin Pair.A)
        assertBool s' ("pg_drop_replication_slot(slot_name)" `isInfixOf` s')
        assertBool s' ("salmon_pair_app_b" `isInfixOf` s')
        -- after the start: dropping a slot needs a server to ask
        assertBool s' (at "main start" s' < at "pg_drop_replication_slot" s')
    , testCase "a rejoined member comes back as a standby, whatever pg_rewind decided" $ do
        let s = script (Pair.Rejoin Pair.A)
        assertBool s ("standby.signal" `isInfixOf` s)
        assertBool s ("primary_conninfo" `isInfixOf` s)
        -- a slot the new primary never heard of is not a smaller problem
        -- than a conninfo pointing at the wrong machine: it is a bigger one,
        -- since the standby then retries forever instead of failing.
        assertBool s ("/^primary_slot_name/d" `isInfixOf` s)
        assertBool s ("host=10.0.0.2 port=5432 user=replicator" `isInfixOf` s)
        assertBool s ("passfile=/etc/postgresql/repl.pgpass" `isInfixOf` s)
    , testCase "the rewind password is read from its file, never carried" $
        assertBool (script (Pair.Rejoin Pair.A)) ("PGPASSFILE='/etc/postgresql/rewind.pass'" `isInfixOf` script (Pair.Rejoin Pair.A))
    , testCase "the steps that are arrivals, waits or refusals run nothing" $
        mapM_
            (\st -> assertBool (show st) (isLeft (Pair.stepCommand pair st)))
            [Pair.Done, Pair.Degraded "x", Pair.Refuse "x", Pair.AwaitCatchUp Pair.B (lsn "0/1"), Pair.AwaitStreaming Pair.A, Pair.PauseBouncers, Pair.RepointBouncers Pair.B]
    ]
  where
    script st = case Pair.stepCommand pair st of
        Right (_, s) -> s
        Left why -> error ("expected a command for " <> show st <> ": " <> Text.unpack why)
    host st = case Pair.stepCommand pair st of
        Right (m, _) -> Just (Pair.member_host m)
        Left _ -> Nothing
    isLeft (Left _) = True
    isLeft _ = False
    -- where a word first appears, so that two of them can be ordered
    at needle hay = length (takeWhile (not . isPrefixOf needle) (tails' hay))
    tails' [] = [[]]
    tails' xs@(_ : rest) = xs : tails' rest
    -- what the script does before the given word
    takeWhile' needle hay = case breakOn needle hay of (before, _) -> before
    breakOn needle hay = go "" hay
      where
        go acc [] = (reverse acc, [])
        go acc rest@(c : cs)
            | needle `isPrefixOf` rest = (reverse acc, rest)
            | otherwise = go (c : acc) cs

-------------------------------------------------------------------------------

pair :: Pair.Pair
pair =
    Pair.Pair
        { Pair.pair_name = "app"
        , Pair.pair_a = member "10.0.0.1"
        , Pair.pair_b = member "10.0.0.2"
        , Pair.pair_primary = Pair.B
        , Pair.pair_may_discard = Nothing
        , Pair.pair_repl_role = "replicator"
        , Pair.pair_repl_passfile = "/etc/postgresql/repl.pgpass"
        , Pair.pair_rewind_role = "rewinder"
        , Pair.pair_rewind_passfile = "/etc/postgresql/rewind.pass"
        , Pair.pair_ssh_known_hosts = Nothing
        , Pair.pair_catch_up_seconds = 60
        }
  where
    member host = Pair.Member "root" host "main" 5432 Nothing

lsn :: Text -> Pair.Lsn
lsn t = maybe (error ("bad lsn in test: " <> Text.unpack t)) id (Pair.parseLsn t)

-- | A standby streaming from the machine it should be streaming from.
streamingFrom :: Text -> Text -> Pair.Observed
streamingFrom host at = Pair.Standby "7000" 1 (Just host) (Just host) (lsn at) (lsn at)

{- | A standby pointed at a machine it is not connected to: a partition, a
standby still starting, a primary that was promoted a second ago. The
configuration is the same as 'streamingFrom''s -- only the connection is
missing, and only one of the two fields says so.
-}
pointedAt :: Text -> Text -> Pair.Observed
pointedAt host at = Pair.Standby "7000" 1 Nothing (Just host) (lsn at) (lsn at)

primaryAt :: Text -> Pair.Observed
primaryAt at = Pair.Primary "7000" 1 (lsn at) [(Pair.slotNameFor pair Pair.A, "reserved")]

-- | A primary whose slot for the peer has fallen off the end of the budget.
primaryWithLostSlot :: Text -> Pair.Observed
primaryWithLostSlot at = Pair.Primary "7000" 1 (lsn at) [(Pair.slotNameFor pair Pair.A, "lost")]

-- | A cluster that was shut down: its last checkpoint is the end of its WAL.
stoppedAt :: Text -> Pair.Observed
stoppedAt at = Pair.Stopped "7000" 1 (lsn at) True

{- | A cluster that stopped without shutting down. The position is the same
field, and it no longer means the same thing: there may be any amount of WAL
after it that nothing on disk records.
-}
crashedAt :: Text -> Pair.Observed
crashedAt at = Pair.Stopped "7000" 1 (lsn at) False

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

{- | A slot name is derived, never declared, so that a member that rejoins
computes the same one the member it rejoins would.
-}
slotNameTests :: [TestTree]
slotNameTests =
    [ testCase "one per side, since both of them are somebody's standby eventually" $ do
        assertEqual "" "salmon_pair_app_a" (Pair.slotNameFor pair Pair.A)
        assertEqual "" "salmon_pair_app_b" (Pair.slotNameFor pair Pair.B)
    , -- a pair is named by whoever declares it; a slot name is Postgres's to
      -- accept, and it accepts rather less.
      testCase "anything Postgres will not take becomes an underscore" $
        assertEqual
            ""
            "salmon_pair_orders_eu_west_a"
            (Pair.slotNameFor pair{Pair.pair_name = "Orders-EU.west"} Pair.A)
    , testCase "and the whole thing fits in the 63 characters Postgres allows" $
        assertBool "" (Text.length (Pair.slotNameFor pair{Pair.pair_name = Text.replicate 200 "x"} Pair.B) <= 63)
    ]

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
            (Pair.Primary "7412" 3 (lsn "0/3000028") [])
            (Pair.parseObserved "status=running\nsysid=7412\ntimeline=3\nin_recovery=f\nlsn=0/3000028\nreplayed=\nupstream=\nconfigured=\n")
    , -- the one field that is a list: a machine may hold several slots, and
      -- what matters is what each one's WAL is still worth.
      testCase "a primary, with the slots it holds" $
        assertEqual
            ""
            (Pair.Primary "7412" 3 (lsn "0/3000028") [("salmon_pair_app_a", "lost"), ("other", "reserved")])
            (Pair.parseObserved "status=running\nsysid=7412\ntimeline=3\nin_recovery=f\nlsn=0/3000028\nreplayed=\nupstream=\nconfigured=\nslot=salmon_pair_app_a:lost\nslot=other:reserved\n")
    , testCase "a standby, with where it streams from" $
        assertEqual
            ""
            (Pair.Standby "7412" 3 (Just "10.0.0.1") (Just "10.0.0.1") (lsn "0/4000000") (lsn "0/3FFFFFF"))
            (Pair.parseObserved "status=running\nsysid=7412\ntimeline=3\nin_recovery=t\nlsn=0/4000000\nreplayed=0/3FFFFFF\nupstream=10.0.0.1\nconfigured=10.0.0.1\n")
    , -- the partition's shape: told where to stream from, connected to nobody
      testCase "a standby that is pointed somewhere and connected to nobody" $
        assertEqual
            ""
            (Pair.Standby "7412" 3 Nothing (Just "10.0.0.1") (lsn "0/4000000") (lsn "0/4000000"))
            (Pair.parseObserved "status=running\nsysid=7412\ntimeline=3\nin_recovery=t\nlsn=0/4000000\nreplayed=\nupstream=\nconfigured=10.0.0.1\n")
    , testCase "a standby streaming from nowhere" $
        assertEqual
            ""
            (Pair.Standby "7412" 3 Nothing Nothing (lsn "0/4000000") (lsn "0/4000000"))
            (Pair.parseObserved "status=running\nsysid=7412\ntimeline=3\nin_recovery=t\nlsn=0/4000000\nreplayed=\nupstream=\nconfigured=\n")
    , testCase "a stopped cluster, read off pg_controldata" $
        assertEqual
            ""
            (Pair.Stopped "7412" 3 (lsn "0/2000060") True)
            (Pair.parseObserved "status=stopped\nsysid=7412\nstate=shut down\ncheckpoint=0/2000060\ntimeline=3\nmin_recovery=0/0\n")
    , testCase "a stopped standby replayed past its last checkpoint" $
        assertEqual
            ""
            (Pair.Stopped "7412" 3 (lsn "0/4000000") True)
            (Pair.parseObserved "status=stopped\nsysid=7412\nstate=shut down in recovery\ncheckpoint=0/2000060\ntimeline=3\nmin_recovery=0/4000000\n")
    , -- "in production" on a cluster that is not running is a crash.
      testCase "a cluster that stopped without shutting down" $
        assertEqual
            ""
            (Pair.Stopped "7412" 3 (lsn "0/2000060") False)
            (Pair.parseObserved "status=stopped\nsysid=7412\nstate=in production\ncheckpoint=0/2000060\ntimeline=3\nmin_recovery=0/0\n")
    , -- an old pg_controldata, a translated one, a field that moved: none of
      -- them is a reason to believe a cluster shut down cleanly.
      testCase "a cluster state nobody recognises is not a clean stop" $
        assertEqual
            ""
            (Pair.Stopped "7412" 3 (lsn "0/2000060") False)
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
        mapM_ (\k -> assertBool (k <> " missing from the probe") ((k <> "=") `isInfixOf` script)) ["sysid", "timeline", "in_recovery", "lsn", "replayed", "upstream", "configured", "slot"]
    , testCase "a stopped cluster reports what the promotion turns on" $
        mapM_ (\k -> assertBool (k <> " missing from the probe") ((k <> "=") `isInfixOf` script)) ["checkpoint", "state", "min_recovery"]
    , -- the standby whose primary is gone has received nothing this
      -- session, and that is the one whose position decides a failover.
      testCase "a standby's position falls back on what it replayed" $ do
        assertBool script ("GREATEST" `isInfixOf` script)
        assertBool script ("pg_last_wal_replay_lsn" `isInfixOf` script)
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
    , -- a partition, and the single most tempting moment to do damage: the
      -- peer looks exactly like a standby that belongs to somebody else.
      testCase "the peer is pointed at us but not streaming: wait, do not rewind it" $
        assertEqual "" (Pair.AwaitStreaming Pair.A) (step (pointedAt "10.0.0.2" "0/5") (primaryAt "0/5") settled)
    , -- a lost slot is WAL that has been recycled: there is nothing left to
      -- stream, so waiting is not a plan and rewinding is not a fix.
      testCase "the peer's slot is lost: say so, and wait for nobody" $
        assertBool "" (degraded (step (pointedAt "10.0.0.2" "0/5") (primaryWithLostSlot "0/5") settled))
    , testCase "the peer is stopped and its slot is lost: do not rewind it either" $
        assertBool "" (degraded (step (stoppedAt "0/4") (primaryWithLostSlot "0/5") settled))
    , testCase "somebody else's lost slot is not this pair's business" $
        assertEqual
            ""
            (Pair.Rejoin Pair.A)
            (step (stoppedAt "0/4") (Pair.Primary "7000" 1 (lsn "0/5") [("somebody_elses", "lost")]) settled)
    , testCase "the peer streams from the wrong machine: rejoin it" $
        assertEqual "" (Pair.Rejoin Pair.A) (step (streamingFrom "10.0.0.9" "0/5") (primaryAt "0/5") settled)
    , testCase "the peer streams from nobody: rejoin it" $
        assertEqual "" (Pair.Rejoin Pair.A) (step (Pair.Standby "7000" 1 Nothing Nothing (lsn "0/5") (lsn "0/5")) (primaryAt "0/5") settled)
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
    , -- a crash is the case the checkpoint comparison cannot see: the peer
      -- may have written and acknowledged anything at all after it.
      testCase "failover: the peer crashed, so its checkpoint proves nothing" $
        assertBool "" (refuses (step (crashedAt "0/5000060") (streamingFrom "10.0.0.1" "0/5000060") paused))
    , testCase "failover: the peer crashed, and its writes are declared expendable" $
        assertEqual
            ""
            (Pair.Promote Pair.B)
            (Pair.nextStep pair{Pair.pair_may_discard = Just Pair.A} (crashedAt "0/5000060") (streamingFrom "10.0.0.1" "0/5000060") paused)
    , -- with the flag, waiting for a machine that will send nothing more is
      -- only a slower way to reach the same place.
      testCase "failover: expendable writes are not waited for" $
        assertEqual
            ""
            (Pair.Promote Pair.B)
            (Pair.nextStep pair{Pair.pair_may_discard = Just Pair.A} (stoppedAt "0/5000060") (streamingFrom "10.0.0.1" "0/4000000") paused)
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
            (refuses (step (Pair.Standby "7000" 1 (Just "10.0.0.2") (Just "10.0.0.2") (lsn "0/5") (lsn "0/5")) (Pair.Primary "9999" 1 (lsn "0/5") []) settled))
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
