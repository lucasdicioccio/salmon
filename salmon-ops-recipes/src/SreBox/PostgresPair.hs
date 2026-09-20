{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A primary and a streaming standby on two machines, with the primary's
location declared rather than discovered.

This is the pure half: what the two machines are observed to be, and what to
do next about it. The node that does the observing and the doing comes on
top of these; see @specs\/pg-switchover.md@ for the whole design, and
@specs\/pg-patroni.md@ for the other end of the range, where a consensus
system decides instead of an operator.

= The primary's location is a declaration

'pair_primary' says which side should be the primary. Nothing here decides
that a machine has died: salmon has no consensus, and promoting on a hunch
is how two primaries happen. An operator changes the declaration, and a pass
converges to it -- which is a /switchover/ when both machines are there, and
a failover only when the operator also says, through 'pair_may_discard',
which side's un-replicated writes they accept losing.

= Why the state is re-derived, never remembered

'nextStep' takes what the machines are /now/ and returns one step. Its
caller loops: observe, step, act, observe again. A switchover interrupted
half-way -- the controller killed between stopping the old primary and
promoting the new one -- leaves a state that the next pass recognises and
finishes, because there is no progress file that could disagree with the
machines. That is the property worth protecting when changing this module:
every step must be decidable from an observation alone.
-}
module SreBox.PostgresPair (
    -- * Declaring a pair
    Side (..),
    other,
    Member (..),
    Pair (..),
    memberOn,
    slotNameFor,

    -- * What the machines are
    Lsn (..),
    parseLsn,
    Observed (..),
    sysidOf,
    probeScript,
    parseObserved,
    BouncerState (..),

    -- * What to do about it
    Step (..),
    nextStep,
    stepCommand,

    -- * Doing it
    Report (..),
    pairRole,
    verdict,
    observe,
    decide,
    converge,
    convergeUpTo,
    stepBudget,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import GHC.Generics (Generic)
import Numeric (readHex)
import System.Exit (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter

-------------------------------------------------------------------------------
-- Declaring a pair

data Side = A | B
    deriving (Eq, Show, Generic)

instance FromJSON Side
instance ToJSON Side

other :: Side -> Side
other A = B
other B = A

{- | One machine of the pair.

@member_ssh_user@ and @member_host@ are how the /controller/ reaches it;
@member_host@ is also how the peer and the bouncers do, so it has to be an
address both can use.
-}
data Member
    = Member
    { member_ssh_user :: Text
    , member_host :: Postgres.Host
    , member_cluster :: Postgres.ClusterName
    , member_port :: Postgres.Port
    , member_ssh_identity :: Maybe FilePath
    -- ^ a key to authenticate with, for a machine that does not answer to
    -- whatever the controller offers by default. Per member rather than per
    -- pair: two machines need not have been given the same key.
    }
    deriving (Eq, Show, Generic)

instance FromJSON Member
instance ToJSON Member

data Pair
    = Pair
    { pair_name :: Text
    , pair_a :: Member
    , pair_b :: Member
    , pair_primary :: Side
    -- ^ where the primary should be. An operator's declaration, not an observation.
    , pair_repl_role :: Postgres.RoleName
    -- ^ the @REPLICATION@ role a rejoined member streams as.
    , pair_repl_passfile :: FilePath
    -- ^ that role's password, in @.pgpass@ format, on each member: it goes
    -- into @primary_conninfo@ as @passfile=@, which is how a standby
    -- authenticates without the password being written into a config file.
    , pair_rewind_role :: Postgres.RoleName
    -- ^ the role @pg_rewind@ connects as when an old primary rejoins. Not
    -- the replication role: rewind reads files through ordinary function
    -- calls, so it needs a plain login with @EXECUTE@ on @pg_ls_dir@,
    -- @pg_stat_file@ and the two @pg_read_binary_file@s.
    , pair_rewind_passfile :: FilePath
    -- ^ that role's password, in @.pgpass@ format, on each member. A path,
    -- never the password: these commands are shell scripts, and a script is
    -- visible in @ps@ and printed by every report. (@.pgpass@ here, unlike
    -- 'Postgres.standby_repl_passfile''s bare password, because
    -- @primary_conninfo@ can only take this form.)
    , pair_ssh_known_hosts :: Maybe FilePath
    , pair_catch_up_seconds :: Int
    -- ^ how long a standby may take to replay the old primary's last
    -- checkpoint before the switchover gives up and says so.
    , pair_may_discard :: Maybe Side
    -- ^ "I accept losing writes on this side that the other does not have".
    --
    -- The one escape hatch, and its name is what it costs. It covers the two
    -- cases salmon cannot judge for itself: promoting although the other
    -- machine cannot be confirmed stopped (a failover), and rewinding one of
    -- two primaries onto the other's history (a split brain). It must name
    -- the side that is /not/ 'pair_primary'; a directive saying otherwise is
    -- contradictory and is rejected where it is built.
    }
    deriving (Eq, Show, Generic)

instance FromJSON Pair
instance ToJSON Pair

memberOn :: Pair -> Side -> Member
memberOn pair A = pair.pair_a
memberOn pair B = pair.pair_b

{- | The physical replication slot a member streams with, which lives on the
/other/ member.

One per side rather than one per pair, because both of them are somebody's
standby eventually and a slot is named on the machine that holds it. The name
is derived rather than declared so that nothing has to remember it across a
switchover: a member that rejoins computes the same name the member it
rejoins would, and a slot nobody can name is a slot nobody can drop.

Postgres allows a slot name of at most 63 lower-case letters, digits and
underscores, so anything else in the pair's name becomes an underscore.
-}
slotNameFor :: Pair -> Side -> Text
slotNameFor pair side =
    Text.take 63 ("salmon_pair_" <> Text.map keep (Text.toLower pair.pair_name) <> side')
  where
    side' = case side of A -> "_a"; B -> "_b"
    keep c
        | c >= 'a' && c <= 'z' = c
        | c >= '0' && c <= '9' = c
        | otherwise = '_' 

-------------------------------------------------------------------------------
-- What the machines are

{- | A write-ahead log position, as Postgres prints it (@0\/3000028@), kept
as the number it stands for so that two of them can be compared.
-}
newtype Lsn = Lsn Integer
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Lsn
instance ToJSON Lsn

parseLsn :: Text -> Maybe Lsn
parseLsn txt =
    case Text.splitOn "/" (Text.strip txt) of
        [hi, lo] -> Lsn <$> ((\h l -> h * 0x100000000 + l) <$> hex hi <*> hex lo)
        _ -> Nothing
  where
    hex :: Text -> Maybe Integer
    hex t = case readHex (Text.unpack t) of
        [(n, "")] -> Just n
        _ -> Nothing

{- | What one machine turned out to be.

'Unreachable' is deliberately one constructor for "ssh could not get there"
and "what came back made no sense": both mean the same thing to 'nextStep',
which is that this machine's state is not known, and neither is a reason to
touch the other one.
-}
data Observed
    = Unreachable Text
    | -- | no such cluster on that machine at all
      Absent
    | Stopped
        { o_sysid :: Text
        , o_timeline :: Int
        , o_checkpoint :: Lsn
        -- ^ the furthest position @pg_controldata@ can prove this cluster
        -- reached: its latest checkpoint, or its minimum recovery point if
        -- that is further (which it is for a standby that was stopped).
        , o_clean :: Bool
        -- ^ whether it stopped cleanly, from @pg_controldata@'s cluster
        -- state. This is what says whether 'o_checkpoint' is the whole
        -- story: a cluster shut down cleanly ends with a checkpoint, so
        -- there is no WAL after it, while a crashed one may have written
        -- any amount past its last checkpoint and pg_control says nothing
        -- about it.
        }
    | Primary
        { o_sysid :: Text
        , o_timeline :: Int
        , o_lsn :: Lsn
        , o_slots :: [(Text, Text)]
        -- ^ the replication slots this machine holds, and what each one's
        -- WAL is worth (@pg_replication_slots.wal_status@). Only a primary
        -- is asked, because only a primary holds the slot its standby
        -- streams with -- and @lost@ on that slot is the one observation
        -- that says a standby can never catch up again.
        }
    | Standby
        { o_sysid :: Text
        , o_timeline :: Int
        , o_upstream :: Maybe Postgres.Host
        -- ^ where it /is/ streaming from, which is empty the moment the
        -- connection drops.
        , o_configured :: Maybe Postgres.Host
        -- ^ where it is /told/ to stream from, from @primary_conninfo@. A
        -- standby keeps this through a partition, which is what makes "it
        -- cannot reach its primary right now" distinguishable from "it was
        -- never pointed here at all" -- states that look identical in
        -- 'o_upstream' and call for opposite actions.
        , o_received :: Lsn
        -- ^ the furthest position it holds: what it received, or what it
        -- replayed from its own WAL if it has received nothing since it
        -- started.
        , o_replayed :: Lsn
        }
    deriving (Eq, Show)

-- | The system identifier, for the machines that have one.
sysidOf :: Observed -> Maybe Text
sysidOf (Stopped s _ _ _) = Just s
sysidOf (Primary s _ _ _) = Just s
sysidOf (Standby s _ _ _ _ _) = Just s
sysidOf _ = Nothing

{- | What to run on a member to produce what 'parseObserved' reads: one
@key=value@ per line.

A running cluster is asked; a stopped one is read off @pg_controldata@,
which needs no server. The two paths report different keys on purpose --
there is no "current LSN" for a cluster that is not running, and inventing
one would be inventing the very fact a switchover turns on.
-}
probeScript :: Member -> String
probeScript m =
    unlines
        [ "set -e"
        , "version=$(pg_lsclusters --no-header | awk -v c=" <> shellQuote cluster <> " '$2==c {print $1}' | head -n1)"
        , "if [ -z \"$version\" ]; then echo status=absent; exit 0; fi"
        , "datadir=/var/lib/postgresql/$version/" <> cluster
        , "pg_controldata=/usr/lib/postgresql/$version/bin/pg_controldata"
        , "[ -x \"$pg_controldata\" ] || pg_controldata=pg_controldata"
        , "if pg_ctlcluster \"$version\" " <> cluster <> " status >/dev/null 2>&1; then"
        , "  echo status=running"
        , "  sudo -u postgres psql -p " <> show m.member_port <> " -tAX -d postgres -c " <> shellQuote runningQuery
        , "else"
        , "  echo status=stopped"
        , "  \"$pg_controldata\" -D \"$datadir\" " <> controldataFilter
        , "fi"
        ]
  where
    cluster = Text.unpack m.member_cluster
    runningQuery =
        unwords
            [ "SELECT 'sysid=' || system_identifier FROM pg_control_system();"
            , "SELECT 'timeline=' || timeline_id FROM pg_control_checkpoint();"
            , "SELECT 'in_recovery=' || CASE WHEN pg_is_in_recovery() THEN 't' ELSE 'f' END;"
            , -- a standby that has just restarted and cannot reach its
              -- primary -- which is the failover case, and so exactly when
              -- this has to work -- has received nothing in this session, and
              -- pg_last_wal_receive_lsn() is then NULL rather than the
              -- position it crash-recovered to. Asking for the furthest of
              -- the two is the same answer whenever both exist, since a
              -- standby cannot replay what it has not received.
              "SELECT 'lsn=' || CASE WHEN pg_is_in_recovery()"
                <> " THEN GREATEST(coalesce(pg_last_wal_receive_lsn(), '0/0'::pg_lsn), coalesce(pg_last_wal_replay_lsn(), '0/0'::pg_lsn))"
                <> " ELSE pg_current_wal_lsn() END;"
            , "SELECT 'replayed=' || coalesce(pg_last_wal_replay_lsn()::text, '');"
            , "SELECT 'upstream=' || coalesce((SELECT sender_host FROM pg_stat_wal_receiver LIMIT 1), '');"
            , -- where it is *told* to stream from, which survives the
              -- connection dropping. Read from pg_settings rather than SHOW
              -- so that a primary, which has no such setting, reports an
              -- empty value rather than failing the whole probe.
              "SELECT 'configured=' || coalesce((SELECT substring(setting from 'host=([^ ]+)') FROM pg_settings WHERE name = 'primary_conninfo'), '');"
            , "SELECT 'slot=' || slot_name || ':' || coalesce(wal_status, '') FROM pg_replication_slots;"
            ]
    controldataFilter =
        unwords
            [ "| sed -n"
            , "-e 's/^Database system identifier: *\\(.*\\)$/sysid=\\1/p'"
            , "-e 's/^Database cluster state: *\\(.*\\)$/state=\\1/p'"
            , "-e 's/^Latest checkpoint location: *\\(.*\\)$/checkpoint=\\1/p'"
            , "-e \"s/^Latest checkpoint's TimeLineID: *\\(.*\\)$/timeline=\\1/p\""
            , -- 0/0 on a primary; on a standby that was stopped, this is how
              -- far it actually replayed, which is past its last checkpoint.
              "-e 's/^Minimum recovery ending location: *\\(.*\\)$/min_recovery=\\1/p'"
            ]
    shellQuote :: String -> String
    shellQuote s = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) s <> "'"

-- | Reads 'probeScript''s output. Anything missing or malformed is 'Unreachable'.
parseObserved :: Text -> Observed
parseObserved out =
    case field "status" of
        Just "absent" -> Absent
        Just "stopped" -> stopped
        Just "running" -> running
        _ -> Unreachable "the probe reported no status"
  where
    fields :: [(Text, Text)]
    fields = [(k, Text.drop 1 v) | l <- Text.lines out, let (k, v) = Text.breakOn "=" (Text.strip l), not (Text.null v)]

    field :: Text -> Maybe Text
    field k = lookup k fields

    lsn :: Text -> Maybe Lsn
    lsn k = field k >>= parseLsn

    timeline :: Maybe Int
    timeline = field "timeline" >>= \t -> case reads (Text.unpack t) of
        [(n, "")] -> Just n
        _ -> Nothing

    stopped = case (field "sysid", timeline, lsn "checkpoint") of
        (Just s, Just tl, Just cp) -> Stopped s tl (max cp (fromMaybe cp (lsn "min_recovery"))) cleanly
        _ -> Unreachable "a stopped cluster reported no control data"

    -- pg_controldata's cluster state, of which exactly two values mean the
    -- cluster was stopped rather than lost: a primary shut down, and a
    -- standby shut down while in recovery. "in production" on a cluster that
    -- is not running is what a crash reads as, and an unrecognised value is
    -- treated the same way -- the direction that refuses rather than the one
    -- that promotes.
    cleanly = field "state" `elem` [Just "shut down", Just "shut down in recovery"]

    running = case (field "sysid", timeline, field "in_recovery") of
        (Just s, Just tl, Just "f") -> case lsn "lsn" of
            Just l -> Primary s tl l slots
            Nothing -> Unreachable "a primary reported no write position"
        (Just s, Just tl, Just "t") -> case lsn "lsn" of
            Just recv -> Standby s tl upstream configured recv (fromMaybe recv (lsn "replayed"))
            Nothing -> Unreachable "a standby reported no receive position"
        _ -> Unreachable "a running cluster reported no identity"

    upstream = host "upstream"
    configured = host "configured"

    -- one line per slot, so this is the one field that is a list
    slots = [(Text.takeWhile (/= ':') v, Text.drop 1 (Text.dropWhile (/= ':') v)) | (k, v) <- fields, k == "slot"]

    host k = case field k of
        Just u | not (Text.null u) -> Just u
        _ -> Nothing

{- | One pgbouncer in front of the pair, as it was found: which member it
sends clients to, and whether it is holding them.
-}
data BouncerState
    = BouncerState
    { bouncer_name :: Text
    , bouncer_upstream :: Maybe Postgres.Host
    , bouncer_paused :: Bool
    }
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- What to do about it

data Step
    = -- | the declaration holds and the pair is redundant
      Done
    | -- | the declaration holds, but the pair is one machine short
      Degraded Text
    | PauseBouncers
    | -- | a clean, fast shutdown, so the standby gets the final checkpoint
      StopMember Side
    | -- | this side must have received past that position before it is promoted
      AwaitCatchUp Side Lsn
    | -- | this side is pointed at the primary but is not streaming from it yet
      AwaitStreaming Side
    | Promote Side
    | -- | @pg_rewind@ onto the primary's history, then start, as a standby
      Rejoin Side
    | StartMember Side
    | -- | rewrite each bouncer's upstream, reload, and let clients go
      RepointBouncers Side
    | -- | nothing safe to do from here; the text says why
      Refuse Text
    deriving (Eq, Show)

{- | The one step to take next, given both machines and the bouncers.

Read in order, first match wins. Two rules run before everything else
because they are about whether this is even the pair it claims to be, and
whether anybody is being served at all.
-}
nextStep :: Pair -> Observed -> Observed -> [BouncerState] -> Step
nextStep pair obsA obsB bouncers
    | mismatchedCluster = Refuse "the two machines hold different clusters"
    | otherwise = case (p, q) of
        -- The declared primary is the primary. Everything from here is about
        -- the peer, and about where clients are being sent.
        (Primary{}, _) | not bouncersReady -> RepointBouncers primarySide
        (Primary{}, Standby _ _ up conf _ _)
            -- the peer must be streaming from the primary, which is the
            -- *other* machine's address: a standby pointed anywhere else is
            -- not part of this pair, however healthy it looks.
            | up == Just primary.member_host -> Done
            -- pointed here, but not connected: a partition, a primary that
            -- has just been promoted and not yet been found, a standby still
            -- starting up. Rewinding a standby that is already this pair's
            -- would stop it and then fail, since whatever keeps it from
            -- streaming keeps pg_rewind from reading too -- a partition would
            -- take the standby down rather than ride it out.
            -- ... unless the primary's own slot for it says the waiting is
            -- over: a lost slot is WAL that has been recycled, so there is
            -- nothing left for this standby to stream and no amount of
            -- patience produces it.
            | peerSlotLost -> Degraded lostSlotWhy
            | conf == Just primary.member_host -> AwaitStreaming peerSide
            | otherwise -> Rejoin peerSide
        (Primary{}, Stopped{})
            -- the same, one state earlier: rewinding a member whose slot is
            -- lost succeeds and changes nothing, since what it then needs to
            -- replay is gone. Re-seeding is the only way back, and it is an
            -- operator's decision, not a step.
            | peerSlotLost -> Degraded lostSlotWhy
            | otherwise -> Rejoin peerSide
        (Primary{}, Absent) -> Degraded "the peer has no cluster: seed it before it can stream"
        (Primary{}, Unreachable why) -> Degraded ("the peer is unreachable: " <> why)
        (Primary{}, Primary{})
            | discardable peerSide -> StopMember peerSide
            | otherwise -> Refuse "both machines are primaries; say which side's writes may be discarded"
        -- The declared primary is a standby: this is the switchover, and what
        -- it costs depends on what the peer is doing.
        (Standby _ _ up _ _ _, Primary{})
            -- stopping the peer is safe only once the declared primary is
            -- streaming from it, because a clean stop hands the tail over
            -- through that connection and there is otherwise nothing to hand
            -- it over. Declared during a partition, the old rule stopped the
            -- primary and stranded every record the standby had not got --
            -- an outage produced out of a healthy pair by a declaration.
            | up /= Just peer.member_host -> AwaitStreaming primarySide
            | any (not . bouncer_paused) bouncers -> PauseBouncers
            | otherwise -> StopMember peerSide
        (Standby _ _ up _ recv _, Stopped _ _ checkpoint clean)
            -- the operator has already said what may be lost, so nothing
            -- below can tell them anything they have not accepted.
            | discardable peerSide -> Promote primarySide
            -- a crashed peer proves nothing past its last checkpoint: it may
            -- have written and acknowledged any amount of WAL after it, and
            -- pg_control does not say. Comparing against the checkpoint would
            -- read as "the standby has everything" precisely when it is least
            -- likely to be true.
            | not clean ->
                Refuse
                    "the peer did not stop cleanly, so what it wrote after its last checkpoint is unknown; say whether its writes may be discarded"
            | recv >= checkpoint -> Promote primarySide
            -- the tail is only ever in flight while the connection is up.
            -- Once it is gone and the peer is stopped, nothing will arrive
            -- however long anybody waits, and the way to converge without
            -- losing those records is to start the machine that has them:
            -- the standby catches up from it, and the ordinary switchover
            -- takes it from there.
            | up /= Just peer.member_host -> StartMember peerSide
            | otherwise -> AwaitCatchUp primarySide checkpoint
        (Standby{}, Absent) -> Promote primarySide
        (Standby _ _ _ _ recv _, Standby _ _ _ _ peerRecv _)
            | recv >= peerRecv -> Promote primarySide
            | otherwise -> Refuse "the peer standby is ahead of the declared primary"
        (Standby{}, Unreachable why)
            | not (discardable peerSide) ->
                Refuse ("cannot confirm the peer has stopped (" <> why <> "); say whether its writes may be discarded")
            | any (not . bouncer_paused) bouncers -> PauseBouncers
            | otherwise -> Promote primarySide
        -- The declared primary is not running: nothing can be decided until it is.
        (Stopped{}, _) -> StartMember primarySide
        (Absent, _) -> Refuse "the declared primary has no cluster"
        (Unreachable why, _) -> Refuse ("the declared primary is unreachable: " <> why)
  where
    primarySide = pair.pair_primary
    peerSide = other primarySide
    peer = memberOn pair peerSide
    primary = memberOn pair primarySide
    (p, q) = case primarySide of
        A -> (obsA, obsB)
        B -> (obsB, obsA)

    mismatchedCluster = case (sysidOf p, sysidOf q) of
        (Just x, Just y) -> x /= y
        _ -> False

    discardable side = pair.pair_may_discard == Just side

    -- the slot the peer streams with lives on the declared primary, which is
    -- the only machine that can say what has become of it.
    peerSlotLost = case p of
        Primary _ _ _ slots -> lookup (slotNameFor pair peerSide) slots == Just "lost"
        _ -> False

    lostSlotWhy =
        "the peer's replication slot ("
            <> slotNameFor pair peerSide
            <> ") is lost: it fell further behind than the slot budget allows, so the WAL it needs is gone and only a re-seed brings it back"

    -- every bouncer sending clients to the declared primary, and none of
    -- them holding those clients: a bouncer left paused is an outage, so it
    -- is not a state any pass may call finished.
    bouncersReady =
        all
            (\b -> not b.bouncer_paused && b.bouncer_upstream == Just primary.member_host)
            bouncers

-------------------------------------------------------------------------------
-- Doing it

{- | The command a step runs, and where.

Pure, so that what a switchover actually does to a machine is readable and
testable without one. 'Left' is a step that runs nowhere: 'Done' and
'Degraded' are arrivals, 'Refuse' is a stop, the two @Await@s are waits, and
the two bouncer steps are not implemented yet (see @specs\/pg-switchover.md@
phase 4) -- they cannot arise while no bouncer is declared.
-}
stepCommand :: Pair -> Step -> Either Text (Member, String)
stepCommand pair = go
  where
    go (StopMember side) = Right (on side, stopScript side)
    go (StartMember side) =
        Right (on side, pgctl side "status >/dev/null 2>&1 || " <> unwords ["pg_ctlcluster", "\"$version\"", cluster side, "start"])
    go (Promote side) =
        Right
            ( on side
            , psql side "SELECT CASE WHEN pg_promote(true, 60) THEN 'promoted' ELSE 'promotion timed out' END"
            )
    go (Rejoin side) = Right (on side, rejoinScript side)
    go Done = Left "nothing to do"
    go (Degraded why) = Left why
    go (Refuse why) = Left why
    go (AwaitCatchUp _ _) = Left "waiting for the standby to catch up"
    go (AwaitStreaming _) = Left "waiting for the standby to start streaming"
    go PauseBouncers = Left "bouncers are not wired up yet"
    go (RepointBouncers _) = Left "bouncers are not wired up yet"

    on = memberOn pair
    cluster side = Text.unpack (on side).member_cluster
    port side = show (on side).member_port

    {- Fast, not immediate: a clean shutdown sends the standby everything it
    has not got, including the shutdown checkpoint, which is the record the
    promotion waits for.

    What that clean shutdown also does is checkpoint, and a checkpoint
    recycles the WAL before it -- which is the WAL a rewind of this member
    would need, read back to the last checkpoint the two machines share. In
    an ordinary switchover that is the shutdown checkpoint itself and nothing
    older is wanted; after a split brain the histories parted much earlier,
    and stopping the loser is what destroys the record of how. Pinning what
    pg_wal already holds costs nothing, since it is on the disk either way,
    and the rejoin takes the pin off again. -}
    stopScript side =
        unlines
            [ "set -e"
            , versionOf side
            , "datadir=/var/lib/postgresql/$version/" <> cluster side
            , "keep=$(du -sm \"$datadir/pg_wal\" | awk '{print $1 + 1}')"
            , "sudo -u postgres psql -p " <> port side <> " -tAX -d postgres -c \"ALTER SYSTEM SET wal_keep_size = '${keep}MB'\""
            , "sudo -u postgres psql -p " <> port side <> " -tAX -d postgres -c 'SELECT pg_reload_conf()'"
            , unwords ["pg_ctlcluster", "\"$version\"", cluster side, "stop -m fast"]
            ]

    pgctl side action =
        unlines
            [ "set -e"
            , versionOf side
            , unwords ["pg_ctlcluster", "\"$version\"", cluster side, action]
            ]

    versionOf side =
        "version=$(pg_lsclusters --no-header | awk -v c=" <> shQuote (cluster side) <> " '$2==c {print $1}' | head -n1)"

    psql side sql =
        unlines
            [ "set -e"
            , unwords ["sudo", "-u", "postgres", "psql", "-p", port side, "-tAX", "-d", "postgres", "-c", shQuote sql]
            ]

    {- An old primary rejoins by being rewound onto the new one's history,
    not by being copied over the network: same machine, same data, only the
    records that diverged are replaced. @-R@ writes standby.signal and
    primary_conninfo, so starting it afterwards is starting a standby.

    pg_rewind wants the target shut down, and refuses outright if
    wal_log_hints was off when the cluster was made -- which is why
    Postgres.defaultReplicationTuning turns it on before there is data. -}
    rejoinScript side =
        let peerSide' = other side
            conf = "\"$datadir/postgresql.auto.conf\""
            slotHere = slotNameFor pair side
            slotHeld = slotNameFor pair peerSide' 
         in unlines
                [ "set -e"
                , versionOf side
                , "datadir=/var/lib/postgresql/$version/" <> cluster side
                , "confdir=/etc/postgresql/$version/" <> cluster side
                , "bindir=/usr/lib/postgresql/$version/bin"
                , unwords ["pg_ctlcluster", "\"$version\"", cluster side, "stop || true"]
                , -- pg_rewind finishes a crashed target's recovery itself, by
                  -- running `postgres --single -D <datadir>` -- which takes
                  -- the configuration to be in the data directory. On Debian
                  -- it is in /etc/postgresql, so that step fails and the
                  -- rewind with it, in exactly the case a failover is about:
                  -- the machine that died. Do the recovery here, where the
                  -- config file's location is known. Single-user, never a
                  -- start: a server that listens is a second primary, and
                  -- this one still believes it is the primary.
                  "state=$(\"$bindir/pg_controldata\" -D \"$datadir\" | sed -n 's/^Database cluster state: *//p')"
                , -- and it must not throw away what it is being run for: a
                  -- clean shutdown ends in a checkpoint, and a checkpoint
                  -- recycles the WAL before it -- which is the WAL pg_rewind
                  -- then reads, from the last checkpoint the two machines
                  -- share. Keeping as much as pg_wal already holds costs
                  -- nothing, since it is on the disk either way.
                  "keep=$(du -sm \"$datadir/pg_wal\" | awk '{print $1 + 1}')"
                , "case \"$state\" in"
                , "  'shut down'|'shut down in recovery') ;;"
                , "  *) sudo -u postgres \"$bindir/postgres\" --single -D \"$datadir\""
                    <> " -c config_file=\"$confdir/postgresql.conf\" -c wal_keep_size=\"${keep}MB\""
                    <> " template1 </dev/null >/dev/null ;;"
                , "esac"
                , "sudo -u postgres env PGPASSFILE=" <> shQuote pair.pair_rewind_passfile <> " \"$bindir/pg_rewind\""
                    <> " --target-pgdata=\"$datadir\" -R --source-server="
                    <> shQuote (sourceServer peerSide')
                , -- pg_rewind's own -R writes these, but it is also entitled
                  -- to decide no rewind was needed at all -- which is the
                  -- ordinary case after a clean switchover. Writing them
                  -- here makes "this member comes back as a standby" true of
                  -- both outcomes, instead of leaving a second primary
                  -- behind on the quiet one.
                  "sudo -u postgres sed -i '/^primary_conninfo/d' " <> conf
                , "echo " <> shQuote ("primary_conninfo = '" <> primaryConninfo peerSide' <> "'") <> " | sudo -u postgres tee -a " <> conf <> " >/dev/null"
                , {- The slot this member streams with, on the machine it
                  streams from. Slots are not replicated and nothing else
                  creates this one, so a rejoining member makes its own --
                  and then checks, because a standby that names a slot the
                  primary does not have retries forever ("replication slot
                  ... does not exist") while looking, to every other query,
                  like a healthy standby. Creating it goes over the
                  replication connection, which is the one path the pair is
                  already required to have; the check goes over the rewind
                  role's ordinary one. -}
                  "sudo -u postgres env PGPASSFILE=" <> shQuote pair.pair_repl_passfile <> " psql -tAX -d " <> shQuote (replicationConn peerSide')
                    <> " -c " <> shQuote ("CREATE_REPLICATION_SLOT " <> Text.unpack slotHere <> " PHYSICAL") <> " >/dev/null 2>&1 || true"
                , "have=$(sudo -u postgres env PGPASSFILE=" <> shQuote pair.pair_rewind_passfile <> " psql -tAX -d " <> shQuote (sourceServer peerSide')
                    <> " -c " <> shQuote ("SELECT count(*) FROM pg_replication_slots WHERE slot_name = '" <> Text.unpack slotHere <> "'") <> ")"
                , "[ \"$have\" = 1 ] || { echo " <> shQuote ("no replication slot " <> Text.unpack slotHere <> " on " <> Text.unpack (on peerSide').member_host) <> " >&2; exit 1; }"
                , "sudo -u postgres sed -i '/^primary_slot_name/d' " <> conf
                , "echo " <> shQuote ("primary_slot_name = '" <> Text.unpack slotHere <> "'") <> " | sudo -u postgres tee -a " <> conf <> " >/dev/null"
                , -- and the WAL the stop pinned so that this rewind could
                  -- happen: it has happened, and a standby holding every
                  -- segment it ever saw fills a disk.
                  "sudo -u postgres sed -i '/^wal_keep_size/d' " <> conf
                , "sudo -u postgres touch \"$datadir/standby.signal\""
                , unwords ["pg_ctlcluster", "\"$version\"", cluster side, "start"]
                , -- and the slot this member held for the peer back when it
                  -- was the primary. Nothing consumes it here, and a slot
                  -- nobody consumes still pins every segment behind it: a
                  -- standby that keeps one fills its own disk waiting for a
                  -- machine that is not coming. Not fatal if it fails --
                  -- this member is already back in the pair by now.
                  "sudo -u postgres psql -p " <> port side <> " -tAX -d postgres -c "
                    <> shQuote ("SELECT pg_drop_replication_slot(slot_name) FROM pg_replication_slots WHERE slot_name = '" <> Text.unpack slotHeld <> "'")
                    <> " >/dev/null || echo " <> shQuote ("could not drop the stale slot " <> Text.unpack slotHeld) <> " >&2"
                ]

    primaryConninfo side =
        unwords
            [ "host=" <> Text.unpack (on side).member_host
            , "port=" <> port side
            , "user=" <> Text.unpack pair.pair_repl_role
            , "passfile=" <> pair.pair_repl_passfile
            ]

    replicationConn side =
        unwords
            [ "host=" <> Text.unpack (on side).member_host
            , "port=" <> port side
            , "user=" <> Text.unpack pair.pair_repl_role
            , "dbname=postgres"
            , -- the physical kind. `replication=database` is the logical one,
              -- and pg_hba matches that against the database name.
              "replication=true"
            ]

    sourceServer side =
        unwords
            [ "host=" <> Text.unpack (on side).member_host
            , "port=" <> port side
            , "user=" <> Text.unpack pair.pair_rewind_role
            , "dbname=postgres"
            ]

shQuote :: String -> String
shQuote s = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) s <> "'"

-------------------------------------------------------------------------------

data Report
    = Probed !Side !Observed
    | Deciding !Step
    | Acted !Side !ExitCode !Text
    deriving (Show)

{- | Where this pair's primary is.

A node stating a fact about two machines, not an action: its @check@ asks
them both and is satisfied only when the declaration holds, and its @up@
takes 'nextStep's steps until it does. Both run from a /controlling/
machine, over ssh -- never from a member, since the member that dies might
be the one running this.

@ref@ is keyed on the pair, never on which side is primary, so moving the
primary changes this node rather than declaring a second one. The declared
side is in @notes@ instead, which is what makes a re-declaration visible to
@run serve@ as a change (see "Salmon.Actions.Serve"'s @Stale@).

There is no @down@: tearing a pair down is not "stop being a primary", it is
whatever the machines' own nodes do, and a switchover node that could stop
serving on the way out is a footgun with no use.
-}
pairRole :: Reporter Report -> Pair -> Op
pairRole r pair =
    op "pg-pair-role" nodeps $ \actions ->
        actions
            { ref = mkRef "pg-pair-role" pair.pair_name
            , help = Text.unwords ["primary of", pair.pair_name, "is on", Text.pack (show pair.pair_primary)]
            , notes =
                [ "primary declared on " <> Text.pack (show pair.pair_primary)
                , maybe "no side's writes may be discarded" (\s -> "writes may be discarded on " <> Text.pack (show s)) pair.pair_may_discard
                ]
            , check = verdict <$> decide pair
            , up = converge r pair
            }

{- | What the pair is, as a verdict.

'Degraded' is 'Unknown' on purpose. Under @run serve@ that is the one
verdict which restarts nothing (see "Salmon.Actions.Upkeep"), and "the
primary is where it should be, and the other machine is unreachable" is
exactly a state to keep looking at and not to act on.
-}
verdict :: Step -> CheckResult
verdict Done = Success
verdict (Degraded _) = Unknown
-- the peer is where it should be and pointed where it should be, and is not
-- streaming: a partition reads as this, and so does a standby that came back
-- a second ago. Neither is a reason to act, and only one of them is a reason
-- to worry, which is a distinction no observation can make.
verdict (AwaitStreaming _) = Unknown
verdict (Refuse why) = Failure why
verdict step = Failure (Text.pack (show step) <> " is still to do")

-- | Asks both machines, then draws the conclusion.
decide :: Pair -> IO Step
decide pair = do
    obsA <- observe pair A
    obsB <- observe pair B
    -- no bouncers yet: an empty list is "nobody is holding any clients",
    -- which is true, rather than a special case.
    pure (nextStep pair obsA obsB [])

-- | Runs 'probeScript' on a member and reads what comes back.
observe :: Pair -> Side -> IO Observed
observe pair side = do
    (code, out, err) <- sshTo pair side (probeScript (memberOn pair side))
    pure $ case code of
        ExitSuccess -> parseObserved out
        ExitFailure _ -> Unreachable (Text.strip (Text.take 200 err))

{- | Steps until the declaration holds.

The loop is the design: every turn starts by asking the machines again, so
an @up@ that died half-way is resumed by the next one rather than continued
from a note it left itself. The budget is a guard against a state this table
cannot leave, not a timeout -- a pair that needs more than a handful of
steps is a pair something else is fighting over.
-}
converge :: Reporter Report -> Pair -> IO ()
converge r = convergeUpTo r stepBudget

{- | How many steps a pass may take before it decides the pair is being
fought over by something else. A switchover between two machines that are
both there is three: stop the old primary, promote the new one, rejoin the
old one.
-}
stepBudget :: Int
stepBudget = 12

-- | 'converge', with the budget spelled out. A test stopping a controller
-- part-way is what this is for.
convergeUpTo :: Reporter Report -> Int -> Pair -> IO ()
convergeUpTo r budget0 pair = go budget0
  where
    go budget = do
        step <- decide pair
        runReporter r (Deciding step)
        case step of
            Done -> pure ()
            Degraded _ -> pure ()
            Refuse why -> throwIO (userError (Text.unpack ("pair " <> pair.pair_name <> ": " <> why)))
            -- a standby that is pointed here and still not streaming when the
            -- waiting runs out is a pair that is one machine short, which is
            -- a state to report and keep looking at -- not a pass that
            -- failed. Every other step that outlasts its budget is.
            AwaitStreaming _ | budget <= 0 -> pure ()
            _
                -- asked after the arrivals, never before: a pass that has
                -- spent its budget and is /there/ has not failed at anything.
                | budget <= 0 ->
                    throwIO (userError ("pair " <> Text.unpack pair.pair_name <> ": still " <> show step <> " after " <> show budget0 <> " steps, giving up"))
            AwaitCatchUp _ _ -> waitABit >> go (budget - 1)
            AwaitStreaming _ -> waitABit >> go (budget - 1)
            _ -> case stepCommand pair step of
                Left why -> throwIO (userError (Text.unpack ("pair " <> pair.pair_name <> ": " <> why)))
                Right (_, script) -> do
                    let side = sideOf step
                    (code, out, err) <- sshTo pair side script
                    runReporter r (Acted side code (Text.strip (out <> err)))
                    case code of
                        ExitSuccess -> go (budget - 1)
                        ExitFailure _ ->
                            throwIO (userError (Text.unpack ("pair " <> pair.pair_name <> ": " <> Text.pack (show step) <> " failed: " <> Text.strip err)))

    waitABit = threadDelay (min 5 pair.pair_catch_up_seconds * 1000000)

    sideOf (StopMember s) = s
    sideOf (StartMember s) = s
    sideOf (Promote s) = s
    sideOf (Rejoin s) = s
    sideOf _ = pair.pair_primary

-- | Runs a script on a member over ssh, as the login that member declares.
sshTo :: Pair -> Side -> String -> IO (ExitCode, Text, Text)
sshTo pair side script = do
    (code, out, err) <- readCreateProcessWithExitCode (proc "ssh" args) ""
    pure (code, decode out, decode err)
  where
    m = memberOn pair side
    args =
        concat
            [ maybe [] (\key -> ["-i", key, "-o", "IdentitiesOnly=yes"]) m.member_ssh_identity
            , maybe [] (\hosts -> ["-o", "UserKnownHostsFile=" <> hosts, "-o", "StrictHostKeyChecking=accept-new"]) pair.pair_ssh_known_hosts
            , ["-o", "BatchMode=yes"]
            , -- a member that cannot be reached is the case this recipe
              -- exists for, so deciding that must take seconds. Left to
              -- itself ssh retries a dropped connection for minutes, which
              -- would make every pass during a partition hang rather than
              -- report. The second pair covers a connection that dies while
              -- the probe is already running.
              ["-o", "ConnectTimeout=10", "-o", "ServerAliveInterval=5", "-o", "ServerAliveCountMax=2"]
            , [Text.unpack m.member_ssh_user <> "@" <> Text.unpack m.member_host]
            , ["bash", "-c", shQuote script]
            ]
    decode = Text.decodeUtf8With TextError.lenientDecode
