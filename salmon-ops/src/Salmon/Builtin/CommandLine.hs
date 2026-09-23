{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Salmon.Builtin.CommandLine where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Data.Foldable (traverse_)
import Control.Monad.Identity
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBysteString
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Options.Applicative
import qualified Options.Applicative
import Options.Generic
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdin, stdout)

import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Concurrency as Concurrency
import Salmon.Op.Configure
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (Ref)
import Salmon.Op.Rewrite (Phase (..), Rewrite, Rewritten)
import qualified Salmon.Op.Rewrite as Rewrite
import Salmon.Op.Eval
import Salmon.Op.OpGraph
import Salmon.Op.Track

import Salmon.Actions.Dot as Dot
import qualified Salmon.Actions.Follow as Follow
import qualified Salmon.Actions.Follow.Registry as Registry
import qualified Salmon.Actions.Follow.Scheduler as Scheduler
import Salmon.Actions.Help as Help
import qualified Salmon.Actions.Query as Query
import qualified Salmon.Actions.Serve as Serve
import qualified Salmon.Actions.Serve.Events as Events
import qualified Salmon.Actions.Serve.Http as Http
import qualified Salmon.Actions.Serve.Socket as Socket
import qualified Salmon.Actions.Serve.StatusSink as StatusSink
-- 'CheckResult' constructors are hidden: 'Success'/'Failure' collide with
-- optparse-applicative's 'ParserResult' ones, which this module pattern
-- matches on. Nothing here needs a 'CheckResult'.
import Salmon.Actions.UpDown as UpDown hiding (Failure, Success)
import Salmon.Builtin.Extension
import Salmon.Reporter
import qualified Salmon.Reporter.Tagged as Tagged

data Command seed
    = Config seed
    | Query QueryCommand
    | Run RunCommand
    deriving (Eq, Ord, Generic, Show)

-- | Kept for the JSON/remote-call contract in "Salmon.Builtin.Nodes.Self"
-- ('CLI.RemoteCall'/'argForBaseCommand') — a self-call is always a plain
-- 'Up' today, never plan-aware, so this never needs a plan file field.
data BaseCommand
    = Up
    | Down
    | Tree
    | DAG
    | Serve
    deriving (Eq, Ord, Generic, Show, Read)

argForBaseCommand :: BaseCommand -> Text
argForBaseCommand = \case
    Up -> "up"
    Down -> "down"
    Tree -> "tree"
    DAG -> "dag"
    Serve -> "serve"

-- | The @run@ subcommand's own subcommands. Parsed by hand (rather than via
-- 'Options.Generic''s derived 'ParseRecord', which 'BaseCommand' still uses
-- for its own, unrelated JSON contract) so that @up@ can carry an optional
-- @--plan@/@--force-stale-plan@ pair.
data RunCommand
    = -- | @run up@, optionally honoring a @query plan@-emitted 'Query.Plan' file.
      RunUp !(Maybe FilePath) !Bool !ReportFormat
    | RunDown !ReportFormat
    | RunTree
    | RunDAG
    | -- | @run serve@, optionally capping how many nodes converge at once
      -- per pass (R6 in @specs/per-node-state-machines-remaining.md@;
      -- 'Nothing' is unbounded, matching every version of @serve@ before
      -- this flag existed), and optionally starting with @autoconverge@ off
      -- (@--no-autoconverge@; 'False' is the default, matching every
      -- version of @serve@ before the setting existed). Then pull mode
      -- ("Salmon.Actions.Follow"): a registry to follow (@--follow
      -- REGISTRY@, a directory or @git+URL@ — see
      -- "Salmon.Actions.Follow.Registry"), the labels to fetch from it
      -- (@--label L@,
      -- repeatable; both or neither), and the fetcher's schedule
      -- ('FollowOptions'). Last, optionally listening for the same
      -- line protocol on a unix socket (@--listen PATH@, milestone 2 of
      -- @specs/generic-server.md@; see "Salmon.Actions.Serve.Socket"),
      -- stdin still read beside it, and for HTTP on a second unix socket
      -- (@--http PATH@, milestone 3; see "Salmon.Actions.Serve.Http").
      -- Last, the status sink ('SinkOptions', milestone 5 of
      -- @specs/pull-mode.md@; see "Salmon.Actions.Serve.StatusSink").
      -- The HTTP server's event stream keeps @--events-ring N@ events for a
      -- client to resume from (milestone 4; see "Salmon.Actions.Serve.Events").
      RunServe !(Maybe Int) !Bool !ReportFormat !(Maybe FilePath) ![Text] !FollowOptions !(Maybe FilePath) !(Maybe FilePath) !Int !SinkOptions
    deriving (Eq, Ord, Generic, Show)

{- | @--status-sink PATH@ and @--status-sink-interval SECONDS@ (default
'StatusSink.defaultInterval'): where this host's status document is written,
and how often between the writes a convergence pass or a follow injection
triggers on their own. 'Nothing' writes none.
-}
data SinkOptions = SinkOptions
    { sinkPath :: !(Maybe FilePath)
    , sinkInterval :: !Int
    }
    deriving (Eq, Ord, Generic, Show)

instance FromJSON SinkOptions
instance ToJSON SinkOptions

instance FromJSON RunCommand
instance ToJSON RunCommand

{- | The @--follow-*@ flags: the scheduler's numbers
("Salmon.Actions.Follow.Scheduler"), in seconds where they are durations,
then the cache directory (@--follow-cache DIR@; none by default, in which
case nothing survives a restart) and @--follow-refuse-older@ (see
'Follow.followRefuseOlder'). @--follow-interval@ is milestone 2's name for
the base delay, kept as a synonym of @--follow-base@; either may be given,
the base's own flag wins. Last, the backends' own knobs (milestone 6):
@--follow-workdir@ for the git checkout.
-}
data FollowOptions = FollowOptions
    { followBase :: !(Maybe Int)
    , followInterval :: !(Maybe Int)
    , followFactor :: !Double
    , followCap :: !Int
    , followJitter :: !Double
    , followDebounce :: !Int
    , followMaxWait :: !Int
    , followCacheDir :: !(Maybe FilePath)
    , followRefuseOlder :: !Bool
    , followWorkdir :: !(Maybe FilePath)
    }
    deriving (Eq, Ord, Generic, Show)

instance FromJSON FollowOptions
instance ToJSON FollowOptions

followSchedule :: FollowOptions -> Scheduler.Config
followSchedule o =
    Scheduler.Config
        { Scheduler.schedBase = seconds (fromMaybe defaultBase (o.followBase <|> o.followInterval))
        , Scheduler.schedFactor = max 1 o.followFactor
        , Scheduler.schedCap = seconds o.followCap
        , Scheduler.schedJitter = max 0 (min 1 o.followJitter)
        , Scheduler.schedDebounce = seconds o.followDebounce
        , Scheduler.schedMaxWait = seconds o.followMaxWait
        }
  where
    seconds n = max 0 n * 1000000
    defaultBase = Scheduler.defaultConfig.schedBase `div` 1000000

{- | How the commands that execute something (@run up@, @run down@, @run
serve@) report. @--json@ selects 'ReportJson': one JSON object per line on
stdout, in the encoding "Salmon.Reporter.Tagged" defines, in place of the
binary's own text reporters — so @run up --json | jq@ works, and a client
of the server @specs\/generic-server.md@ sketches reads the same objects.
Absent, 'ReportText' hands every report to the reporters the binary
passed in, untouched.
-}
data ReportFormat
    = ReportText
    | ReportJson
    deriving (Eq, Ord, Generic, Show)

instance FromJSON ReportFormat
instance ToJSON ReportFormat

data QueryCommand
    = -- | @query show@: annotate the directive's tree with [selected]/[excluded].
      -- The 'Bool's are dedupe (print each 'Salmon.Op.Ref.Ref' only once, at
      -- its first-encountered path; on by default, @--no-dedupe@ turns it
      -- off) and descriptions (@--descriptions@: print each node's help text
      -- on an indented line below its path).
      QueryShow !QuerySelection !Bool !Bool
    | -- | @query plan@: emit a 'Query.Plan' (JSON) for @run up --plan@.
      QueryPlan !QuerySelection !Bool
    | -- | @query extract-directive@: recover an embedded directive from a
      -- 'Query.Plan' file made with @query plan --embed-directive@.
      QueryExtractDirective !FilePath
    deriving (Eq, Ord, Generic, Show)

instance FromJSON QueryCommand
instance ToJSON QueryCommand

data QuerySelection = QuerySelection
    { querySelect :: [Text]
    , queryExclude :: [Text]
    }
    deriving (Eq, Ord, Generic, Show)

instance FromJSON QuerySelection
instance ToJSON QuerySelection

instance (ParseRecord seed) => ParseRecord (Command seed) where
    parseRecord =
        combo <**> helper
      where
        combo =
            hsubparser $
                mconcat
                    [ command "config" (info (Config <$> parseRecord) cfg)
                    , command "query" (info (Query <$> queryCommandParser) qry)
                    , command "run" (info (Run <$> runCommandParser) run)
                    , commandGroup "Salmon Commands."
                    ]
        cfg = progDesc "Prints a config."
        qry = progDesc "Inspects, or plans an exclusion against, a directive on stdin."
        run = progDesc "Runs a config."

runCommandParser :: Parser RunCommand
runCommandParser =
    hsubparser $
        mconcat
            [ command "up" (info upP (progDesc "Runs (up) the directive on stdin."))
            , command "down" (info (RunDown <$> reportFormatP) (progDesc "Tears down (down) the directive on stdin."))
            , command "tree" (info (pure RunTree) (progDesc "Prints a human-readable dependency tree."))
            , command "dag" (info (pure RunDAG) (progDesc "Prints Graphviz dot output."))
            , command "serve" (info serveP (progDesc "Reads a stream of seed declarations on stdin and converges."))
            ]
  where
    serveP =
        RunServe
            <$> optional
                ( Options.Applicative.option
                    Options.Applicative.auto
                    ( long "max-concurrency"
                        <> Options.Applicative.metavar "N"
                        <> Options.Applicative.help "Cap how many nodes converge (check/up/down) at once per pass. Omitted = unbounded."
                    )
                )
            <*> switch
                ( long "no-autoconverge"
                    <> Options.Applicative.help
                        "Start with `autoconverge off`: declarations are recorded but not converged until an explicit `converge`."
                )
            <*> reportFormatP
            <*> optional
                ( strOption
                    ( long "follow"
                        <> Options.Applicative.metavar "REGISTRY"
                        <> Options.Applicative.help "Pull mode: fetch declarations (one document per --label) from a registry: a directory (DIR/<label>.json) or git+URL[#BRANCH[:SUBDIR]] (SUBDIR/<label>.json in the checkout)."
                    )
                )
            <*> many
                ( strOption
                    ( long "label"
                        <> Options.Applicative.metavar "LABEL"
                        <> Options.Applicative.help "A label to follow in the --follow registry; repeatable, the desired set is the union."
                    )
                )
            <*> followOptionsP
            <*> optional
                ( strOption
                    ( long "listen"
                        <> Options.Applicative.metavar "PATH"
                        <> Options.Applicative.help
                            "Also accept the line protocol on a unix socket at PATH (created owner-only); each client is answered on its own connection, as JSON lines. Stdin keeps working alongside."
                    )
                )
            <*> optional
                ( strOption
                    ( long "http"
                        <> Options.Applicative.metavar "PATH"
                        <> Options.Applicative.help
                            "Also serve HTTP on a unix socket at PATH (created owner-only): GET /dag, /status, /history, /help/seed and POST /command[?async]. Stdin keeps working alongside."
                    )
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "events-ring"
                    <> Options.Applicative.metavar "N"
                    <> Options.Applicative.value (Events.configRing Events.defaultConfig)
                    <> Options.Applicative.showDefault
                    <> Options.Applicative.help "How many events --http's /events keeps for a client to resume from with ?since=; a client further behind is sent a gap event."
                )
            <*> sinkOptionsP
    sinkOptionsP =
        SinkOptions
            <$> optional
                ( strOption
                    ( long "status-sink"
                        <> Options.Applicative.metavar "PATH"
                        <> Options.Applicative.help "Write this host's status document (JSON: host, mode, applied documents per label, the `status` object, the last converge and follow reports) to PATH, atomically, after every convergence pass and follow injection and on a timer; `salmon-fleet status DIR` folds a directory of them."
                    )
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "status-sink-interval"
                    <> Options.Applicative.metavar "SECONDS"
                    <> Options.Applicative.value (StatusSink.defaultInterval `div` 1000000)
                    <> showDefault
                    <> Options.Applicative.help "Seconds between two status sink writes when nothing triggers one."
                )
    followOptionsP =
        FollowOptions
            <$> optional
                ( Options.Applicative.option
                    Options.Applicative.auto
                    ( long "follow-base"
                        <> Options.Applicative.metavar "SECONDS"
                        <> Options.Applicative.help ("Seconds between two rounds of fetching the followed labels while rounds succeed (default " <> show (defaultSecs (.schedBase)) <> ").")
                    )
                )
            <*> optional
                ( Options.Applicative.option
                    Options.Applicative.auto
                    ( long "follow-interval"
                        <> Options.Applicative.metavar "SECONDS"
                        <> Options.Applicative.help "Same as --follow-base (the older name)."
                    )
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "follow-factor"
                    <> Options.Applicative.metavar "FACTOR"
                    <> Options.Applicative.value Scheduler.defaultConfig.schedFactor
                    <> showDefault
                    <> Options.Applicative.help "How much slower each consecutive failed round makes the next one."
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "follow-cap"
                    <> Options.Applicative.metavar "SECONDS"
                    <> Options.Applicative.value (defaultSecs (.schedCap))
                    <> showDefault
                    <> Options.Applicative.help "The longest a failing registry is left alone between rounds."
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "follow-jitter"
                    <> Options.Applicative.metavar "FRACTION"
                    <> Options.Applicative.value Scheduler.defaultConfig.schedJitter
                    <> showDefault
                    <> Options.Applicative.help "Every delay is scaled by a uniform draw from [1-j, 1+j], so a fleet does not poll in step."
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "follow-debounce"
                    <> Options.Applicative.metavar "SECONDS"
                    <> Options.Applicative.value (defaultSecs (.schedDebounce))
                    <> showDefault
                    <> Options.Applicative.help "How long the registry must be quiet after a change before the change is applied; 0 applies at once."
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "follow-max-wait"
                    <> Options.Applicative.metavar "SECONDS"
                    <> Options.Applicative.value (defaultSecs (.schedMaxWait))
                    <> showDefault
                    <> Options.Applicative.help "The longest a change waits to be applied while the registry keeps changing."
                )
            <*> optional
                ( strOption
                    ( long "follow-cache"
                        <> Options.Applicative.metavar "DIR"
                        <> Options.Applicative.help "Keep each label's last applied document in DIR, and replay it at startup when the registry cannot be reached (status then says `mode: replay`). Without it a restart against an unreachable registry declares nothing."
                    )
                )
            <*> switch
                ( long "follow-refuse-older"
                    <> Options.Applicative.help "Refuse a fetched document whose `published` timestamp is older than the one already applied for its label (reported as stale, not injected). Documents without `published` are never refused."
                )
            <*> optional
                ( strOption
                    ( long "follow-workdir"
                        <> Options.Applicative.metavar "DIR"
                        <> Options.Applicative.help "Where a git+ registry is checked out (cloned once, then fetched and reset every round). Default: `checkout` under --follow-cache, else a directory under the system temporary directory named by the repository."
                    )
                )
    defaultSecs :: (Scheduler.Config -> Int) -> Int
    defaultSecs f = f Scheduler.defaultConfig `div` 1000000
    upP =
        RunUp
            <$> optional
                ( strOption
                    ( long "plan"
                        <> Options.Applicative.metavar "FILE"
                        <> Options.Applicative.help "A `query plan`-emitted Plan: force-skip its excluded nodes."
                    )
                )
            <*> switch
                ( long "force-stale-plan"
                    <> Options.Applicative.help "Proceed even if the plan's directive digest doesn't match stdin."
                )
            <*> reportFormatP
    reportFormatP =
        Options.Applicative.flag
            ReportText
            ReportJson
            ( long "json"
                <> Options.Applicative.help "Report as one JSON object per line on stdout (see Salmon.Reporter.Tagged) instead of text."
            )

queryCommandParser :: Parser QueryCommand
queryCommandParser =
    hsubparser $
        mconcat
            [ command "show" (info showP (progDesc "Prints the directive's tree, annotating [selected]/[excluded] nodes."))
            , command "plan" (info planP (progDesc "Emits a Plan (JSON) excluding --exclude matches."))
            , command "extract-directive" (info extractP (progDesc "Prints a plan file's embedded directive (see `query plan --embed-directive`)."))
            ]
  where
    selectionP =
        QuerySelection
            <$> many (Text.pack <$> strOption (long "select" <> Options.Applicative.metavar "PATTERN" <> Options.Applicative.help "May repeat; union. Omitted entirely = everything."))
            <*> many (Text.pack <$> strOption (long "exclude" <> Options.Applicative.metavar "PATTERN" <> Options.Applicative.help "May repeat; union, then subtracted from the selection."))
    showP =
        QueryShow
            <$> selectionP
            <*> (not <$> switch
                    ( long "no-dedupe"
                        <> Options.Applicative.help "Print every path a node is reachable from, instead of only its first-encountered one (dedupe is on by default)."
                    ))
            <*> switch
                ( long "descriptions"
                    <> Options.Applicative.help "Print each node's help text on an indented line (\"  # ...\") below its path."
                )
    planP =
        QueryPlan
            <$> selectionP
            <*> switch
                ( long "embed-directive"
                    <> Options.Applicative.help "Embed the directive itself in the plan, so `query extract-directive` can recover it later without the original directive on hand."
                )
    extractP =
        QueryExtractDirective
            <$> Options.Applicative.strArgument (Options.Applicative.metavar "PLAN-FILE")

instance (FromJSON seed) => FromJSON (Command seed)
instance (ToJSON seed) => ToJSON (Command seed)

instance FromJSON BaseCommand
instance ToJSON BaseCommand

{- | Function to combine a configuration system (based on a seed).
todo: consider adding some non-det when the graph depends not just on a seed but also on reading a variable in the directive
- either at the configure step: then the seed must contain enough to build the ops
- either in the expand phase from the directive
-}
execCommandOrSeed ::
    forall directive seed.
    (ToJSON directive, FromJSON directive, ParseRecord seed) =>
    Reporter (UpDown.Report Extension) ->
    Configure IO seed directive ->
    Track' directive ->
    Command seed ->
    IO ()
execCommandOrSeed = execCommandOrSeedWith Serve.reportText

{- | 'execCommandOrSeed' with a say in how @run serve@ reports its own
loop-level events (as opposed to the per-node events, which go to the same
reporter every other command uses).
-}
execCommandOrSeedWith ::
    forall directive seed.
    (ToJSON directive, FromJSON directive, ParseRecord seed) =>
    Reporter Serve.Report ->
    Reporter (UpDown.Report Extension) ->
    Configure IO seed directive ->
    Track' directive ->
    Command seed ->
    IO ()
execCommandOrSeedWith serveR r = execCommandOrSeedWithRewrites serveR r []

{- | 'execCommandOrSeedWith' with "Salmon.Op.Rewrite" phases registered.

This is how an application asks for something like
'Salmon.Builtin.Nodes.Debian.Package.batchPackages' — a collection of many
small nodes into one bulk invocation — instead of applying an @Op -> Op@ pass
by hand inside its own 'Track''. The difference is not stylistic: a phase runs
after the fold, so it sees every declaration and which way each node is
wanted, neither of which a @directive -> Op@ can see. See "Salmon.Op.Rewrite".

The phases apply to @run up@, @run down@, @run serve@ — the commands that
execute something — and, as of (R4), @run tree@\/@run dag@: both now print
the /computed/ 'Salmon.Op.Dag.Dag' through 'Salmon.Actions.Help.printDagTree'
\/'Salmon.Actions.Dot.printDagCograph' rather than the declared @Cofree
Graph@, so a batched node shows up once, the way it will actually run.
@query@ is the one holdout still printing the /declared/ graph: it resolves
@--select@\/@--exclude@ as path globs (see 'Salmon.Actions.Query.resolveSelectors'),
and a rewritten 'Salmon.Op.Dag.Dag' has refs and edges but no paths for a
pattern to match against — fixing that needs either a path-free renderer with
its own selection language, or resolving a pattern against the declared graph
and translating the result through 'Salmon.Op.Rewrite.membersOf', neither of
which is worth doing speculatively.
-}
execCommandOrSeedWithRewrites ::
    forall directive seed.
    (ToJSON directive, FromJSON directive, ParseRecord seed) =>
    Reporter Serve.Report ->
    Reporter (UpDown.Report Extension) ->
    [Rewrite Extension] ->
    Configure IO seed directive ->
    Track' directive ->
    Command seed ->
    IO ()
execCommandOrSeedWithRewrites serveR r rewrites genBase traceBase cmd = do
    case cmd of
        (Run (RunUp Nothing _ fmt)) -> do
            result <- withGraph (runUp (updownFor fmt) Set.empty)
            when (result == Just False) exitFailure
        (Run (RunUp (Just planPath) forceStale fmt)) -> do
            result <- withGraphAndBytes $ \dirBytes op -> do
                planBytes <- LBysteString.readFile planPath
                case eitherDecode planBytes of
                    Left err -> do
                        putStrLn ("failed to json-parse plan " <> planPath <> ": " <> err)
                        exitFailure
                    Right plan -> do
                        let actual = Query.digestBytes dirBytes
                        let expected = Query.planDirectiveDigest plan
                        if actual == expected
                            then runUp (updownFor fmt) (Set.fromList (Query.planExcludedRefs plan)) op
                            else
                                if forceStale
                                    then do
                                        putStrLn $
                                            "warning: plan digest mismatch (plan expects "
                                                <> Text.unpack expected
                                                <> ", this directive hashes to "
                                                <> Text.unpack actual
                                                <> "); proceeding due to --force-stale-plan"
                                        runUp (updownFor fmt) (Set.fromList (Query.planExcludedRefs plan)) op
                                    else do
                                        putStrLn $
                                            "refusing to run stale plan: plan expects digest "
                                                <> Text.unpack expected
                                                <> ", but this directive hashes to "
                                                <> Text.unpack actual
                                        exitFailure
            when (result == Just False) exitFailure
        (Run (RunDown fmt)) -> do
            result <- withGraph (runDown (updownFor fmt))
            when (result == Just False) exitFailure
        (Run RunTree) -> do
            -- (R4): the computed 'Dag' is what @run up@ would actually walk
            -- once any "Salmon.Op.Rewrite" phases are registered; with none
            -- registered `computed` is the declared graph, still collapsed
            -- to one line per 'Ref' rather than one per path.
            void $ withGraph (\op -> computedTreeDag op >>= Help.printDagTree)
        (Run RunDAG) -> do
            void $ withGraph (\op -> computedTreeDag (injectRemoteSubgraphs 0 op) >>= Dot.printDagCograph)
        (Run (RunServe maxConcurrency noAutoConverge fmt followDir labels followOptions listen http eventsRing sinkOptions)) -> do
            limit <- traverse Concurrency.newConcurrencyLimit maxConcurrency
            let own = taggedFor fmt
            follow <- case (followDir, traverse Follow.mkLabel labels) of
                (Nothing, Right []) -> pure Nothing
                (Nothing, _) -> do
                    hPutStrLn stderr "--label needs a --follow REGISTRY to fetch from"
                    exitFailure
                (Just _, Right []) -> do
                    hPutStrLn stderr "--follow needs at least one --label to fetch"
                    exitFailure
                (Just _, Left err) -> do
                    hPutStrLn stderr (Text.unpack err)
                    exitFailure
                (Just addr, Right lbls) -> do
                    -- the backend is chosen by the shape of the address; see
                    -- "Salmon.Actions.Follow.Registry"
                    address <- case Registry.parseAddress (Text.pack addr) of
                        Left err -> hPutStrLn stderr (Text.unpack err) >> exitFailure
                        Right a -> pure a
                    registry <-
                        Registry.open
                            Registry.defaultOptions
                                { Registry.optWorkdir = followOptions.followWorkdir
                                , Registry.optCacheDir = followOptions.followCacheDir
                                }
                            address
                    pure $
                        Just
                            Follow.Follow
                                { Follow.followRegistry = registry
                                , Follow.followLabels = lbls
                                , Follow.followSchedule = followSchedule followOptions
                                , Follow.followCache = followOptions.followCacheDir
                                , Follow.followRefuseOlder = followOptions.followRefuseOlder
                                , Follow.followVerify = Follow.noVerifier
                                }
            -- the fetcher's first round is in the inbox before standard
            -- input is even read, so the first convergence is what the
            -- registry says, deterministically; after that both interleave
            -- at line granularity.
            gate <- newEmptyMVar
            -- what `fetch` pokes: the fetcher's clock wakes on it; and
            -- what `status` reads: the fetcher's mode
            pk <- Scheduler.newPoke
            modeVar <- Follow.newMode
            appliedVar <- Follow.newApplied
            host <- StatusSink.hostName
            let onFetch = Follow.followed pk modeVar appliedVar <$ follow
                sinkConfig path =
                    StatusSink.Config
                        { StatusSink.configPath = path
                        , StatusSink.configInterval = max 1 sinkOptions.sinkInterval * 1000000
                        , StatusSink.configHost = host
                        }
            -- the status sink watches the loop's stream for its triggers,
            -- so what everything below reports through is the loop's own
            -- reporter with the sink beside it; the sink's own complaints
            -- go to the loop's own alone.
            withMaybe sinkOptions.sinkPath (\path -> StatusSink.withSink (sinkConfig path) onFetch own) $ \msink -> do
              let tagged = maybe own (reportBoth own . StatusSink.sinkReporter) msink
                  -- with a socket to talk to, the process must outlive
                  -- whatever started it (`< /dev/null &` is the ordinary way
                  -- to run it), so standard input is read as a named source
                  -- rather than as the loop's 'Serve.Stdin': its end of input
                  -- is a hang-up like any client's and only `quit` — from
                  -- stdin or from a client — ends the loop.
                  stdinP = case (listen, http) of
                      (Nothing, Nothing) -> Serve.stdinProducer stdin
                      _ -> Serve.handleProducer (Serve.Origin "stdin") stdin
                  producersWith more =
                      case follow of
                          Nothing -> stdinP : more
                          Just f -> Follow.follower (Tagged.followStream tagged) pk modeVar appliedVar f (putMVar gate ()) : Follow.gated gate stdinP : more
              -- the listener's reporters answer each socket client on its own
              -- connection, the HTTP server's answer each request with its
              -- own reports, and both hand everything on to the loop's own,
              -- which stays exactly as `fmt` says.
              withMaybe listen Socket.withUnixListener $ \mlistener ->
                withMaybe http (\path -> Http.withHttpServerWith Events.defaultConfig{Events.configRing = eventsRing} path (seedHelpText (parseRecord :: Parser seed)) (maybe (pure Serve.Interactive) Serve.followedMode onFetch)) $ \mserver -> do
                    let (serveR0, r0) = reportersOver tagged
                        base = case mlistener of
                            Nothing -> (contramap Serve.attributed serveR0, contramap Serve.attributed r0)
                            Just listener -> Socket.listenerReporters listener tagged
                        (serveR', r') = maybe base (`Http.serverReporters` base) mserver
                        observe acc = do
                            traverse_ (`Http.serverObserver` acc) mserver
                            traverse_ (`StatusSink.sinkObserver` acc) msink
                        more = foldMap (pure . Socket.listenerProducer) mlistener <> foldMap (pure . Http.serverProducer) mserver
                    void $
                        Serve.serveObserved
                            observe
                            rewrites
                            limit
                            (not noAutoConverge)
                            serveR'
                            r'
                            parseSeedArgs
                            genBase
                            traceBase
                            onFetch
                            (producersWith more)
        (Query (QueryShow (QuerySelection sel exc) dedupe showDescriptions)) -> do
            void $ withGraph $ \op -> do
                let cograph = runIdentity (expand op)
                computed <- computedRewritten op
                let (selected, excluded) = Query.resolveRewrittenSelectors cograph computed sel exc
                Query.printAnnotated cograph selected excluded dedupe showDescriptions
        (Query (QueryPlan (QuerySelection sel exc) embedDirective)) -> do
            void $ withGraphAndBytes $ \dirBytes op -> do
                let cograph = runIdentity (expand op)
                computed <- computedRewritten op
                let (_, excluded) = Query.resolveRewrittenSelectors cograph computed sel exc
                let embedded = if embedDirective then Just (Text.decodeUtf8 (LBysteString.toStrict dirBytes)) else Nothing
                let plan = Query.Plan (Query.digestBytes dirBytes) (Set.toList excluded) exc embedded
                LBysteString.putStr (encode plan)
        (Query (QueryExtractDirective planPath)) -> do
            planBytes <- LBysteString.readFile planPath
            case eitherDecode planBytes of
                Left err -> do
                    putStrLn ("failed to json-parse plan " <> planPath <> ": " <> err)
                    exitFailure
                Right plan ->
                    case Query.planDirective plan of
                        Nothing -> do
                            putStrLn ("plan " <> planPath <> " has no embedded directive (was it created with `query plan --embed-directive`?)")
                            exitFailure
                        Just dirText ->
                            LBysteString.putStr (LBysteString.fromStrict (Text.encodeUtf8 dirText))
        Config seed -> do
            dir <- gen genBase seed
            LBysteString.putStr $ encode dir
  where
    nat = pure . runIdentity

    {- | The one 'Tagged.Tagged' reporter a @run up@\/@run down@\/@run
    serve@ speaks through, by 'ReportFormat': for 'ReportText' it dispatches
    back to the reporters the binary passed in (so nothing about the text
    output changes), for 'ReportJson' it is "Salmon.Reporter.Tagged"'s line
    writer on stdout in their place. The tending loop's own stream never
    reaches here on its own — @serve@ forwards what it keeps of it as
    'Serve.Tended', which the encoding nests — so its slot is 'silent'. The
    fetcher's stream ("Salmon.Actions.Follow") goes through here too, so
    that @--json@ covers it and a status sink can watch it. -}
    taggedFor :: ReportFormat -> Reporter Tagged.Tagged
    taggedFor fmt = case fmt of
        ReportText -> Tagged.reportTexts serveR r silent Follow.reportText
        ReportJson -> Tagged.reportJSONLines stdout

    -- | The tagged reporter split contravariantly into the two the drivers take.
    reportersOver :: Reporter Tagged.Tagged -> (Reporter Serve.Report, Reporter (UpDown.Report Extension))
    reportersOver tagged = (Tagged.serveStream tagged, Tagged.updownStream tagged)

    updownFor :: ReportFormat -> Reporter (UpDown.Report Extension)
    updownFor = snd . reportersOver . taggedFor

    {- | @run up@: everything in this one directive's graph is wanted up, so
    that is the rewrites' 'phaseDesired'. @excluded@ (a plan's skipped
    refs) is what they must not collect: batching a node the operator asked
    to skip would run it anyway, under another node's name.

    Exclusion is a 'UpDown.Gate' rather than 'Query.forceSkip' precisely so it
    composes with collections — a batch is worth running iff some member of
    it is, which is the same 'Rewrite.membersOf' translation @serve@'s gate
    does. The report stream is identical either way: both produce a 'Skip'. -}
    runUp :: Reporter (UpDown.Report Extension) -> Set Ref -> Op -> IO Bool
    runUp r' excluded op = do
        dag <- UpDown.expandDag r' nat op
        let computed = Rewrite.rewrite rewrites (Phase (Set.fromList (Dag.dagOrder dag)) excluded) dag
        UpDown.upDag (excluding computed excluded) r' (Rewrite.computedDag computed)

    {- | @run down@: nothing is wanted up, which is what makes a
    direction-aware rewrite emit a teardown batch here and an install batch
    under @run up@, from the same registered phase. -}
    runDown :: Reporter (UpDown.Report Extension) -> Op -> IO Bool
    runDown r' op = do
        dag <- UpDown.expandDag r' nat op
        let computed = Rewrite.rewrite rewrites (Phase Set.empty Set.empty) dag
        UpDown.downDag UpDown.alwaysRequired r' (Rewrite.computedDag computed)

    {- | (R4): the whole-graph 'Rewritten' `run tree`\/`run dag`\/`query`
    all read from — everything in the declared graph is "desired" and
    nothing is "ignored", the same 'Phase' 'Rewrite.wholeGraph' builds for a
    bare @run down@'s rewrite pass, since none of the three is about one
    direction of travel. Kept as the full 'Rewritten' (not just
    'Rewrite.computedDag') because `query` also needs 'Rewrite.membersOf' —
    see 'Query.resolveRewrittenSelectors'.
    -}
    computedRewritten :: Op -> IO (Rewritten Extension)
    computedRewritten op = do
        dag <- UpDown.expandDag r nat op
        pure (Rewrite.rewrite rewrites (Rewrite.wholeGraph dag) dag)

    computedTreeDag :: Op -> IO (Dag.Dag Extension)
    computedTreeDag op = Rewrite.computedDag <$> computedRewritten op

    excluding :: Rewritten Extension -> Set Ref -> UpDown.Gate Extension
    excluding computed excluded
        | Set.null excluded = UpDown.alwaysRequired
        | otherwise = \act ->
            pure $
                if any (`Set.notMember` excluded) (Set.toList (Rewrite.membersOf computed act.extension.ref))
                    then UpDown.Required
                    else UpDown.Skippable

    -- | 'Nothing' iff the incoming JSON graph failed to parse (in which case @cont@ never ran).
    withGraph :: (Op -> IO a) -> IO (Maybe a)
    withGraph cont = withGraphAndBytes (const cont)

    -- | Like 'withGraph', but also hands the continuation the exact raw
    -- bytes read off stdin — needed to digest the directive itself (see
    -- 'Query.digestBytes'), since re-'encode'ing the decoded value gives no
    -- guarantee of hashing to the same bytes.
    withGraphAndBytes :: (LBysteString.ByteString -> Op -> IO a) -> IO (Maybe a)
    withGraphAndBytes cont = do
        jsonbody <- LBysteString.getContents
        case eitherDecode jsonbody of
            Left err -> do
                putStrLn ("failed to json-parse graph: " <> err)
                pure Nothing
            Right a -> do
                Just <$> cont jsonbody (run traceBase a)

-- | Bracket over an optional resource: the continuation gets 'Nothing'
-- when there was nothing to acquire.
withMaybe :: Maybe x -> (x -> (y -> IO r) -> IO r) -> (Maybe y -> IO r) -> IO r
withMaybe Nothing _ k = k Nothing
withMaybe (Just x) with k = with x (k . Just)

{- | A seed parser's own @--help@ text, as @config --help@ prints it: what
@GET \/help\/seed@ answers, the one non-generic surface the server has.
-}
seedHelpText :: Parser seed -> Text
seedHelpText p =
    case execParserPure defaultPrefs (info (p <**> helper) briefDesc) ["--help"] of
        Failure failure -> Text.pack (fst (renderFailure failure "config"))
        Success _ -> ""
        CompletionInvoked _ -> ""

{- | Runs a seed's own command-line parser over the arguments of one @run
serve@ declaration — i.e. the same words that would follow @config@ on an
actual command line.
-}
parseSeedArgs :: forall seed. (ParseRecord seed) => [String] -> Either Text seed
parseSeedArgs args =
    case execParserPure defaultPrefs (info parseRecord briefDesc) args of
        Success seed -> Right seed
        Failure failure -> Left (Text.pack $ fst $ renderFailure failure "config")
        CompletionInvoked _ -> Left "unexpected shell-completion request"

updownOnReport ::
    Reporter (UpDown.Report Extension) ->
    Reporter Op
updownOnReport r =
    ReporterM $ \op -> void $ UpDown.upTree r nat op
  where
    nat = pure . runIdentity

injectRemoteSubgraphs :: Int -> Op -> Op
injectRemoteSubgraphs lvl orig =
    orig `overlaid` flattenAllRemoteCalls orig

-- | A record for dynamic remote-op.
data RemoteOp = RemoteOp {unRemote :: Op}

flattenAllRemoteCalls :: Op -> Op
flattenAllRemoteCalls root =
    op "remote-call-details" (deps remoteCalls) id
  where
    remoteCalls = concatMap adapt $ collectDynamics root
    adapt :: (Op, [RemoteOp]) -> [Op]
    adapt (orig, remotes) = [unRemote r | r <- remotes]
