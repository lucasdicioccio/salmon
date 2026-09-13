{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Salmon.Builtin.CommandLine where

import Control.Monad (void, when)
import Control.Monad.Identity
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBysteString
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Options.Applicative
import qualified Options.Applicative
import Options.Generic
import System.Exit (exitFailure)
import System.IO (stdin)

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
import Salmon.Actions.Help as Help
import qualified Salmon.Actions.Query as Query
import qualified Salmon.Actions.Serve as Serve
-- 'CheckResult' constructors are hidden: 'Success'/'Failure' collide with
-- optparse-applicative's 'ParserResult' ones, which this module pattern
-- matches on. Nothing here needs a 'CheckResult'.
import Salmon.Actions.UpDown as UpDown hiding (Failure, Success)
import Salmon.Builtin.Extension
import Salmon.Reporter

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
      RunUp !(Maybe FilePath) !Bool
    | RunDown
    | RunTree
    | RunDAG
    | -- | @run serve@, optionally capping how many nodes converge at once
      -- per pass (R6 in @specs/per-node-state-machines-remaining.md@;
      -- 'Nothing' is unbounded, matching every version of @serve@ before
      -- this flag existed), and optionally starting with @autoconverge@ off
      -- (@--no-autoconverge@; 'False' is the default, matching every
      -- version of @serve@ before the setting existed).
      RunServe !(Maybe Int) !Bool
    deriving (Eq, Ord, Generic, Show)

instance FromJSON RunCommand
instance ToJSON RunCommand

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
            , command "down" (info (pure RunDown) (progDesc "Tears down (down) the directive on stdin."))
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
        (Run (RunUp Nothing _)) -> do
            result <- withGraph (runUp Set.empty)
            when (result == Just False) exitFailure
        (Run (RunUp (Just planPath) forceStale)) -> do
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
                            then runUp (Set.fromList (Query.planExcludedRefs plan)) op
                            else
                                if forceStale
                                    then do
                                        putStrLn $
                                            "warning: plan digest mismatch (plan expects "
                                                <> Text.unpack expected
                                                <> ", this directive hashes to "
                                                <> Text.unpack actual
                                                <> "); proceeding due to --force-stale-plan"
                                        runUp (Set.fromList (Query.planExcludedRefs plan)) op
                                    else do
                                        putStrLn $
                                            "refusing to run stale plan: plan expects digest "
                                                <> Text.unpack expected
                                                <> ", but this directive hashes to "
                                                <> Text.unpack actual
                                        exitFailure
            when (result == Just False) exitFailure
        (Run RunDown) -> do
            result <- withGraph runDown
            when (result == Just False) exitFailure
        (Run RunTree) -> do
            -- (R4): the computed 'Dag' is what @run up@ would actually walk
            -- once any "Salmon.Op.Rewrite" phases are registered; with none
            -- registered `computed` is the declared graph, still collapsed
            -- to one line per 'Ref' rather than one per path.
            void $ withGraph (\op -> computedTreeDag op >>= Help.printDagTree)
        (Run RunDAG) -> do
            void $ withGraph (\op -> computedTreeDag (injectRemoteSubgraphs 0 op) >>= Dot.printDagCograph)
        (Run (RunServe maxConcurrency noAutoConverge)) -> do
            limit <- traverse Concurrency.newConcurrencyLimit maxConcurrency
            void $ Serve.serveWith rewrites limit (not noAutoConverge) serveR r parseSeedArgs genBase traceBase stdin
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

    {- | @run up@: everything in this one directive's graph is wanted up, so
    that is the rewrites' 'phaseDesired'. @excluded@ (a plan's skipped
    refs) is what they must not collect: batching a node the operator asked
    to skip would run it anyway, under another node's name.

    Exclusion is a 'UpDown.Gate' rather than 'Query.forceSkip' precisely so it
    composes with collections — a batch is worth running iff some member of
    it is, which is the same 'Rewrite.membersOf' translation @serve@'s gate
    does. The report stream is identical either way: both produce a 'Skip'. -}
    runUp :: Set Ref -> Op -> IO Bool
    runUp excluded op = do
        dag <- UpDown.expandDag r nat op
        let computed = Rewrite.rewrite rewrites (Phase (Set.fromList (Dag.dagOrder dag)) excluded) dag
        UpDown.upDag (excluding computed excluded) r (Rewrite.computedDag computed)

    {- | @run down@: nothing is wanted up, which is what makes a
    direction-aware rewrite emit a teardown batch here and an install batch
    under @run up@, from the same registered phase. -}
    runDown :: Op -> IO Bool
    runDown op = do
        dag <- UpDown.expandDag r nat op
        let computed = Rewrite.rewrite rewrites (Phase Set.empty Set.empty) dag
        UpDown.downDag UpDown.alwaysRequired r (Rewrite.computedDag computed)

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
