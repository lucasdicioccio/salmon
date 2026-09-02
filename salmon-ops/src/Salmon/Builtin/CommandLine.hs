{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Salmon.Builtin.CommandLine where

import Control.Monad (void, when)
import Control.Monad.Identity
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBysteString
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Options.Applicative
import qualified Options.Applicative
import Options.Generic
import System.Exit (exitFailure)
import System.IO (stdin)

import Salmon.Op.Configure
import Salmon.Op.Eval
import Salmon.Op.OpGraph
import Salmon.Op.Track

import Salmon.Actions.Check as Check
import Salmon.Actions.Dot as Dot
import Salmon.Actions.Help as Help
import qualified Salmon.Actions.Query as Query
import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.UpDown as UpDown
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
    | RunServe
    deriving (Eq, Ord, Generic, Show)

instance FromJSON RunCommand
instance ToJSON RunCommand

data QueryCommand
    = -- | @query show@: annotate the directive's tree with [selected]/[excluded].
      QueryShow !QuerySelection
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
            , command "serve" (info (pure RunServe) (progDesc "Reads a stream of seed declarations on stdin and converges."))
            ]
  where
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
    showP = QueryShow <$> selectionP
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
execCommandOrSeedWith serveR r genBase traceBase cmd = do
    case cmd of
        (Run (RunUp Nothing _)) -> do
            result <- withGraph (UpDown.upTree r nat)
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
                            then UpDown.upTree r nat (Query.forceSkip (Set.fromList (Query.planExcludedRefs plan)) op)
                            else
                                if forceStale
                                    then do
                                        putStrLn $
                                            "warning: plan digest mismatch (plan expects "
                                                <> Text.unpack expected
                                                <> ", this directive hashes to "
                                                <> Text.unpack actual
                                                <> "); proceeding due to --force-stale-plan"
                                        UpDown.upTree r nat (Query.forceSkip (Set.fromList (Query.planExcludedRefs plan)) op)
                                    else do
                                        putStrLn $
                                            "refusing to run stale plan: plan expects digest "
                                                <> Text.unpack expected
                                                <> ", but this directive hashes to "
                                                <> Text.unpack actual
                                        exitFailure
            when (result == Just False) exitFailure
        (Run RunDown) -> do
            result <- withGraph (UpDown.downTree r nat)
            when (result == Just False) exitFailure
        (Run RunTree) -> do
            void $ withGraph (Help.printHelpCograph . (runIdentity . expand))
        (Run RunDAG) -> do
            void $ withGraph (Dot.printCograph . (runIdentity . expand) . injectRemoteSubgraphs 0)
        (Run RunServe) -> do
            void $ Serve.serve serveR r parseSeedArgs genBase traceBase stdin
        (Query (QueryShow (QuerySelection sel exc))) -> do
            void $ withGraph $ \op -> do
                let cograph = runIdentity (expand op)
                let (selected, excluded) = Query.resolveSelectors cograph sel exc
                Query.printAnnotated cograph selected excluded
        (Query (QueryPlan (QuerySelection sel exc) embedDirective)) -> do
            void $ withGraphAndBytes $ \dirBytes op -> do
                let cograph = runIdentity (expand op)
                let (_, excluded) = Query.resolveSelectors cograph sel exc
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
