{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Salmon.Builtin.CommandLine where

import Control.Monad (void, when)
import Control.Monad.Identity
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBysteString
import qualified Data.Text as Text
import Options.Applicative
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
import qualified Salmon.Actions.Serve as Serve
import Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import Salmon.Reporter

data Command seed
    = Config seed
    | Run BaseCommand
    deriving (Eq, Ord, Generic, Show)

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

instance (ParseRecord seed) => ParseRecord (Command seed) where
    parseRecord =
        combo <**> helper
      where
        combo =
            hsubparser $
                mconcat
                    [ command "config" (info (Config <$> parseRecord) cfg)
                    , command "run" (info (Run <$> parseRecord) run)
                    , commandGroup "Salmon Commands."
                    ]
        cfg = progDesc "Prints a config."
        run = progDesc "Runs a config."

instance (FromJSON seed) => FromJSON (Command seed)
instance (ToJSON seed) => ToJSON (Command seed)

instance FromJSON BaseCommand
instance ToJSON BaseCommand
instance ParseRecord BaseCommand
instance ParseField BaseCommand
instance ParseFields BaseCommand

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
        (Run Up) -> do
            result <- withGraph (UpDown.upTree r nat)
            when (result == Just False) exitFailure
        (Run Down) -> do
            result <- withGraph (UpDown.downTree r nat)
            when (result == Just False) exitFailure
        (Run Tree) -> do
            void $ withGraph (Help.printHelpCograph . (runIdentity . expand))
        (Run DAG) -> do
            void $ withGraph (Dot.printCograph . (runIdentity . expand) . injectRemoteSubgraphs 0)
        (Run Serve) -> do
            void $ Serve.serve serveR r parseSeedArgs genBase traceBase stdin
        Config seed -> do
            dir <- gen genBase seed
            LBysteString.putStr $ encode dir
  where
    nat = pure . runIdentity

    -- | 'Nothing' iff the incoming JSON graph failed to parse (in which case @cont@ never ran).
    withGraph :: (Op -> IO a) -> IO (Maybe a)
    withGraph cont = do
        jsonbody <- LBysteString.getContents
        case eitherDecode jsonbody of
            Left err -> do
                putStrLn ("failed to json-parse graph: " <> err)
                pure Nothing
            Right a -> do
                Just <$> cont (run traceBase a)

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
