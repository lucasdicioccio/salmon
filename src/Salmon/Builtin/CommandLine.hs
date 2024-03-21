{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Salmon.Builtin.CommandLine where

import Control.Monad (void)
import Control.Monad.Identity
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBysteString
import Options.Applicative
import Options.Generic

import Salmon.Op.Configure
import Salmon.Op.Eval
import Salmon.Op.OpGraph
import Salmon.Op.Track

import Salmon.Actions.Check as Check
import Salmon.Actions.Dot as Dot
import Salmon.Actions.Help as Help
import Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import Salmon.Reporter

data Command seed
    = Config seed
    | Run BaseCommand
    deriving (Eq, Ord, Generic, Show)

data BaseCommand
    = Up
    | Tree
    | DAG
    deriving (Eq, Ord, Generic, Show, Read)

argForBaseCommand :: BaseCommand -> Text
argForBaseCommand = \case
    Up -> "up"
    Tree -> "tree"
    DAG -> "dag"

instance (ParseRecord seed) => ParseRecord (Command seed) where
    parseRecord =
        combo <**> helper
      where
        combo =
            subparser $
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
    (ToJSON directive, FromJSON directive) =>
    Reporter (UpDown.Report Extension) ->
    Configure IO seed directive ->
    Track' directive ->
    Command seed ->
    IO ()
execCommandOrSeed r genBase traceBase cmd = void $ do
    case cmd of
        (Run Up) -> do
            withGraph (UpDown.upTree r nat)
        (Run Tree) -> do
            withGraph (Help.printHelpCograph . (runIdentity . expand))
        (Run DAG) -> do
            withGraph (Dot.printCograph . (runIdentity . expand) . injectRemoteSubgraphs 0)
        Config seed -> do
            dir <- gen genBase seed
            LBysteString.putStr $ encode dir
  where
    nat = pure . runIdentity

    withGraph :: (Op -> IO ()) -> IO ()
    withGraph cont = do
        jsonbody <- LBysteString.getContents
        case eitherDecode jsonbody of
            Left err -> putStrLn ("failed to json-parse graph: " <> err)
            Right a -> do
                cont (run traceBase a)

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
