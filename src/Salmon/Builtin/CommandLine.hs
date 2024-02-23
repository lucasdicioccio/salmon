{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Salmon.Builtin.CommandLine where

import Control.Monad.Identity
import qualified Data.ByteString.Lazy as LBysteString
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Options.Generic
import Options.Applicative

import Salmon.Op.Eval
import Salmon.Op.Track
import Salmon.Op.Configure

import Salmon.Actions.Help as Help
import Salmon.Actions.UpDown as UpDown
import Salmon.Actions.Check as Check
import Salmon.Actions.Dot as Dot

import Salmon.Builtin.Extension

data Command seed
  = Config seed
  | Run BaseCommand
  deriving (Eq, Ord, Generic, Show)

data BaseCommand
  = Up
  | Down
  | Tree
  | DAG
  deriving (Eq, Ord, Generic, Show, Read)

argForBaseCommand :: BaseCommand -> Text
argForBaseCommand = \case
  Up -> "up"
  Down -> "down"
  Tree -> "tree"
  DAG -> "dag"

instance (ParseRecord seed) => ParseRecord (Command seed) where
  parseRecord =
      combo <**> helper
     where
       combo = subparser $ mconcat
           [ command "config" (info (Config <$> parseRecord) cfg)
           , command "run" (info (Run <$> parseRecord) run)
           , commandGroup "Salmon Commands."
           ]
       cfg = progDesc "Prints a config."
       run = progDesc "Runs a config."

instance FromJSON seed => FromJSON (Command seed)
instance ToJSON seed => ToJSON (Command seed)

instance FromJSON BaseCommand
instance ToJSON BaseCommand
instance ParseRecord BaseCommand
instance ParseField BaseCommand
instance ParseFields BaseCommand

-- | Function to combine a configuration system (based on a seed).
-- todo: consider adding some non-det when the graph depends not just on a seed but also on reading a variable in the directive
-- - either at the configure step: then the seed must contain enough to build the ops
-- - either in the expand phase from the directive
execCommandOrSeed
  :: forall directive seed. (ToJSON directive, FromJSON directive)
  => Configure IO seed directive
  -> Track' directive
  -> Command seed
  -> IO ()
execCommandOrSeed genBase traceBase cmd = void $ do
  case cmd of
    (Run Up) -> do
      withGraph (UpDown.upTree nat)
    (Run Down) -> do
      withGraph (UpDown.downTree nat) 
    (Run Tree) -> do
      withGraph (Help.printHelpCograph . (runIdentity . expand))
    (Run DAG) -> do
      withGraph (Dot.printCograph . (runIdentity . expand))
    Config seed -> do
      dir <- gen genBase seed
      LBysteString.putStr $ encode dir
  where
    nat = pure . runIdentity
    withGraph cont = do
      jsonbody <- LBysteString.getContents
      case eitherDecode jsonbody of
        Left err -> putStrLn ("failed to json-parse graph: " <> err)
        Right a -> do
          cont (run traceBase a)

-- | Function to build a main reading a base command and a directive from
-- stdin.
execBaseCommand
  :: forall directive seed. (FromJSON directive)
  => Track' directive
  -> BaseCommand
  -> IO ()
execBaseCommand traceBase cmd = void $ do
  case cmd of
    (Up) -> do
      withGraph (UpDown.upTree nat)
    (Down) -> do
      withGraph (UpDown.downTree nat) 
    (Tree) -> do
      withGraph (Help.printHelpCograph . (runIdentity . expand))
    (DAG) -> do
      withGraph (Dot.printCograph . (runIdentity . expand))
  where
    nat = pure . runIdentity
    withGraph cont = do
      jsonbody <- LBysteString.getContents
      case eitherDecode jsonbody of
        Left err -> putStrLn ("failed to json-parse graph: " <> err)
        Right a -> do
          cont (run traceBase a)
