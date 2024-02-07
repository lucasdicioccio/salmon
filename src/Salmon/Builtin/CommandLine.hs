{-# LANGUAGE DeriveGeneric #-}


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
  deriving (Generic, Show)

data BaseCommand
  = Up
  | Down
  | Tree
  | DAG
  deriving (Generic, Show, Read)

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

execCommand
  :: forall directive seed. (ToJSON directive, FromJSON directive)
  => Configure' seed directive
  -> Track' directive
  -> Command seed
  -> IO ()
execCommand genBase traceBase cmd = void $ do
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
      LBysteString.putStr $ encode $ runIdentity $ gen genBase seed
  where
    nat = pure . runIdentity
    withGraph cont = do
      jsonbody <- LBysteString.getContents
      case eitherDecode jsonbody of
        Left err -> putStrLn ("failed to json-parse graph: " <> err)
        Right a -> do
          cont (run traceBase a)