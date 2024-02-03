{-# LANGUAGE DeriveGeneric #-}


module Salmon.Builtin.CommandLine where

import Control.Monad.Identity
import qualified Data.ByteString.Lazy as LBysteString
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Options.Generic

import Salmon.Op.Eval
import Salmon.Op.Track
import Salmon.Op.Configure

import Salmon.Actions.Help as Help
import Salmon.Actions.UpDown as UpDown
import Salmon.Actions.Check as Check
import Salmon.Actions.Dot as Dot

import Salmon.Builtin.Extension

data Command seed
  = Config { seed :: seed }
  | Run OpCommand
  | Tree
  | DAG
  deriving (Generic, Show)

instance (ParseFields seed) => ParseRecord (Command seed)
instance FromJSON seed => FromJSON (Command seed)
instance ToJSON seed => ToJSON (Command seed)

data OpCommand
  = Up
  | Down
  deriving (Generic, Show, Read)

instance FromJSON OpCommand
instance ToJSON OpCommand
instance ParseRecord OpCommand
instance ParseField OpCommand
instance ParseFields OpCommand

execCommand
  :: forall directive seed. (ToJSON directive, FromJSON directive)
  => Configure' seed directive
  -> Track' directive
  -> Command seed
  -> IO ()
execCommand genBase traceBase cmd = void $ do
  case cmd of
    Run Up -> do
      withGraph (UpDown.upTree nat)
    Run Down -> do
      withGraph (UpDown.downTree nat) 
    Tree -> do
      withGraph (Help.printHelpCograph . (runIdentity . expand))
    DAG -> do
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
