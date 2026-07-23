{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Initializer where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Options.Applicative (execParser, fullDesc, header, info, progDesc)
import Options.Generic (ParseRecord (..))

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension (Track')
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (reportPrint)

import qualified SreBox.Initialize as Initialize

main :: IO ()
main = do
    let desc = fullDesc <> progDesc "Runs the local-machine salmon setup (sudoers, salmon user/group)" <> header "for Salmon"
    let opts = info parseRecord desc
    cmd <- execParser opts
    CLI.execCommandOrSeed reportPrint configure program cmd

data Seed = Seed
    deriving (Eq, Show, Generic)

instance FromJSON Seed
instance ToJSON Seed
instance ParseRecord Seed

data Spec = Spec
    deriving (Eq, Show, Generic)

instance FromJSON Spec
instance ToJSON Spec

program :: Track' Spec
program = Track $ \_ -> Initialize.initialize reportPrint

configure :: Configure IO Seed Spec
configure = Configure $ \_ -> pure Spec
