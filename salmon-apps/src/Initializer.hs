{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Initializer where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Options.Applicative (execParser, fullDesc, header, helper, info, long, optional, progDesc, strOption, (<**>))
import qualified Options.Applicative
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

newtype Seed = Seed
    { authorizedKeyFile :: Maybe FilePath
    }
    deriving (Eq, Show, Generic)

instance FromJSON Seed
instance ToJSON Seed

instance ParseRecord Seed where
    parseRecord =
        build <**> helper
      where
        build =
            Seed
                <$> optional
                    ( strOption
                        ( long "authorized-key-file"
                            <> Options.Applicative.help "path to an SSH public key to authorize for the salmon user"
                        )
                    )

newtype Spec = Spec
    { authorizedKey :: Maybe Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON Spec
instance ToJSON Spec

program :: Track' Spec
program = Track $ \spec -> Initialize.initialize reportPrint spec.authorizedKey

configure :: Configure IO Seed Spec
configure = Configure $ \seed -> do
    mbKey <- traverse readKeyFile seed.authorizedKeyFile
    pure (Spec mbKey)
  where
    readKeyFile :: FilePath -> IO Text
    readKeyFile path = Text.strip . Text.pack <$> readFile path
