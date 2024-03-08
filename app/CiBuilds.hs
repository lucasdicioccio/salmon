{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Aeson (FromJSON, ToJSON, encode)
import Options.Generic (Generic,ParseRecord(..))
import Options.Applicative (help,helper,info,fullDesc,progDesc,strArgument,header,execParser,command,(<**>), subparser)
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified SreBox.CabalBuilding as CabalBuilding
import Salmon.Builtin.Extension (Op,ref,notes,Track',op,deps)
import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Op.Configure (Configure(..))
import Salmon.Op.Track (Track(..),opGraph)
import Salmon.Op.Ref (dotRef)
import Salmon.Op.OpGraph (inject)

-------------------------------------------------------------------------------
data HaskellBuild
  = KitchenSink
  | MicroDNS
  deriving (Generic)
instance FromJSON HaskellBuild
instance ToJSON HaskellBuild

data Spec
  = Batch [Spec]
  | HaskellBuild HaskellBuild
  deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec

program :: Track' Spec
program =
   go 0
  where
   go n = Track $ \spec ->
     optimizedDeps $ op "program" (deps $ specOp (n+1) spec) $ \actions -> actions {
       notes = [Text.pack $ "at depth " <> show n]
     , ref = dotRef $ Text.pack $ "program:" <> show n
     }

   specOp :: Int -> Spec -> [Op]
   -- meta
   specOp k (Batch xs) = concatMap (specOp k) xs
   -- machine
   specOp k (HaskellBuild MicroDNS) = [opGraph $ CabalBuilding.microDNS "./microdns"]
   specOp k (HaskellBuild KitchenSink) = [opGraph $ CabalBuilding.kitchenSink "./kitchen-sink"]

   optimizedDeps :: Op -> Op
   optimizedDeps base =
     let pkgs = Debian.installAllDebsAtOnce base
     in Debian.removeSinglePackages base `inject` pkgs

-------------------------------------------------------------------------------
data Seed
  = BuildSeed { buildName :: Text }

instance ParseRecord Seed where
  parseRecord =
      combo <**> helper
    where
      combo =
        subparser $ mconcat
          [ command "build" (info build (progDesc "makes a build"))
          ]
      build = BuildSeed <$> strArgument (Options.Applicative.help "build-name")

configure :: Configure IO Seed Spec
configure = Configure go
  where
    go :: Seed -> IO Spec
    go (BuildSeed "kitchensink:hs") = pure $  HaskellBuild KitchenSink
    go (BuildSeed "microdns:hs") = pure $  HaskellBuild MicroDNS
    go (BuildSeed ":all") = pure $ Batch [ HaskellBuild MicroDNS, HaskellBuild KitchenSink ]

-------------------------------------------------------------------------------
main :: IO ()
main = do
  let desc = fullDesc <> progDesc "Some builds." <> header "for dicioccio.fr"
  let opts = info parseRecord desc
  cmd <- execParser opts
  CLI.execCommandOrSeed configure program cmd

