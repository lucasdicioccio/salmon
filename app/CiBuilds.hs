{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Aeson (FromJSON, ToJSON, encode)
import Options.Generic (Generic,ParseRecord(..))
import Options.Applicative (help,helper,info,fullDesc,progDesc,strArgument,header,execParser,command,(<**>), subparser)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import qualified Salmon.Builtin.Nodes.Cabal as Cabal
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Git as Git
import qualified SreBox.CabalBuilding as CabalBuilding
import Salmon.Builtin.Extension (Op,ref,notes,Track',op,deps,realNoop)
import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Op.Configure (Configure(..))
import Salmon.Op.Track (Track(..),opGraph)
import Salmon.Op.Ref (dotRef)
import Salmon.Op.OpGraph (inject)

data Prefs
  = Prefs
  { bindir :: FilePath -> FilePath
  }

homedir :: BinDir -> Prefs
homedir h =
  Prefs
   bindir
  where
  bindir :: FilePath -> FilePath
  bindir x =
    h </> "ci" </> x

-------------------------------------------------------------------------------
microDNS p = CabalBuilding.microDNS (p.bindir "microdns")

kitchenSink p = CabalBuilding.kitchenSink (p.bindir "kitchen-sink")

postgrest p = 
  CabalBuilding.cabalRepoBuild
   "postgrest"
   (p.bindir "postgrest")
   (Debian.deb $ Debian.Package "libpq-dev")
   "exe:postgrest"
   "postgrest"
   (Git.Remote "https://github.com/PostgREST/postgrest.git")
   "main"
   ""
   []

prodapi p = CabalBuilding.cabalRepoBuild
  "prodapi"
  (p.bindir "prodapi")
  realNoop
  "prodapi"
  "prodapi-example-exe"
  (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
  "master"
  ""
  []

salmon p = CabalBuilding.cabalRepoBuild
  "salmon"
  (p.bindir "salmon")
  realNoop
  "salmon"
  "srebox-salmon"
  (Git.Remote "https://github.com/lucasdicioccio/salmon.git")
  "master"
  ""
  [Cabal.AllowNewer]

duckling p = CabalBuilding.cabalRepoBuild
  "duckling"
  (p.bindir "duckling")
  (Debian.deb $ Debian.Package "libpcre3-dev")
  "duckling"
  "duckling-example-exe"
  (Git.Remote "https://github.com/facebook/duckling.git")
  "main"
  ""
  [Cabal.AllowNewer]

-------------------------------------------------------------------------------
type BinDir = FilePath

data HaskellBuild
  = KitchenSink
  | MicroDNS
  | ProdAPI
  | Salmon
  | PostgREST
  | Duckling
  deriving (Generic)
instance FromJSON HaskellBuild
instance ToJSON HaskellBuild

data Spec
  = Batch [Spec]
  | HaskellBuild BinDir HaskellBuild
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
   specOp k (HaskellBuild h MicroDNS) = [opGraph $ microDNS (homedir h)]
   specOp k (HaskellBuild h KitchenSink) = [opGraph $ kitchenSink (homedir h)]
   specOp k (HaskellBuild h ProdAPI) = [opGraph $ prodapi (homedir h)]
   specOp k (HaskellBuild h Salmon) = [opGraph $ salmon (homedir h)]
   specOp k (HaskellBuild h PostgREST) = [opGraph $ postgrest (homedir h)]
   specOp k (HaskellBuild h Duckling) = [opGraph $ duckling (homedir h)]

   optimizedDeps :: Op -> Op
   optimizedDeps base =
     let pkgs = Debian.installAllDebsAtOnce base
     in Debian.removeSinglePackages base `inject` pkgs

-------------------------------------------------------------------------------
data Seed
  = BuildSeed { binroot :: BinDir , buildName :: Text }

instance ParseRecord Seed where
  parseRecord =
      combo <**> helper
    where
      combo =
        subparser $ mconcat
          [ command "build" (info build (progDesc "makes a build"))
          ]
      build = BuildSeed <$> strArgument (Options.Applicative.help "bin-root")
                        <*> strArgument (Options.Applicative.help "build-name")

configure :: Configure IO Seed Spec
configure = Configure go
  where
    go :: Seed -> IO Spec
    go (BuildSeed h "kitchensink:hs") = pure $  HaskellBuild h KitchenSink
    go (BuildSeed h "microdns:hs") = pure $  HaskellBuild h MicroDNS
    go (BuildSeed h "prodapi:hs") = pure $  HaskellBuild h ProdAPI
    go (BuildSeed h "salmon:hs") = pure $  HaskellBuild h Salmon
    go (BuildSeed h "postgrest:hs") = pure $  HaskellBuild h PostgREST
    go (BuildSeed h "duckling:hs") = pure $  HaskellBuild h Duckling
    go (BuildSeed h ":all") =
      pure $ Batch
        [ HaskellBuild h MicroDNS
        , HaskellBuild h KitchenSink
        , HaskellBuild h ProdAPI
        , HaskellBuild h Salmon
        , HaskellBuild h PostgREST
        , HaskellBuild h Duckling
        ]

-------------------------------------------------------------------------------
main :: IO ()
main = do
  let desc = fullDesc <> progDesc "Some builds." <> header "for dicioccio.fr"
  let opts = info parseRecord desc
  cmd <- execParser opts
  CLI.execCommandOrSeed configure program cmd
