{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative (command, execParser, fullDesc, header, help, helper, info, progDesc, strArgument, subparser, (<**>))
import Options.Generic (Generic, ParseRecord (..))

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension (Extension, Op, Track', Tracked', deps, ignoreTrack, notes, op, realNoop, ref)
import qualified Salmon.Builtin.Nodes.Cabal as Cabal
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Git as Git
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (dotRef)
import Salmon.Op.Track (Track (..), Tracked (..), bindTracked, opGraph)
import Salmon.Reporter
import qualified SreBox.CabalBuilding as CabalBuilding
import System.FilePath ((</>))

-------------------------------------------------------------------------------
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
successfulBuildTag :: CabalBuilding.CloneDir -> Git.Remote -> Git.Remote -> CabalBuilding.BranchName -> Op
successfulBuildTag dirname remoteForCloning remoteForTagging branch =
    dopush
  where
    dopush :: Op
    dopush =
        Git.push
            reportPrint
            Debian.git
            mktherepo
            therepo
            remoteForTagging
            (Git.RemoteName "salmon-tag-remote")
            (op "changes-to-push" (deps [dotag]) id)

    dotag :: Op
    dotag =
        Git.tag
            reportPrint
            Debian.git
            mktherepo
            therepo
            (Git.TagName "salmon-build")
            (Just "successul build")

    mktherepo :: Track' Git.Repo
    mktherepo = ignoreTrack

    therepo :: Git.Repo
    therepo = Git.Repo "./git-repos/" dirname remoteForCloning (Git.Branch branch)

reportWithTag :: CabalBuilding.CloneDir -> Git.Remote -> Git.Remote -> CabalBuilding.BranchName -> Reporter CabalBuilding.Report
reportWithTag dirname remoteForCloning remoteForTagging branch =
    reportBoth reportPrint applyTagOnSuccess
  where
    applyTagOnSuccess :: Reporter CabalBuilding.Report
    applyTagOnSuccess = reportIf CabalBuilding.isBuildSuccess applyTag

    applyTag :: Reporter CabalBuilding.Report
    applyTag = contramap (const mkTag) (CLI.updownOnReport reportPrint)

    mkTag :: Op
    mkTag =
        successfulBuildTag
            dirname
            remoteForCloning
            remoteForTagging
            branch

-------------------------------------------------------------------------------
microdns :: Prefs -> Tracked' FilePath
microdns p =
    CabalBuilding.microDNS r (p.bindir "microdns")
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportWithTag
            "microdns"
            (Git.Remote "https://github.com/lucasdicioccio/microdns.git")
            (Git.Remote "git@github.com:lucasdicioccio/microdns.git")
            "main"

kitchenSink :: Prefs -> Tracked' FilePath
kitchenSink p =
    CabalBuilding.kitchenSink r (p.bindir "kitchen-sink")
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportWithTag
            "kitchensink"
            (Git.Remote "https://github.com/kitchensink-tech/kitchensink.git")
            (Git.Remote "git@github.com:kitchensink-tech/kitchensink.git")
            "main"

postgrest :: Prefs -> Tracked' FilePath
postgrest p =
    CabalBuilding.cabalRepoBuild
        reportPrint
        "postgrest"
        (p.bindir "postgrest")
        (Debian.deb $ Debian.Package "libpq-dev")
        "exe:postgrest"
        "postgrest"
        (Git.Remote "https://github.com/PostgREST/postgrest.git")
        "main"
        ""
        []

prodapi :: Prefs -> Op
prodapi p =
    CabalBuilding.cabalRepoOnlyBuild
        r
        "prodapi"
        (p.bindir "prodapi")
        realNoop
        "prodapi"
        (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
        "master"
        ""
        []
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportWithTag
            "prodapi"
            (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
            (Git.Remote "git@github.com:lucasdicioccio/prodapi.git")
            "master"

acmeNotAJoke :: Prefs -> Op
acmeNotAJoke p =
    CabalBuilding.cabalRepoOnlyBuild
        r
        "acme-not-a-joke"
        (p.bindir "acme-not-a-joke")
        realNoop
        "acme-not-a-joke"
        (Git.Remote "https://github.com/lucasdicioccio/acme-not-a-joke.git")
        "main"
        ""
        []
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportWithTag
            "acme-not-a-joke"
            (Git.Remote "https://github.com/lucasdicioccio/acme-not-a-joke.git")
            (Git.Remote "git@github.com:lucasdicioccio/acme-not-a-joke.git")
            "main"

minizincProcess :: Prefs -> Op
minizincProcess p =
    CabalBuilding.cabalRepoOnlyBuild
        r
        "minizinc-process"
        (p.bindir "minizinc-process")
        realNoop
        "minizinc-process"
        (Git.Remote "https://github.com/lucasdicioccio/minizinc-process.git")
        "master"
        ""
        []
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportWithTag
            "minizinc-process"
            (Git.Remote "https://github.com/lucasdicioccio/minizinc-process.git")
            (Git.Remote "git@github.com:lucasdicioccio/minizinc-process.git")
            "master"

sqq :: Prefs -> Tracked' FilePath
sqq p =
    CabalBuilding.cabalRepoBuild
        r
        "sqq"
        (p.bindir "sqq")
        (Debian.deb $ Debian.Package "sqlite3-dev")
        "sqq"
        "sqq"
        (Git.Remote "https://github.com/lucasdicioccio/sqq.git")
        "main"
        ""
        []
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportWithTag
            "sqq"
            (Git.Remote "https://github.com/lucasdicioccio/sqq.git")
            (Git.Remote "git@github.com:lucasdicioccio/sqq.git")
            "master"

salmon :: Prefs -> Tracked' FilePath
salmon p =
    CabalBuilding.cabalRepoBuild
        reportPrint
        "salmon"
        (p.bindir "salmon")
        realNoop
        "salmon"
        "exe:cibuilds-salmon"
        (Git.Remote "https://github.com/lucasdicioccio/salmon.git")
        "master"
        ""
        [Cabal.AllowNewer]
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportWithTag
            "salmon"
            (Git.Remote "https://github.com/lucasdicioccio/salmon.git")
            (Git.Remote "git@github.com:lucasdicioccio/salmon.git")
            "master"

fourmolu :: Prefs -> Tracked' FilePath
fourmolu p =
    CabalBuilding.cabalRepoBuild
        reportPrint
        "fourmolu"
        (p.bindir "fourmolu")
        (realNoop)
        "exe:fourmolu"
        "fourmolu"
        (Git.Remote "https://github.com/fourmolu/fourmolu.git")
        "main"
        ""
        []

swarmRoot :: Prefs -> Tracked' FilePath
swarmRoot p =
    CabalBuilding.cabalRepoBuild
        reportPrint
        "swarmroot"
        (p.bindir "swarm")
        (realNoop)
        "swarm"
        "swarm"
        (Git.Remote "https://github.com/swarm-game/swarm.git")
        "main"
        ""
        []

mustache :: Prefs -> Tracked' FilePath
mustache p =
    CabalBuilding.cabalRepoBuild
        reportPrint
        "mustache"
        (p.bindir "mustache")
        (realNoop)
        "mustache"
        "haskell-mustache"
        (Git.Remote "https://github.com/JustusAdam/mustache.git")
        "master"
        ""
        []

duckling :: Prefs -> Tracked' FilePath
duckling p =
    CabalBuilding.cabalRepoBuild
        reportPrint
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
    | AcmeNotAJoke
    | MinizincProcess
    | Sqq
    | PostgREST
    | Fourmolu
    | Mustache
    | SwarmRoot
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
        optimizedDeps $ op "program" (deps $ specOp (n + 1) spec) $ \actions ->
            actions
                { notes = [Text.pack $ "at depth " <> show n]
                , ref = dotRef $ Text.pack $ "program:" <> show n
                }

    specOp :: Int -> Spec -> [Op]
    -- meta
    specOp k (Batch xs) = concatMap (specOp k) xs
    -- machine
    specOp k (HaskellBuild h MicroDNS) = [opGraph $ microdns (homedir h)]
    specOp k (HaskellBuild h KitchenSink) = [opGraph $ kitchenSink (homedir h)]
    specOp k (HaskellBuild h ProdAPI) = [prodapi (homedir h)]
    specOp k (HaskellBuild h Salmon) = [opGraph $ salmon (homedir h)]
    specOp k (HaskellBuild h MinizincProcess) = [minizincProcess (homedir h)]
    specOp k (HaskellBuild h Sqq) = [opGraph $ sqq (homedir h)]
    specOp k (HaskellBuild h AcmeNotAJoke) = [acmeNotAJoke (homedir h)]
    specOp k (HaskellBuild h PostgREST) = [opGraph $ postgrest (homedir h)]
    specOp k (HaskellBuild h Mustache) = [opGraph $ mustache (homedir h)]
    specOp k (HaskellBuild h SwarmRoot) = [opGraph $ swarmRoot (homedir h)]
    specOp k (HaskellBuild h Fourmolu) = [opGraph $ fourmolu (homedir h)]
    specOp k (HaskellBuild h Duckling) = [opGraph $ duckling (homedir h)]

    optimizedDeps :: Op -> Op
    optimizedDeps base =
        let pkgs = Debian.installAllDebsAtOnce base
         in Debian.removeSinglePackages base `inject` pkgs

-------------------------------------------------------------------------------
data Seed
    = BuildSeed {binroot :: BinDir, buildName :: Text}

instance ParseRecord Seed where
    parseRecord =
        combo <**> helper
      where
        combo =
            subparser $
                mconcat
                    [ command "build" (info build (progDesc "makes a build"))
                    ]
        build =
            BuildSeed
                <$> strArgument (Options.Applicative.help "bin-root")
                <*> strArgument (Options.Applicative.help "build-name")

configure :: Configure IO Seed Spec
configure = Configure go
  where
    go :: Seed -> IO Spec
    go (BuildSeed h "kitchensink:hs") = pure $ HaskellBuild h KitchenSink
    go (BuildSeed h "microdns:hs") = pure $ HaskellBuild h MicroDNS
    go (BuildSeed h "prodapi:hs") = pure $ HaskellBuild h ProdAPI
    go (BuildSeed h "salmon:hs") = pure $ HaskellBuild h Salmon
    go (BuildSeed h "minizinc-process:hs") = pure $ HaskellBuild h MinizincProcess
    go (BuildSeed h "sqq:hs") = pure $ HaskellBuild h Sqq
    go (BuildSeed h "acme-not-a-joke:hs") = pure $ HaskellBuild h AcmeNotAJoke
    go (BuildSeed h "postgrest:hs") = pure $ HaskellBuild h PostgREST
    go (BuildSeed h "mustache:hs") = pure $ HaskellBuild h Mustache
    go (BuildSeed h "swarmroot:hs") = pure $ HaskellBuild h SwarmRoot
    go (BuildSeed h "fourmolu:hs") = pure $ HaskellBuild h Fourmolu
    go (BuildSeed h "duckling:hs") = pure $ HaskellBuild h Duckling
    go (BuildSeed h ":mine") =
        pure $
            Batch
                [ HaskellBuild h ProdAPI
                , HaskellBuild h AcmeNotAJoke
                , HaskellBuild h MicroDNS
                , HaskellBuild h KitchenSink
                , HaskellBuild h Salmon
                , HaskellBuild h MinizincProcess
                , HaskellBuild h Sqq
                ]
    go (BuildSeed h ":important") =
        pure $
            Batch
                [ HaskellBuild h ProdAPI
                , HaskellBuild h AcmeNotAJoke
                , HaskellBuild h MicroDNS
                , HaskellBuild h KitchenSink
                , HaskellBuild h Salmon
                , HaskellBuild h MinizincProcess
                , HaskellBuild h Mustache
                , HaskellBuild h PostgREST
                , HaskellBuild h Fourmolu
                ]
    go (BuildSeed h ":fun") =
        pure $
            Batch
                [ HaskellBuild h SwarmRoot
                , HaskellBuild h Duckling
                ]

-------------------------------------------------------------------------------
main :: IO ()
main = do
    let desc = fullDesc <> progDesc "Some builds." <> header "for dicioccio.fr"
    let opts = info parseRecord desc
    cmd <- execParser opts
    CLI.execCommandOrSeed reportPrint configure program cmd
