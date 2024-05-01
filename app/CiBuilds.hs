{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Functor.Contravariant (Predicate (..))
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
import qualified Salmon.Builtin.Nodes.Spago as Spago
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
    , sdistdir :: FilePath -> FilePath
    }

homedir :: BinDir -> Prefs
homedir h =
    Prefs
        bindir
        sdistdir
  where
    bindir :: FilePath -> FilePath
    bindir x =
        h </> "ci" </> x
    sdistdir :: FilePath -> FilePath
    sdistdir x =
        h </> "sdist" </> x

-------------------------------------------------------------------------------
pushNewTag ::
    Git.TagName ->
    Text ->
    CabalBuilding.CloneDir ->
    Git.Remote ->
    Git.Remote ->
    Git.BranchName ->
    Op
pushNewTag tag tagtxt dirname remoteForCloning remoteForTagging branch =
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
            tag
            (Just tagtxt)

    mktherepo :: Track' Git.Repo
    mktherepo = ignoreTrack

    therepo :: Git.Repo
    therepo = Git.Repo "./git-repos/" dirname remoteForCloning (Git.Branch branch)

reportWithTag ::
    forall buildReport.
    Git.TagName ->
    CabalBuilding.CloneDir ->
    CabalBuilding.CloneDir ->
    Git.Remote ->
    Git.Remote ->
    Git.BranchName ->
    Predicate buildReport ->
    Reporter buildReport
reportWithTag tag tagtxt dirname remoteForCloning remoteForTagging branch predicate =
    reportWhen predicate applyTag
  where
    applyTag :: Reporter buildReport
    applyTag = contramap (const mkTag) (CLI.updownOnReport reportPrint)

    mkTag :: Op
    mkTag =
        pushNewTag
            tag
            tagtxt
            dirname
            remoteForCloning
            remoteForTagging
            branch

reportCabalWithTag ::
    Git.TagName ->
    CabalBuilding.CloneDir ->
    CabalBuilding.CloneDir ->
    Git.Remote ->
    Git.Remote ->
    Git.BranchName ->
    Reporter CabalBuilding.Report
reportCabalWithTag tag tagtxt dirname remoteForCloning remoteForTagging branch =
    reportBoth
        reportPrint
        (reportWithTag tag tagtxt dirname remoteForCloning remoteForTagging branch predicate)
  where
    predicate :: Predicate CabalBuilding.Report
    predicate = Predicate $ \x -> (CabalBuilding.isBuildSuccess x || CabalBuilding.isReleaseSuccess x)

reportSpagoWithTag ::
    Git.TagName ->
    CabalBuilding.CloneDir ->
    CabalBuilding.CloneDir ->
    Git.Remote ->
    Git.Remote ->
    Git.BranchName ->
    Reporter Spago.Report
reportSpagoWithTag tag tagtxt dirname remoteForCloning remoteForTagging branch =
    reportBoth
        reportPrint
        (reportWithTag tag tagtxt dirname remoteForCloning remoteForTagging branch predicate)
  where
    predicate :: Predicate Spago.Report
    predicate = Predicate Spago.isBuildSuccess

defaultTag :: Git.TagName
defaultTag = Git.TagName "salmon-build"

defaultTagText :: Text
defaultTagText = "successful build"

-------------------------------------------------------------------------------
microdns :: Prefs -> Tracked' FilePath
microdns p =
    CabalBuilding.microDNS r (p.bindir "microdns")
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            defaultTag
            defaultTagText
            "microdns"
            (Git.Remote "https://github.com/lucasdicioccio/microdns.git")
            (Git.Remote "git@github.com:lucasdicioccio/microdns.git")
            "main"

microdns__publish :: Prefs -> CabalBuilding.VersionString -> Op
microdns__publish p v =
    CabalBuilding.publishHackage
        r
        "microdns"
        (p.sdistdir "microdns")
        "microdns"
        "."
        v
        (Git.Remote "https://github.com/lucasdicioccio/microdns.git")
        "master"
        ""
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-published-microdns")
            defaultTagText
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
        reportCabalWithTag
            defaultTag
            defaultTagText
            "kitchensink"
            (Git.Remote "https://github.com/kitchensink-tech/kitchensink.git")
            (Git.Remote "git@github.com:kitchensink-tech/kitchensink.git")
            "main"

kitchenSink_bridge :: Prefs -> Tracked' FilePath
kitchenSink_bridge p =
    CabalBuilding.cabalRepoBuild
        r
        "kitchensink"
        (p.bindir "kitchen-sink")
        realNoop
        "kitchen-sink-purescript-bridge"
        "kitchen-sink-purescript-bridge"
        (Git.Remote "https://github.com/kitchensink-tech/kitchensink.git")
        "main"
        "hs"
        []
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-build-kitchensink-bridge")
            defaultTagText
            "kitchensink"
            (Git.Remote "https://github.com/kitchensink-tech/kitchensink.git")
            (Git.Remote "git@github.com:kitchensink-tech/kitchensink.git")
            "main"

kitchenSink_graphexplorer :: Prefs -> Op
kitchenSink_graphexplorer p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/kitchensink/purs/graphexplorer"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "kitchensink"
            (Git.Remote "https://github.com/kitchensink-tech/kitchensink.git")
            (Git.Branch "main")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build-kitchensink-graphexplorer")
            defaultTagText
            "kitchensink"
            (Git.Remote "https://github.com/kitchensink-tech/kitchensink.git")
            (Git.Remote "git@github.com:kitchensink-tech/kitchensink.git")
            "main"

kitchenSink_searchbox :: Prefs -> Op
kitchenSink_searchbox p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/kitchensink/purs/search-box"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "kitchensink"
            (Git.Remote "https://github.com/kitchensink-tech/kitchensink.git")
            (Git.Branch "main")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build-kitchensink-searchbox")
            defaultTagText
            "kitchensink"
            (Git.Remote "https://github.com/kitchensink-tech/kitchensink.git")
            (Git.Remote "git@github.com:kitchensink-tech/kitchensink.git")
            "main"

halogen_echarts_simple :: Prefs -> Op
halogen_echarts_simple p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/ps-halogen-echarts-simple"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "ps-halogen-echarts-simple"
            (Git.Remote "https://github.com/lucasdicioccio/purescript-halogen-echarts-simple.git")
            (Git.Branch "main")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build")
            defaultTagText
            "ps-halogen-echarts-simple"
            (Git.Remote "https://github.com/lucasdicioccio/purescript-halogen-echarts-simple.git")
            (Git.Remote "git@github.com:lucasdicioccio/purescript-halogen-echarts-simple.git")
            "main"

echarts_simple :: Prefs -> Op
echarts_simple p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/ps-echarts-simple"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "ps-echarts-simple"
            (Git.Remote "https://github.com/lucasdicioccio/purescript-echarts-simple.git")
            (Git.Branch "main")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build")
            defaultTagText
            "ps-echarts-simple"
            (Git.Remote "https://github.com/lucasdicioccio/purescript-echarts-simple.git")
            (Git.Remote "git@github.com:lucasdicioccio/purescript-echarts-simple.git")
            "main"

humdrum :: Prefs -> Op
humdrum p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/ps-humdrum"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "ps-humdrum"
            (Git.Remote "https://github.com/lucasdicioccio/purescript-humdrum.git")
            (Git.Branch "main")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build")
            defaultTagText
            "ps-humdrum"
            (Git.Remote "https://github.com/lucasdicioccio/purescript-humdrum.git")
            (Git.Remote "git@github.com:lucasdicioccio/purescript-humdrum.git")
            "main"

halogendemo :: Prefs -> Op
halogendemo p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/blog/purescript/halogen-demo"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Branch "main")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build-halogendemo")
            defaultTagText
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            "main"

halogenechartsdemo :: Prefs -> Op
halogenechartsdemo p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/blog/purescript/halogen-echarts-demo/"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Branch "main")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build-halogenechartsdemo")
            defaultTagText
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            "main"

prometheusmonitor :: Prefs -> Op
prometheusmonitor p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/blog/purescript/prometheus-monitor"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Branch "main")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build-prometheus-monitor")
            defaultTagText
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            "main"

postgrestTable :: Prefs -> Op
postgrestTable p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/postgrest-table"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "postgrest-table"
            (Git.Remote "git@github.com:lucasdicioccio/postgrest-table.git")
            (Git.Branch "master")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build")
            defaultTagText
            "postgrest-table"
            (Git.Remote "git@github.com:lucasdicioccio/postgrest-table.git")
            (Git.Remote "git@github.com:lucasdicioccio/postgrest-table.git")
            "master"

tradeoffs :: Prefs -> Op
tradeoffs p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/blog/purescript/tradeoffs"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Branch "main")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build-tradeoffs")
            defaultTagText
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            "main"

scopeexplorer :: Prefs -> Op
scopeexplorer p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/blog/purescript/scope-explorer"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Branch "main")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build-scope-explorer")
            defaultTagText
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            "main"

babywordsgame :: Prefs -> Op
babywordsgame p =
    build `inject` clone
  where
    build = Spago.build r ignoreTrack spago
    clone = Git.repo reportPrint Debian.git ksrepo

    spago :: Spago.Spago
    spago = Spago.Spago "./git-repos/blog/purescript/babywordgame"

    ksrepo :: Git.Repo
    ksrepo =
        Git.Repo
            "./git-repos/"
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Branch "main")

    r :: Reporter Spago.Report
    r =
        reportSpagoWithTag
            (Git.TagName "salmon-build-babywordsgame")
            defaultTagText
            "blog"
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
            (Git.Remote "git@github.com:lucasdicioccio/blog.git")
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

proto3wire :: Prefs -> Op
proto3wire p =
    CabalBuilding.cabalRepoOnlyBuild
        reportPrint
        "proto3-wire"
        (p.bindir "proto3-wire")
        realNoop
        "proto3-wire"
        (Git.Remote "https://github.com/awakesecurity/proto3-wire.git")
        "master"
        ""
        [Cabal.AllowNewer]

protolens :: Prefs -> Op
protolens p =
    CabalBuilding.cabalRepoOnlyBuild
        reportPrint
        "protolens"
        (p.bindir "protolens")
        realNoop
        "proto-lens"
        (Git.Remote "https://github.com/google/proto-lens.git")
        "master"
        ""
        []

protolens_protoc :: Prefs -> Tracked' FilePath
protolens_protoc p =
    CabalBuilding.cabalRepoBuild
        reportPrint
        "protolens"
        (p.bindir "protolens")
        realNoop
        "proto-lens-protoc"
        "proto-lens-protoc"
        (Git.Remote "https://github.com/google/proto-lens.git")
        "master"
        ""
        []

unix_default :: Prefs -> Tracked' FilePath
unix_default p =
    CabalBuilding.cabalRepoBuild
        reportPrint
        "unix"
        (p.bindir "unix")
        realNoop
        "unix"
        "unix"
        (Git.Remote "https://github.com/haskell/unix.git")
        "master"
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
        reportCabalWithTag
            defaultTag
            defaultTagText
            "prodapi"
            (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
            (Git.Remote "git@github.com:lucasdicioccio/prodapi.git")
            "master"

prodapi_proxy :: Prefs -> Op
prodapi_proxy p =
    CabalBuilding.cabalRepoOnlyBuild
        r
        "prodapi"
        (p.bindir "prodapi")
        realNoop
        "prodapi-proxy"
        (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
        "master"
        ""
        []
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-build-prodapi-proxy")
            defaultTagText
            "prodapi"
            (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
            (Git.Remote "git@github.com:lucasdicioccio/prodapi.git")
            "master"

prodapi_userauth :: Prefs -> Op
prodapi_userauth p =
    CabalBuilding.cabalRepoOnlyBuild
        r
        "prodapi"
        (p.bindir "prodapi")
        realNoop
        "prodapi-userauth"
        (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
        "master"
        ""
        []
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-build-prodapi-userauth")
            defaultTagText
            "prodapi"
            (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
            (Git.Remote "git@github.com:lucasdicioccio/prodapi.git")
            "master"

prodapi_gen :: Prefs -> Op
prodapi_gen p =
    CabalBuilding.cabalRepoOnlyBuild
        r
        "prodapi"
        (p.bindir "prodapi")
        realNoop
        "prodapi-gen"
        (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
        "master"
        ""
        []
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-build-prodapi-gen")
            defaultTagText
            "prodapi"
            (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
            (Git.Remote "git@github.com:lucasdicioccio/prodapi.git")
            "master"

prodapi__publish :: Prefs -> CabalBuilding.VersionString -> Op
prodapi__publish p v =
    CabalBuilding.publishHackage
        r
        "prodapi"
        (p.sdistdir "prodapi")
        "prodapi"
        "prodapi"
        v
        (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
        "master"
        ""
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-published-prodapi")
            defaultTagText
            "prodapi"
            (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
            (Git.Remote "git@github.com:lucasdicioccio/prodapi.git")
            "master"

prodapi_proxy__publish :: Prefs -> CabalBuilding.VersionString -> Op
prodapi_proxy__publish p v =
    CabalBuilding.publishHackage
        r
        "prodapi"
        (p.sdistdir "prodapi")
        "prodapi-proxy"
        "prodapi-proxy"
        v
        (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
        "master"
        ""
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-published-prodapi-proxy")
            defaultTagText
            "prodapi"
            (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
            (Git.Remote "git@github.com:lucasdicioccio/prodapi.git")
            "master"

prodapi_userauth__publish :: Prefs -> CabalBuilding.VersionString -> Op
prodapi_userauth__publish p v =
    CabalBuilding.publishHackage
        r
        "prodapi"
        (p.sdistdir "prodapi")
        "prodapi-userauth"
        "prodapi-userauth"
        v
        (Git.Remote "https://github.com/lucasdicioccio/prodapi.git")
        "master"
        ""
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-published-prodapi-userauth")
            defaultTagText
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
        reportCabalWithTag
            defaultTag
            defaultTagText
            "acme-not-a-joke"
            (Git.Remote "https://github.com/lucasdicioccio/acme-not-a-joke.git")
            (Git.Remote "git@github.com:lucasdicioccio/acme-not-a-joke.git")
            "main"

acmeNotAJoke__publish :: Prefs -> CabalBuilding.VersionString -> Op
acmeNotAJoke__publish p v =
    CabalBuilding.publishHackage
        r
        "acme-not-a-joke"
        (p.sdistdir "acme-not-a-joke")
        "acme-not-a-joke"
        "."
        v
        (Git.Remote "https://github.com/lucasdicioccio/acme-not-a-joke.git")
        "master"
        ""
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-published-acme-not-a-joke")
            defaultTagText
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
        "all"
        (Git.Remote "https://github.com/lucasdicioccio/minizinc-process.git")
        "master"
        ""
        []
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            defaultTag
            defaultTagText
            "minizinc-process"
            (Git.Remote "https://github.com/lucasdicioccio/minizinc-process.git")
            (Git.Remote "git@github.com:lucasdicioccio/minizinc-process.git")
            "master"

grpcNative_warp :: Prefs -> Op
grpcNative_warp p =
    CabalBuilding.cabalRepoOnlyBuild
        reportPrint
        "http2-grpc-haskell"
        (p.bindir "http2-grpc-haskell")
        realNoop
        "all"
        (Git.Remote "https://github.com/haskell-grpc-native/http2-grpc-haskell.git")
        "master"
        "warp-grpc"
        [Cabal.AllowNewer]
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-build-warp")
            defaultTagText
            "http2-grpc-haskell"
            (Git.Remote "https://github.com/haskell-grpc-native/http2-grpc-haskell.git")
            (Git.Remote "git@github.com:haskell-grpc-native/http2-grpc-haskell.git")
            "master"

grpcNative_types :: Prefs -> Op
grpcNative_types p =
    CabalBuilding.cabalRepoOnlyBuild
        reportPrint
        "http2-grpc-haskell"
        (p.bindir "http2-grpc-haskell")
        realNoop
        "http2-client-grpc"
        (Git.Remote "https://github.com/haskell-grpc-native/http2-grpc-haskell.git")
        "master"
        "http2-grpc-types"
        [Cabal.AllowNewer]
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-build-types")
            defaultTagText
            "http2-grpc-haskell"
            (Git.Remote "https://github.com/haskell-grpc-native/http2-grpc-haskell.git")
            (Git.Remote "git@github.com:haskell-grpc-native/http2-grpc-haskell.git")
            "master"

grpcNative_client :: Prefs -> Op
grpcNative_client p =
    CabalBuilding.cabalRepoOnlyBuild
        reportPrint
        "http2-grpc-haskell"
        (p.bindir "http2-grpc-haskell")
        realNoop
        "http2-client-grpc"
        (Git.Remote "https://github.com/haskell-grpc-native/http2-grpc-haskell.git")
        "master"
        "http2-client-grpc"
        [Cabal.AllowNewer]
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            (Git.TagName "salmon-build-client")
            defaultTagText
            "http2-grpc-haskell"
            (Git.Remote "https://github.com/haskell-grpc-native/http2-grpc-haskell.git")
            (Git.Remote "git@github.com:haskell-grpc-native/http2-grpc-haskell.git")
            "master"

http2Client :: Prefs -> Op
http2Client p =
    CabalBuilding.cabalRepoOnlyBuild
        r
        "http2-client"
        (p.bindir "http2-client")
        realNoop
        "all"
        (Git.Remote "https://github.com/haskell-grpc-native/http2-client.git")
        "master"
        ""
        []
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            defaultTag
            defaultTagText
            "http2-client"
            (Git.Remote "https://github.com/haskell-grpc-native/http2-client.git")
            (Git.Remote "git@github.com:haskell-grpc-native/http2-client.git")
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
        reportCabalWithTag
            defaultTag
            defaultTagText
            "sqq"
            (Git.Remote "https://github.com/lucasdicioccio/sqq.git")
            (Git.Remote "git@github.com:lucasdicioccio/sqq.git")
            "main"

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
        []
  where
    r :: Reporter CabalBuilding.Report
    r =
        reportCabalWithTag
            defaultTag
            defaultTagText
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

data PureScriptBuild
    = EchartsSimple
    | HalogenEchartsSimple
    | Humdrum
    | KitchenSinkSearchBox
    | KitchenSinkGraphExplorer
    deriving (Generic)
instance FromJSON PureScriptBuild
instance ToJSON PureScriptBuild

data PersonalBuild
    = HalogenDemo
    | HalogenEchartsDemo
    | BabyWordsGame
    | PrometheusMonitor
    | PostgrestTable
    | ScopeExplorer
    | Tradeoffs
    deriving (Generic)
instance FromJSON PersonalBuild
instance ToJSON PersonalBuild

data HaskellBuild
    = KitchenSink
    | KitchenSinkBridge
    | MicroDNS
    | ProdAPI
    | ProdAPIUserAuth
    | ProdAPIProxy
    | ProdAPIGen
    | Salmon
    | AcmeNotAJoke
    | MinizincProcess
    | Sqq
    | Http2Client
    | GrpcNativeClient
    | GrpcNativeTypes
    | GrpcNativeWarp
    | PostgREST
    | Proto3Wire
    | ProtoLens
    | ProtoLensProtoc
    | UnixDefault
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
    | PureScriptBuild BinDir PureScriptBuild
    | PersonalBuild BinDir PersonalBuild
    | HaskellPublish CabalBuilding.VersionString BinDir HaskellBuild
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
    specOp k (HaskellBuild h KitchenSinkBridge) = [opGraph $ kitchenSink_bridge (homedir h)]
    specOp k (HaskellBuild h ProdAPI) = [prodapi (homedir h)]
    specOp k (HaskellBuild h ProdAPIUserAuth) = [prodapi_userauth (homedir h)]
    specOp k (HaskellBuild h ProdAPIProxy) = [prodapi_proxy (homedir h)]
    specOp k (HaskellBuild h ProdAPIGen) = [prodapi_gen (homedir h)]
    specOp k (HaskellBuild h Salmon) = [opGraph $ salmon (homedir h)]
    specOp k (HaskellBuild h MinizincProcess) = [minizincProcess (homedir h)]
    specOp k (HaskellBuild h Http2Client) = [http2Client (homedir h)]
    specOp k (HaskellBuild h GrpcNativeClient) = [grpcNative_client (homedir h)]
    specOp k (HaskellBuild h GrpcNativeTypes) = [grpcNative_types (homedir h)]
    specOp k (HaskellBuild h GrpcNativeWarp) = [grpcNative_warp (homedir h)]
    specOp k (HaskellBuild h Sqq) = [opGraph $ sqq (homedir h)]
    specOp k (HaskellBuild h AcmeNotAJoke) = [acmeNotAJoke (homedir h)]
    specOp k (HaskellBuild h PostgREST) = [opGraph $ postgrest (homedir h)]
    specOp k (HaskellBuild h Proto3Wire) = [proto3wire (homedir h)]
    specOp k (HaskellBuild h ProtoLens) = [protolens (homedir h)]
    specOp k (HaskellBuild h ProtoLensProtoc) = [opGraph $ protolens_protoc (homedir h)]
    specOp k (HaskellBuild h UnixDefault) = [opGraph $ unix_default (homedir h)]
    specOp k (HaskellBuild h Mustache) = [opGraph $ mustache (homedir h)]
    specOp k (HaskellBuild h SwarmRoot) = [opGraph $ swarmRoot (homedir h)]
    specOp k (HaskellBuild h Fourmolu) = [opGraph $ fourmolu (homedir h)]
    specOp k (HaskellBuild h Duckling) = [opGraph $ duckling (homedir h)]
    --
    specOp k (PureScriptBuild h EchartsSimple) = [echarts_simple (homedir h)]
    specOp k (PureScriptBuild h HalogenEchartsSimple) = [halogen_echarts_simple (homedir h)]
    specOp k (PureScriptBuild h Humdrum) = [humdrum (homedir h)]
    specOp k (PureScriptBuild h KitchenSinkSearchBox) = [kitchenSink_searchbox (homedir h)]
    specOp k (PureScriptBuild h KitchenSinkGraphExplorer) = [kitchenSink_graphexplorer (homedir h)]
    --
    specOp k (PersonalBuild h HalogenDemo) = [halogendemo (homedir h)]
    specOp k (PersonalBuild h HalogenEchartsDemo) = [halogenechartsdemo (homedir h)]
    specOp k (PersonalBuild h BabyWordsGame) = [babywordsgame (homedir h)]
    specOp k (PersonalBuild h PrometheusMonitor) = [prometheusmonitor (homedir h)]
    specOp k (PersonalBuild h PostgrestTable) = [postgrestTable (homedir h)]
    specOp k (PersonalBuild h ScopeExplorer) = [scopeexplorer (homedir h)]
    specOp k (PersonalBuild h Tradeoffs) = [tradeoffs (homedir h)]
    --
    specOp k (HaskellPublish v h ProdAPI) = [prodapi__publish (homedir h) v]
    specOp k (HaskellPublish v h ProdAPIProxy) = [prodapi_proxy__publish (homedir h) v]
    specOp k (HaskellPublish v h ProdAPIUserAuth) = [prodapi_userauth__publish (homedir h) v]
    specOp k (HaskellPublish v h MicroDNS) = [microdns__publish (homedir h) v]
    specOp k (HaskellPublish v h AcmeNotAJoke) = [acmeNotAJoke__publish (homedir h) v]

    optimizedDeps :: Op -> Op
    optimizedDeps base =
        let pkgs = Debian.installAllDebsAtOnce base
         in Debian.removeSinglePackages base `inject` pkgs

-------------------------------------------------------------------------------
data Seed
    = BuildSeed {binroot :: BinDir, buildName :: Text}
    | ReleaseSeed {sdistroot :: BinDir, buildName :: Text, versionString :: Text}

instance ParseRecord Seed where
    parseRecord =
        combo <**> helper
      where
        combo =
            subparser $
                mconcat
                    [ command "build" (info build (progDesc "makes a build"))
                    , command "release" (info release (progDesc "makes a release"))
                    ]
        build =
            BuildSeed
                <$> strArgument (Options.Applicative.help "bin-root")
                <*> strArgument (Options.Applicative.help "build-name")
        release =
            ReleaseSeed
                <$> strArgument (Options.Applicative.help "sdist-root")
                <*> strArgument (Options.Applicative.help "build-name")
                <*> strArgument (Options.Applicative.help "version")

configure :: Configure IO Seed Spec
configure = Configure go
  where
    go :: Seed -> IO Spec
    go (BuildSeed h "kitchensink:hs") = pure $ HaskellBuild h KitchenSink
    go (BuildSeed h "kitchensink:bridge:hs") = pure $ HaskellBuild h KitchenSinkBridge
    go (BuildSeed h "kitchensink:searchbox:purs") = pure $ PureScriptBuild h KitchenSinkSearchBox
    go (BuildSeed h "kitchensink:graphexplorer:purs") = pure $ PureScriptBuild h KitchenSinkGraphExplorer
    go (BuildSeed h "microdns:hs") = pure $ HaskellBuild h MicroDNS
    go (BuildSeed h "prodapi:hs") = pure $ HaskellBuild h ProdAPI
    go (BuildSeed h "prodapi:proxy:hs") = pure $ HaskellBuild h ProdAPIProxy
    go (BuildSeed h "prodapi:userauth:hs") = pure $ HaskellBuild h ProdAPIUserAuth
    go (BuildSeed h "prodapi:gen:hs") = pure $ HaskellBuild h ProdAPIGen
    go (BuildSeed h "salmon:hs") = pure $ HaskellBuild h Salmon
    go (BuildSeed h "minizinc-process:hs") = pure $ HaskellBuild h MinizincProcess
    go (BuildSeed h "sqq:hs") = pure $ HaskellBuild h Sqq
    go (BuildSeed h "http2-client:hs") = pure $ HaskellBuild h Http2Client
    go (BuildSeed h "grpc-native-types:hs") = pure $ HaskellBuild h GrpcNativeTypes
    go (BuildSeed h "grpc-native-client:hs") = pure $ HaskellBuild h GrpcNativeClient
    go (BuildSeed h "grpc-native-warp:hs") = pure $ HaskellBuild h GrpcNativeWarp
    go (BuildSeed h "acme-not-a-joke:hs") = pure $ HaskellBuild h AcmeNotAJoke
    go (BuildSeed h "echarts-simple:purs") = pure $ PureScriptBuild h EchartsSimple
    go (BuildSeed h "halogen-echarts-simple:purs") = pure $ PureScriptBuild h HalogenEchartsSimple
    go (BuildSeed h "humdrum:purs") = pure $ PureScriptBuild h Humdrum
    go (BuildSeed h "postgrest:hs") = pure $ HaskellBuild h PostgREST
    go (BuildSeed h "proto3-wire:hs") = pure $ HaskellBuild h Proto3Wire
    go (BuildSeed h "protolens:hs") = pure $ HaskellBuild h ProtoLens
    go (BuildSeed h "protolens-protoc:hs") = pure $ HaskellBuild h ProtoLensProtoc
    go (BuildSeed h "unix-default:hs") = pure $ HaskellBuild h UnixDefault
    go (BuildSeed h "mustache:hs") = pure $ HaskellBuild h Mustache
    go (BuildSeed h "swarmroot:hs") = pure $ HaskellBuild h SwarmRoot
    go (BuildSeed h "fourmolu:hs") = pure $ HaskellBuild h Fourmolu
    go (BuildSeed h "duckling:hs") = pure $ HaskellBuild h Duckling
    go (BuildSeed h "halogen-demo:purs") = pure $ PersonalBuild h HalogenDemo
    go (BuildSeed h "halogenecharts-demo:purs") = pure $ PersonalBuild h HalogenEchartsDemo
    go (BuildSeed h "babywordsgame:purs") = pure $ PersonalBuild h BabyWordsGame
    go (BuildSeed h "prometheus-monitor:purs") = pure $ PersonalBuild h PrometheusMonitor
    go (BuildSeed h "postgrest-table:purs") = pure $ PersonalBuild h PostgrestTable
    go (BuildSeed h "scope-explorer:purs") = pure $ PersonalBuild h ScopeExplorer
    go (BuildSeed h "tradeoffs:purs") = pure $ PersonalBuild h Tradeoffs
    go (BuildSeed h ":grpc") =
        pure $
            Batch
                [ HaskellBuild h Http2Client
                , HaskellBuild h GrpcNativeTypes
                , HaskellBuild h GrpcNativeClient
                , HaskellBuild h GrpcNativeWarp
                , HaskellBuild h ProtoLens
                , HaskellBuild h Proto3Wire
                , HaskellBuild h ProtoLensProtoc
                ]
    go (BuildSeed h ":prodapi") =
        pure $
            Batch
                [ HaskellBuild h ProdAPI
                , HaskellBuild h ProdAPIUserAuth
                , HaskellBuild h ProdAPIProxy
                , HaskellBuild h ProdAPIGen
                ]
    go (BuildSeed h ":mine") =
        pure $
            Batch
                [ HaskellBuild h ProdAPI
                , HaskellBuild h ProdAPIUserAuth
                , HaskellBuild h ProdAPIProxy
                , HaskellBuild h ProdAPIGen
                , HaskellBuild h AcmeNotAJoke
                , HaskellBuild h MicroDNS
                , HaskellBuild h KitchenSink
                , HaskellBuild h KitchenSinkBridge
                , PureScriptBuild h KitchenSinkSearchBox
                , PureScriptBuild h KitchenSinkGraphExplorer
                , HaskellBuild h Salmon
                , HaskellBuild h MinizincProcess
                , HaskellBuild h Sqq
                , HaskellBuild h Http2Client
                , PureScriptBuild h EchartsSimple
                , PureScriptBuild h HalogenEchartsSimple
                , PureScriptBuild h Humdrum
                , PersonalBuild h HalogenDemo
                , PersonalBuild h HalogenEchartsDemo
                , PersonalBuild h BabyWordsGame
                , PersonalBuild h ScopeExplorer
                , PersonalBuild h PrometheusMonitor
                , PersonalBuild h PostgrestTable
                , PersonalBuild h Tradeoffs
                ]
    go (BuildSeed h ":important") =
        pure $
            Batch
                [ HaskellBuild h ProdAPI
                , HaskellBuild h ProdAPIUserAuth
                , HaskellBuild h ProdAPIProxy
                , HaskellBuild h ProdAPIGen
                , HaskellBuild h AcmeNotAJoke
                , HaskellBuild h MicroDNS
                , HaskellBuild h KitchenSink
                , HaskellBuild h KitchenSinkBridge
                , PureScriptBuild h KitchenSinkSearchBox
                , PureScriptBuild h KitchenSinkGraphExplorer
                , HaskellBuild h Salmon
                , HaskellBuild h MinizincProcess
                , HaskellBuild h Mustache
                , HaskellBuild h PostgREST
                , HaskellBuild h Proto3Wire
                , HaskellBuild h ProtoLens
                , HaskellBuild h ProtoLensProtoc
                , HaskellBuild h UnixDefault
                , HaskellBuild h Fourmolu
                , HaskellBuild h Http2Client
                , HaskellBuild h GrpcNativeTypes
                , HaskellBuild h GrpcNativeClient
                , HaskellBuild h GrpcNativeWarp
                , PureScriptBuild h EchartsSimple
                , PureScriptBuild h Humdrum
                , PureScriptBuild h HalogenEchartsSimple
                , PersonalBuild h PrometheusMonitor
                , PersonalBuild h PostgrestTable
                ]
    go (BuildSeed h ":fun") =
        pure $
            Batch
                [ HaskellBuild h SwarmRoot
                , HaskellBuild h Duckling
                ]
    go (ReleaseSeed h "prodapi:publish" v) =
        pure $ HaskellPublish v h ProdAPI
    go (ReleaseSeed h "prodapi-userauth:publish" v) =
        pure $ HaskellPublish v h ProdAPIUserAuth
    go (ReleaseSeed h "prodapi-proxy:publish" v) =
        pure $ HaskellPublish v h ProdAPIProxy
    go (ReleaseSeed h "microdns:publish" v) =
        pure $ HaskellPublish v h MicroDNS
    go (ReleaseSeed h "acme-not-a-joke:publish" v) =
        pure $ HaskellPublish v h AcmeNotAJoke

-------------------------------------------------------------------------------
main :: IO ()
main = do
    let desc = fullDesc <> progDesc "Some builds." <> header "for dicioccio.fr"
    let opts = info parseRecord desc
    cmd <- execParser opts
    CLI.execCommandOrSeed reportPrint configure program cmd
