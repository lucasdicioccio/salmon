{-# LANGUAGE DeriveGeneric #-}

module SreBox.KitchenSinkBlog where

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.FilePath ((</>))

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Certificates as Certs
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Git as Git
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track

import SreBox.CabalBuilding

data KitchenSinkBlogConfig
    = KitchenSinkBlogConfig
    { ks_cfg_repo :: Git.Repo
    , ks_cfg_sourceSubdir :: FilePath
    , ks_cfg_certSpec :: (Certs.Domain, Text)
    , ks_cfg_pemPath :: FilePath
    , ks_cfg_certPath :: FilePath
    }

data KitchenSinkBlogSetup
    = KitchenSinkBlogSetup
    { ks_setup_localBinPath :: FilePath
    , ks_setup_localPemPath :: FilePath
    , ks_setup_localKeyPath :: FilePath
    , ks_setup_localSrcDir :: FilePath
    , ks_setup_subdir :: FilePath
    }
    deriving (Generic)
instance FromJSON KitchenSinkBlogSetup
instance ToJSON KitchenSinkBlogSetup

setupKS ::
    (FromJSON directive, ToJSON directive) =>
    Track' Ssh.Remote ->
    Track' (Certs.Domain, Text) ->
    Track' directive ->
    Self.Remote ->
    Self.SelfPath ->
    KitchenSinkBlogConfig ->
    (KitchenSinkBlogSetup -> directive) ->
    Op
setupKS mkRemote mkCert simulate selfRemote selfpath cfg toSpec =
    using (Git.repodir cloneSite cfg.ks_cfg_repo "") $ \blogSrcDir ->
        using (cabalBinUpload (kitchenSink optBuildsBindir) rsyncRemote) $ \remotepath ->
            let
                setup = KitchenSinkBlogSetup remotepath remotePem remoteKey (remoteBlogDir </> Text.unpack cfg.ks_cfg_repo.repoLocalName) cfg.ks_cfg_sourceSubdir
             in
                op "remote-ks-blog-setup" (depSequence blogSrcDir setup) id
  where
    rsyncRemote = (\(Self.Remote a b) -> Rsync.Remote a b) selfRemote
    cloneSite = Track $ Git.repo Debian.git
    depSequence blogSrcDir setup = deps [opGraph (continueRemotely setup) `inject` uploads blogSrcDir]
    uploads blogSrcDir = op "uploads-ks-blog" (deps [uploadCert, uploadKey, uploadSources blogSrcDir]) id

    -- recursive call
    continueRemotely setup = self `bindTracked` recurse setup

    recurse setup selfref =
        Self.callSelfAsSudo mkRemote selfref simulate CLI.Up (toSpec setup)

    -- upload self
    self = Self.uploadSelf "tmp" selfRemote selfpath

    -- upload sources
    uploadSources blogSrcDir =
        Rsync.sendDir Debian.rsync ignoreTrack blogSrcDir rsyncRemote remoteBlogDir

    remoteBlogDir = "tmp/ks-blog-src"

    -- upload certificate and key
    upload gen localpath distpath =
        Rsync.sendFile Debian.rsync (FS.Generated gen localpath) rsyncRemote distpath

    uploadCert = upload siteCert cfg.ks_cfg_pemPath remotePem
    uploadKey = upload siteCert cfg.ks_cfg_certPath remoteKey

    siteCert = Track $ const $ run mkCert cfg.ks_cfg_certSpec

    remotePem = "tmp/ks.pem"
    remoteKey = "tmp/ks.key"

systemdKitchenSinkBlog :: KitchenSinkBlogSetup -> Op
systemdKitchenSinkBlog arg =
    Systemd.systemdService Debian.systemctl trackConfig config
  where
    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \cfg ->
        let
            execPath = Systemd.start_path $ Systemd.service_execStart $ Systemd.config_service $ cfg
            copybin = FS.fileCopy arg.ks_setup_localBinPath execPath
            copypem = FS.fileCopy arg.ks_setup_localPemPath pemPath
            copykey = FS.fileCopy arg.ks_setup_localKeyPath keyPath
            movesrc = FS.moveDirectory arg.ks_setup_localSrcDir blogSrcDir
         in
            op "setup-systemd-for-ks" (deps [movesrc, copybin, copypem, copykey, localSetup]) id

    localSetup :: Op
    localSetup =
        op "ks-setup" (deps [sysDeps]) id

    sysDeps :: Op
    sysDeps =
        op "system" (deps [Debian.deb (Debian.Package "graphviz")]) id

    config :: Systemd.Config
    config = Systemd.Config tgt unit service install

    tgt :: Systemd.UnitTarget
    tgt = "salmon-ks.service"

    blogSrcDir, keyPath, pemPath :: FilePath
    pemPath = "/opt/rundir/ks/cert.pem"
    keyPath = "/opt/rundir/ks/cert.key"
    blogSrcDir = "/opt/rundir/ks/site"
    blogSrcPath = blogSrcDir </> arg.ks_setup_subdir

    unit :: Systemd.Unit
    unit = Systemd.Unit "Kitchen-Sink from Salmon" "network-online.target"

    service :: Systemd.Service
    service = Systemd.Service Systemd.Simple "root" "root" "007" start Systemd.OnFailure Systemd.Process blogSrcDir

    start :: Systemd.Start
    start =
        Systemd.Start
            "/opt/rundir/ks/bin/kitchen-sink"
            [ "serve"
            , "--servMode"
            , "SERVE"
            , "--httpPort"
            , "80"
            , "--httpsPort"
            , "443"
            , "--tlsCertFile"
            , Text.pack pemPath
            , "--tlsKeyFile"
            , Text.pack keyPath
            , "--outDir"
            , "./out"
            , "--srcDir"
            , Text.pack blogSrcPath
            ]

    install :: Systemd.Install
    install = Systemd.Install "multi-user.target"
