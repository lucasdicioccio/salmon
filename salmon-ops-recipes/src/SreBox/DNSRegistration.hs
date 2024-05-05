{-# LANGUAGE DeriveGeneric #-}

module SreBox.DNSRegistration where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.CronTask as CronTask
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Rsync as Rsync
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track
import Salmon.Reporter
import SreBox.MicroDNS (DNSName, MicroDNSConfig (..), selfSignedCert, sharedToken)
import qualified SreBox.MicroDNS as MicroDNS

-------------------------------------------------------------------------------
data Report
    = UploadToken !Rsync.Report
    | UploadPEM !Rsync.Report
    | SelfSign !MicroDNS.Report
    | MakeToken !MicroDNS.Report
    | UploadSelf !Self.Report
    | CallSelf !Self.Report
    deriving (Show)

-------------------------------------------------------------------------------

data RegisteredMachineConfig
    = RegisteredMachineConfig
    { registration_machineName :: DNSName
    , registration_cfg_local_token_path :: FilePath
    , --
      registration_cfg_task_script :: FilePath
    , registration_cfg_task_data_path_token :: FilePath
    , registration_cfg_task_data_path_pem :: FilePath
    }

setupRegistration ::
    forall directive.
    (FromJSON directive, ToJSON directive) =>
    Reporter Report ->
    Track' Ssh.Remote ->
    Track' directive ->
    Self.SelfPath ->
    Self.Remote ->
    MicroDNSConfig ->
    RegisteredMachineConfig ->
    (RegisteredMachineSetup -> directive) ->
    Op
setupRegistration r mkRemote simulate selfpath selfRemote dns cfg toSpec =
    op "registration" (deps [trackedGraph remoteRegistration, uploadPem, uploadToken]) id
  where
    rsyncRemote :: Rsync.Remote
    rsyncRemote = (\(Self.Remote a b) -> Rsync.Remote a b) selfRemote

    tmpTokenPath :: FilePath
    tmpTokenPath = "tmp/" <> Text.unpack cfg.registration_machineName <> "-registration.token"

    uploadToken =
        Rsync.sendFile (contramap UploadToken r) Debian.rsync (FS.Generated mkToken cfg.registration_cfg_local_token_path) rsyncRemote tmpTokenPath

    tmpPemPath :: FilePath
    tmpPemPath = "tmp/registration.pem"

    uploadPem =
        Rsync.sendFile (contramap UploadPEM r) Debian.rsync (FS.Generated (selfSignedCert (contramap SelfSign r) dns) dns.microdns_cfg_pemPath) rsyncRemote tmpPemPath

    mkToken :: Track' FilePath
    mkToken = Track $ \tokenPath ->
        sharedToken (contramap MakeToken r) dns.microdns_cfg_secretPath cfg.registration_machineName tokenPath

    self = Self.uploadSelf (contramap UploadSelf r) "tmp" selfRemote selfpath
    remoteRegistration = self `bindTracked` \ref -> Self.callSelfAsSudo (contramap CallSelf r) mkRemote ref simulate CLI.Up (toSpec regSetup)

    regSetup =
        RegisteredMachineSetup
            cfg.registration_machineName
            tmpTokenPath
            tmpPemPath
            (cfg.registration_cfg_task_script)
            (cfg.registration_cfg_task_data_path_token)
            (cfg.registration_cfg_task_data_path_pem)

data RegisteredMachineSetup
    = RegisteredMachineSetup
    { registration_name :: DNSName
    , registration_token_tmppath :: FilePath
    , registration_pem_tmppath :: FilePath
    , -- where to copy to
      registration_script_path :: FilePath
    , registration_token_path :: FilePath
    , registration_pem_path :: FilePath
    }
    deriving (Generic)

instance FromJSON RegisteredMachineSetup
instance ToJSON RegisteredMachineSetup

registerMachine :: RegisteredMachineSetup -> Op
registerMachine setup =
    op "enrolled-machine" (deps [autoregister, movetoken, movepem, Binary.justInstall Debian.curl]) id
  where
    tokenPath = setup.registration_token_path
    pemPath = setup.registration_pem_path
    taskScript = setup.registration_script_path

    movetoken :: Op
    movetoken =
        FS.fileCopy setup.registration_token_tmppath tokenPath

    movepem :: Op
    movepem =
        FS.fileCopy setup.registration_pem_tmppath pemPath

    scriptcontents :: Text
    scriptcontents = autoregisterscript setup

    autoregister :: Op
    autoregister =
        let
            task =
                CronTask.CronTask
                    ("register-dns_" <> setup.registration_name)
                    "root"
                    CronTask.everyMinute
                    "bash"
                    [ Text.pack taskScript
                    , Text.pack tokenPath
                    , setup.registration_name
                    , Text.pack pemPath
                    ]
         in
            CronTask.crontask ignoreTrack task `inject` FS.filecontents (FS.FileContents taskScript scriptcontents)

autoregisterscript :: RegisteredMachineSetup -> Text
autoregisterscript setup =
    Text.unlines
        [ "#!/bin/bash"
        , ""
        , "hmacpath=$1"
        , "regname=$2"
        , "pempath=$3"
        , ""
        , "hmac=`cat ${hmacpath}`"
        , "curl -XPOST \\"
        , "  --cacert \"${pempath}\"\\"
        , "  -H \"x-microdns-hmac: ${hmac}\" \\"
        , "  \"https://box.dicioccio.fr:65432/register/auto/${regname}\""
        ]
