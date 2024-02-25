{-# LANGUAGE DeriveGeneric #-}

module SreBox.DNSRegistration where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as Text


import Salmon.Op.OpGraph (inject)
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.CronTask as CronTask
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import SreBox.MicroDNS (DNSName)


data RegisteredMachineSetup
  = RegisteredMachineSetup
  { registration_name :: DNSName
  , registration_token_tmppath :: FilePath
  , registration_pem_tmppath :: FilePath
  -- where to copy to
  , registration_script_path :: FilePath
  , registration_token_path :: FilePath
  , registration_pem_path :: FilePath
  } deriving (Generic)

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
            "register-dns"
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
