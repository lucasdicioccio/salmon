{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.SshAccess (
    SshEndpoint (..),
    sshAvailable,
    MetadataSshCa (..),
    installMetadataCaKey,
    Report (..),
    SshAccessCommand (..),
    sshAccessCommand,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), gcloudProc, withProject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunSshAccessCommand !SshAccessCommand !Binary.Report
    | RunSshProbe !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | An SSH endpoint to probe.
data SshEndpoint = SshEndpoint
    { sshHost :: Text
    , sshPort :: Int
    , sshIdentity :: FilePath
    }
    deriving (Eq, Show)

-- | Verifies that an SSH endpoint is reachable.
sshAvailable :: Reporter Report -> SshEndpoint -> Op
sshAvailable r endpoint =
    op "gcp-ssh-available" nodeps $ \actions ->
        actions
            { help = Text.unwords ["checks SSH is reachable on", endpoint.sshHost]
            , ref = mkRef "gcp-ssh-available" (endpoint.sshHost, endpoint.sshPort)
            , check = checkSsh endpoint
            }
  where
    checkSsh :: SshEndpoint -> IO CheckResult
    checkSsh ep = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                ( proc
                    "ssh"
                    [ "-o"
                    , "ConnectTimeout=5"
                    , "-o"
                    , "BatchMode=yes"
                    , "-i"
                    , ep.sshIdentity
                    , "-p"
                    , show ep.sshPort
                    , Text.unpack ep.sshHost
                    , "true"
                    ]
                )
                ""
        pure $ case code of
            ExitSuccess -> Success
            ExitFailure _ -> Failure ("SSH not reachable: " <> ep.sshHost)

-------------------------------------------------------------------------------

-- | Configuration for injecting an SSH CA public key via project metadata.
data MetadataSshCa = MetadataSshCa
    { sshCaProject :: Project
    , sshCaPublicKey :: FilePath
    }
    deriving (Eq, Show)

-- | Installs an SSH CA public key into project metadata so that GCE instances
-- trust it.
installMetadataCaKey :: Reporter Report -> Track' (Binary "gcloud") -> MetadataSshCa -> Op
installMetadataCaKey r gcloudTrack cfg =
    withBinary gcloudTrack sshAccessCommand (MetadataAddSshCa cfg) $ \up ->
        op "gcp-metadata-ssh-ca" nodeps $ \actions ->
            actions
                { help = Text.unwords ["installs SSH CA public key into project metadata"]
                , ref = mkRef "gcp-metadata-ssh-ca" cfg.sshCaPublicKey
                , up = up r'
                }
  where
    r' = contramap (RunSshAccessCommand (MetadataAddSshCa cfg)) r

-------------------------------------------------------------------------------

data SshAccessCommand
    = MetadataAddSshCa MetadataSshCa
    deriving (Show)

sshAccessCommand :: Command "gcloud" SshAccessCommand
sshAccessCommand = Command $ \cmd -> case cmd of
    MetadataAddSshCa cfg ->
        gcloudProc $
            withProject cfg.sshCaProject
                [ "compute"
                , "project-info"
                , "add-metadata"
                , "--metadata-from-file"
                , "ssh-ca=" <> cfg.sshCaPublicKey
                ]
