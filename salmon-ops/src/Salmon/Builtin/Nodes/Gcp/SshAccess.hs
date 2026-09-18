{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.SshAccess (
    SshEndpoint (..),
    ProbePolicy (..),
    defaultProbePolicy,
    sshAvailable,
    sshAvailableWithin,
    SshUnreachable (..),
    MetadataSshCa (..),
    installMetadataCaKey,
    Report (..),
    SshAccessCommand (..),
    sshAccessCommand,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
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
    { sshUser :: Maybe Text
    -- ^ 'Nothing' connects as whatever @ssh@ defaults to (the local user),
    -- which is rarely the principal a signed certificate was issued for.
    , sshHost :: Text
    , sshPort :: Int
    , sshIdentity :: FilePath
    }
    deriving (Eq, Show)

-- | How long 'sshAvailable'\'s 'up' keeps probing before giving up.
data ProbePolicy = ProbePolicy
    { probeAttempts :: Int
    , probeDelayMicros :: Int
    }
    deriving (Eq, Show)

-- | 30 attempts, 10s apart: a fresh VM's sshd is normally up well inside that.
defaultProbePolicy :: ProbePolicy
defaultProbePolicy = ProbePolicy 30 10000000

data SshUnreachable = SshUnreachable
    { unreachableHost :: Text
    , unreachableAttempts :: Int
    , unreachableLastError :: Text
    }
    deriving (Show)

instance Exception SshUnreachable

-- | 'sshAvailableWithin' 'defaultProbePolicy'.
sshAvailable :: Reporter Report -> SshEndpoint -> Op
sshAvailable = sshAvailableWithin defaultProbePolicy

{- | Verifies that an SSH endpoint is reachable, and /waits/ for it to be.

The 'up' is what makes this node a gate rather than an observation. With a
no-op 'up', a failing 'check' is simply followed by that no-op succeeding,
and everything depending on this node proceeds against an unreachable host
-- the opposite of what the node exists for. So 'up' re-probes on
'ProbePolicy' and throws 'SshUnreachable' if the host never answers, which
is what makes the one-shot drivers report dependants 'Blocked'.

Host keys are accepted on first sight (@StrictHostKeyChecking=accept-new@):
the host being probed is typically a VM created moments earlier, whose key
nothing could have known in advance, and under @BatchMode@ the default
policy would fail the probe forever. A /changed/ key is still refused.
-}
sshAvailableWithin :: ProbePolicy -> Reporter Report -> SshEndpoint -> Op
sshAvailableWithin policy _r endpoint =
    op "gcp-ssh-available" nodeps $ \actions ->
        actions
            { help = Text.unwords ["checks SSH is reachable on", endpoint.sshHost]
            , ref = mkRef "gcp-ssh-available" (endpoint.sshUser, endpoint.sshHost, endpoint.sshPort)
            , check = either (Failure . ("SSH not reachable: " <>)) (const Success) <$> probe
            , up = waitReachable policy.probeAttempts ""
            }
  where
    waitReachable :: Int -> Text -> IO ()
    waitReachable remaining lastErr
        | remaining <= 0 =
            throwIO (SshUnreachable endpoint.sshHost policy.probeAttempts lastErr)
        | otherwise = do
            result <- probe
            case result of
                Right () -> pure ()
                Left err -> do
                    threadDelay policy.probeDelayMicros
                    waitReachable (remaining - 1) err

    probe :: IO (Either Text ())
    probe = do
        (code, _out, err) <-
            readCreateProcessWithExitCode
                ( proc
                    "ssh"
                    [ "-o"
                    , "ConnectTimeout=5"
                    , "-o"
                    , "BatchMode=yes"
                    , "-o"
                    , "StrictHostKeyChecking=accept-new"
                    , "-i"
                    , endpoint.sshIdentity
                    , "-p"
                    , show endpoint.sshPort
                    , Text.unpack (maybe endpoint.sshHost (\u -> u <> "@" <> endpoint.sshHost) endpoint.sshUser)
                    , "true"
                    ]
                )
                ""
        pure $ case code of
            ExitSuccess -> Right ()
            ExitFailure _ -> Left (endpoint.sshHost <> ": " <> Text.strip (Text.decodeUtf8With TextError.lenientDecode err))

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
                , ref = mkRef "gcp-metadata-ssh-ca" (cfg.sshCaProject.projectId, cfg.sshCaPublicKey)
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
