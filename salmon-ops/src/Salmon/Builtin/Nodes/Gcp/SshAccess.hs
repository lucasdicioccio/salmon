{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.SshAccess (
    SshEndpoint (..),
    ProbePolicy (..),
    defaultProbePolicy,
    sshAvailable,
    sshAvailableWithin,
    probeSsh,
    sshProbeArgs,
    waitSshReachable,
    waitReachableWith,
    SshUnreachable (..),
    MetadataSshCa (..),
    installMetadataCaKey,
    Report (..),
    SshAccessCommand (..),
    sshAccessCommand,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import Control.Monad (void, when)
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
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
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
    , sshClientOpts :: Ssh.ClientOpts
    -- ^ the key to authenticate with, and where to keep host keys -- the
    -- same options the upload and the remote call will use, or the probe is
    -- not answering the question they are about to ask.
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
            , check = either (Failure . ("SSH not reachable: " <>)) (const Success) <$> probeSsh endpoint
            , up = waitSshReachable policy endpoint >>= either throwIO pure
            }

-------------------------------------------------------------------------------
-- The same probe without a node.

{- | One probe: @ssh -o ConnectTimeout=5 -o BatchMode=yes ... true@ against
the endpoint. 'Right' when it ran @true@ remotely, else 'Left' with the host
and ssh's own stderr (stripped), which is the "why not".

This is exactly what 'sshAvailableWithin'\'s 'check' runs, for a caller that
wants the answer without building an 'Op' (and without a driver to run it).
It waits no longer than ssh's own connect timeout, plus however long an
accepted connection takes to run @true@.
-}
probeSsh :: SshEndpoint -> IO (Either Text ())
probeSsh endpoint = do
    (code, _out, err) <- readCreateProcessWithExitCode (proc "ssh" (sshProbeArgs endpoint)) ""
    pure $ case code of
        ExitSuccess -> Right ()
        ExitFailure _ -> Left (endpoint.sshHost <> ": " <> Text.strip (Text.decodeUtf8With TextError.lenientDecode err))

-- | The argv 'probeSsh' passes to @ssh@ (pure, so it can be pinned).
sshProbeArgs :: SshEndpoint -> [String]
sshProbeArgs endpoint =
    [ "-o"
    , "ConnectTimeout=5"
    , "-o"
    , "BatchMode=yes"
    ]
        <> Ssh.clientArgs endpoint.sshClientOpts
        <> [ "-p"
           , show endpoint.sshPort
           , Text.unpack (maybe endpoint.sshHost (\u -> u <> "@" <> endpoint.sshHost) endpoint.sshUser)
           , "true"
           ]

{- | Probes until the endpoint answers or the 'ProbePolicy' runs out: what
'sshAvailableWithin'\'s 'up' does, returning the failure instead of
throwing it. 'Left' carries the host, the attempts spent and the last ssh
error. The wait is bounded by roughly
@probeAttempts * (5s connect timeout + probeDelayMicros)@.

A host key that no longer matches is forgotten from the endpoint's
known-hosts file (when it names one) and retried at once, as in the node.
A policy of zero (or fewer) attempts probes nothing and is 'Left'.
-}
waitSshReachable :: ProbePolicy -> SshEndpoint -> IO (Either SshUnreachable ())
waitSshReachable policy endpoint =
    waitReachableWith policy endpoint.sshHost onMismatch (probeSsh endpoint)
  where
    onMismatch = case endpoint.sshClientOpts.optKnownHosts of
        Just hosts -> Just (forgetHostKey hosts endpoint.sshHost)
        Nothing -> Nothing

{- | The retry loop behind 'waitSshReachable', over any single-attempt
probe; exported so the loop can be tested without ssh. The second argument
is the host named in 'SshUnreachable'; the third is what to run on a host
key mismatch before retrying at once ('Nothing' treats a mismatch like any
other failure). No delay follows the last attempt.
-}
waitReachableWith :: ProbePolicy -> Text -> Maybe (IO ()) -> IO (Either Text ()) -> IO (Either SshUnreachable ())
waitReachableWith policy host onMismatch probe = go policy.probeAttempts ""
  where
    go :: Int -> Text -> IO (Either SshUnreachable ())
    go remaining lastErr
        | remaining <= 0 =
            pure (Left (SshUnreachable host policy.probeAttempts lastErr))
        | otherwise = do
            result <- probe
            case result of
                Right () -> pure (Right ())
                Left err
                    -- A host key that no longer matches never resolves by
                    -- waiting: this is a machine rebuilt at an address salmon
                    -- reserved, so "same address, new host" is the expected
                    -- case rather than an attack. Forget the recorded key and
                    -- retry at once; if the mismatch somehow persists, the
                    -- ordinary budget still runs out and still fails.
                    | Ssh.isHostKeyMismatch err
                    , Just forget <- onMismatch -> do
                        forget
                        go (remaining - 1) err
                    | otherwise -> do
                        when (remaining > 1) $ threadDelay policy.probeDelayMicros
                        go (remaining - 1) err

-- ssh-keygen -R rewrites the file in place, and succeeds when there was
-- nothing to remove.
forgetHostKey :: FilePath -> Text -> IO ()
forgetHostKey hosts host =
    void $ readCreateProcessWithExitCode (proc "ssh-keygen" ["-R", Text.unpack host, "-f", hosts]) ""

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
