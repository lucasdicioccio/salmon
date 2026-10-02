{-# LANGUAGE OverloadedStrings #-}

{- | Google Secret Manager: a named secret, and the versions holding its
bytes.

Two nodes rather than one, because they are two effects with different
lifetimes. A secret is a long-lived container with an IAM policy on it (see
"Salmon.Builtin.Nodes.Gcp.Iam", whose @iamResourceArgs@ already understands
@projects\/P\/secrets\/S@); a version is immutable content, and adding one
never replaces what is there.

That second property is the whole design problem here. @gcloud secrets
versions add@ is not idempotent in any useful sense -- it always creates a
new version -- so a node that simply ran it on every @up@ would leave a
project accumulating a version per pass, each one billed, with the older
ones still readable. 'secretVersion' therefore has a real @check@: it reads
the current @latest@ back and compares it with the bytes it would upload. No
change, no version.
-}
module Salmon.Builtin.Nodes.Gcp.SecretManager (
    Secret (..),
    secret,
    interpretSecretDescribe,
    SecretVersion (..),
    secretVersion,
    interpretSecretContents,
    SecretFile (..),
    secretFile,
    SecretFetchFailed (..),
    Report (..),
    SecretManagerCommand (..),
    secretManagerCommand,
) where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import GHC.IO.Exception (ExitCode (..))
import System.Directory (doesFileExist)
import System.Process.ByteString (readCreateProcessWithExitCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), gcloudProc, withProject)
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import Salmon.Builtin.Nodes.SecretDelivery (Placement (..), checkInstalledSecret, installSecretBytes, removeInstalledSecret)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunSecretManagerCommand !SecretManagerCommand !Binary.Report
    | -- | a secret was read for a file: which one, and how @gcloud@ exited.
      -- Never the 'Binary.Report' of that read, whose stdout is the secret.
      FetchedSecretFile !SecretFile !ExitCode
    deriving (Show)

-------------------------------------------------------------------------------

-- | A secret: a name, an IAM policy, and a stack of versions.
data Secret = Secret
    { secretName :: Text
    , secretProject :: Project
    , secretReplication :: Text
    -- ^ @automatic@, or @user-managed@ with locations set out of band.
    }
    deriving (Eq, Show)

-- | Idempotently creates a secret. Deleting one destroys every version in it.
secret :: Reporter Report -> Track' (Binary "gcloud") -> Secret -> Op
secret r gcloudTrack sec =
    withBinary gcloudTrack secretManagerCommand (SecretsCreate sec) $ \create ->
        withBinary gcloudTrack secretManagerCommand (SecretsDelete sec) $ \delete ->
            op "gcp-secret" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates secret", sec.secretName]
                    , notes = ["down destroys every version of the secret"]
                    , ref = mkRef "gcp-secret" (sec.secretProject.projectId, sec.secretName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create (rFor (SecretsCreate sec)))
                    , down = Core.downIfPresent checkSecret (delete (rFor (SecretsDelete sec)))
                    , check = checkSecret
                    }
  where
    rFor cmd = contramap (RunSecretManagerCommand cmd) r

    checkSecret :: IO CheckResult
    checkSecret = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode (prepare secretManagerCommand (SecretsDescribe sec)) ""
        pure $ interpretSecretDescribe sec.secretName code

-- | The verdict drawn from @gcloud secrets describe@.
interpretSecretDescribe :: Text -> ExitCode -> CheckResult
interpretSecretDescribe _name ExitSuccess = Success
interpretSecretDescribe name (ExitFailure _) = Failure ("secret not found: " <> name)

-------------------------------------------------------------------------------

{- | The contents of a secret's latest version, taken from a local file.

The file is read at @up@ time rather than being baked into the graph, so a
certificate re-issued by an earlier node in the same pass is the one that
gets uploaded.
-}
data SecretVersion = SecretVersion
    { versionSecret :: Secret
    , versionSourceFile :: FilePath
    }
    deriving (Eq, Show)

{- | Ensures the secret's latest version holds this file's bytes.

The @check@ reads the secret back and compares, which is what keeps a
re-converged graph from stacking up a new version per pass. Two consequences
worth knowing:

* the comparison happens __in this process__ and the value is never
  reported, logged or passed to a shell. It is still a read of the secret,
  so whoever runs salmon needs @secretmanager.versions.access@ and will
  appear in the audit log doing so on every pass.
* a secret whose latest version was @disabled@ or @destroyed@ reads as a
  failure, and this node then adds a fresh version -- which is the right
  answer, but it does mean disabling a version does not hold if salmon
  converges afterwards.

There is no @down@: versions are immutable, and destroying them is what
deleting the enclosing 'secret' does.
-}
secretVersion :: Reporter Report -> Track' (Binary "gcloud") -> SecretVersion -> Op
secretVersion r gcloudTrack version =
    withBinary gcloudTrack secretManagerCommand (VersionsAdd version) $ \add ->
        op "gcp-secret-version" (deps [secret r gcloudTrack version.versionSecret]) $ \actions ->
            actions
                { help = Text.unwords ["uploads", Text.pack version.versionSourceFile, "to secret", sec.secretName]
                , notes = ["adds a version only when the contents differ from the latest one"]
                , ref = mkRef "gcp-secret-version" (sec.secretProject.projectId, sec.secretName, version.versionSourceFile)
                , up = add (contramap (RunSecretManagerCommand (VersionsAdd version)) r)
                , check = checkContents
                }
  where
    sec = version.versionSecret

    checkContents :: IO CheckResult
    checkContents = do
        present <- doesFileExist version.versionSourceFile
        if not present
            then pure (Failure ("nothing to upload: " <> Text.pack version.versionSourceFile))
            else do
                wanted <- ByteString.readFile version.versionSourceFile
                (code, out, _err) <-
                    readCreateProcessWithExitCode (prepare secretManagerCommand (VersionsAccessLatest sec)) ""
                pure $ interpretSecretContents sec.secretName wanted code out

{- | Whether the bytes read back are the bytes we would upload.

Compared exactly, with no trimming: a secret is whatever bytes were put in
it, and a PEM file's trailing newline is part of the file. Trimming here
would make a node that had just uploaded its own file report a difference
forever after.
-}
interpretSecretContents :: Text -> ByteString.ByteString -> ExitCode -> ByteString.ByteString -> CheckResult
interpretSecretContents name _wanted (ExitFailure _) _ =
    Failure ("no readable version of secret: " <> name)
interpretSecretContents name wanted ExitSuccess got
    | wanted == got = Success
    | otherwise = Failure ("secret " <> name <> " holds different contents")

-------------------------------------------------------------------------------

{- | A Secret Manager secret, as a file on the machine this graph runs on.

The other direction from 'secretVersion': that one runs on the controlling
machine and puts bytes /into/ Secret Manager, this one runs on an instance
and takes them out, authenticating as whatever @gcloud@ finds there -- on a
GCE instance, its service account, through the metadata server. Nothing is
shipped to the machine but the secret's name, so the directive that declares
this node can be printed, piped and reported like any other.
-}
data SecretFile = SecretFile
    { fileProject :: Project
    , fileSecretName :: Text
    , fileVersion :: Text
    -- ^ @latest@, or a version number. With @latest@ a rotation is picked up
    -- by the next pass (or by the tending loop under @run serve@).
    , filePlacement :: Placement
    }
    deriving (Eq, Show)

-- | Thrown when the secret cannot be read. Names the secret; @gcloud@'s
-- stderr on a failed read is a diagnosis, not a payload.
data SecretFetchFailed = SecretFetchFailed Text Int Text

instance Show SecretFetchFailed where
    show (SecretFetchFailed name n err) =
        "could not read secret " <> Text.unpack name <> " (gcloud exit " <> show n <> "): " <> Text.unpack err

instance Exception SecretFetchFailed

{- | Ensures the file holds the secret version's bytes, with the declared
owner and mode.

What it takes for this to work is not declared here and cannot be, since it
is the /other/ side's business: the instance needs a service account with
@roles\/secretmanager.secretAccessor@ on the secret (see
"Salmon.Builtin.Nodes.Gcp.Iam"), the @cloud-platform@ access scope, and a
@gcloud@ on its @PATH@.

The read happens in this process: the bytes are @gcloud@'s standard output,
captured, and written with
'Salmon.Builtin.Nodes.SecretDelivery.installSecretBytes'. They are in no
argv, no report and no failure text. Like 'secretVersion', the @check@ is
itself a read of the secret, so the instance's identity shows in the audit
log once per pass.

The enclosing directory must already exist; @down@ removes the file.
-}
secretFile :: Reporter Report -> Track' (Binary "gcloud") -> SecretFile -> Op
secretFile r gcloudTrack file =
    -- the tracked runner is dropped on purpose: it reports the command's
    -- stdout, which here is the secret.
    withBinary gcloudTrack secretManagerCommand access $ \_reportingRun ->
        op "gcp-secret-file" nodeps $ \actions ->
            actions
                { help = Text.unwords ["writes secret", file.fileSecretName, "to", Text.pack place.placePath]
                , notes =
                    [ "version " <> file.fileVersion <> ", owner " <> place.placeOwner <> ":" <> place.placeGroup <> ", mode " <> place.placeMode
                    , "read as this machine's own identity; the contents are never reported"
                    ]
                , ref = mkRef "secret-file" place.placePath
                , check = fetch >>= either (\(n, _) -> pure (Failure ("no readable version of secret: " <> file.fileSecretName <> " (gcloud exit " <> Text.pack (show n) <> ")"))) (checkInstalledSecret place)
                , up = fetch >>= either (\(n, err) -> throwIO (SecretFetchFailed file.fileSecretName n err)) (installSecretBytes place)
                , down = removeInstalledSecret place
                }
  where
    place = file.filePlacement
    access = VersionsAccess file.fileProject file.fileSecretName file.fileVersion

    fetch :: IO (Either (Int, Text) ByteString.ByteString)
    fetch = do
        (code, out, err) <- readCreateProcessWithExitCode (prepare secretManagerCommand access) ""
        runReporter r (FetchedSecretFile file code)
        pure $ case code of
            ExitSuccess -> Right out
            ExitFailure n -> Left (n, Text.take 400 (Text.strip (Text.decodeUtf8With TextError.lenientDecode err)))

-------------------------------------------------------------------------------

data SecretManagerCommand
    = SecretsCreate Secret
    | SecretsDescribe Secret
    | SecretsDelete Secret
    | VersionsAdd SecretVersion
    | VersionsAccessLatest Secret
    | -- | project, secret name, version: a read that needs no 'Secret' value
      -- (and so no replication policy, which a reader has no opinion on).
      VersionsAccess Project Text Text
    deriving (Show)

secretManagerCommand :: Command "gcloud" SecretManagerCommand
secretManagerCommand = Command $ \cmd -> case cmd of
    SecretsCreate sec ->
        gcloudProc $
            withProject sec.secretProject
                [ "secrets"
                , "create"
                , Text.unpack sec.secretName
                , "--replication-policy"
                , Text.unpack sec.secretReplication
                ]
    SecretsDescribe sec ->
        gcloudProc $
            withProject sec.secretProject ["secrets", "describe", Text.unpack sec.secretName]
    SecretsDelete sec ->
        gcloudProc $
            withProject sec.secretProject ["secrets", "delete", Text.unpack sec.secretName, "--quiet"]
    VersionsAdd version ->
        gcloudProc $
            withProject version.versionSecret.secretProject
                [ "secrets"
                , "versions"
                , "add"
                , Text.unpack version.versionSecret.secretName
                , -- via a file rather than --data-file=- on stdin: the bytes
                  -- never become an argv entry, and argv is world-readable
                  -- through /proc for as long as the process lives.
                  "--data-file"
                , version.versionSourceFile
                ]
    VersionsAccessLatest sec ->
        gcloudProc $
            withProject sec.secretProject
                [ "secrets"
                , "versions"
                , "access"
                , "latest"
                , "--secret"
                , Text.unpack sec.secretName
                ]
    VersionsAccess prj name version ->
        gcloudProc $
            withProject prj
                [ "secrets"
                , "versions"
                , "access"
                , Text.unpack version
                , "--secret"
                , Text.unpack name
                ]
