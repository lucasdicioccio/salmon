{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Core (
    -- * GCP identity
    Project (..),
    Zone (..),
    Region (..),

    -- * gcloud binary
    gcloud,

    -- * The account gcloud acts as
    Account (..),
    declaredAccount,
    interpretAccount,
    activeAccountArgs,
    readActiveAccount,
    withAccount,

    -- * Application Default Credentials
    applicationDefaultCredentials,
    interpretAdc,
    printAccessToken,
    Report (..),
    GcloudCommand (..),
    gcloudCommand,

    -- * teardown
    downIfPresent,

    -- * eventual consistency
    retryingIO,
    afterEnableRetries,
    afterEnableDelay,

    -- * CLI helpers
    gcloudProc,
    quietly,
    disablePromptsLine,
    withProject,
    withZone,
    withRegion,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import GHC.IO.Exception (ExitCode (..))
import System.IO.Error (userError)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | A GCP project identifier.
newtype Project = Project {projectId :: Text}
    deriving (Eq, Ord, Show)

-- | A GCP zone.
newtype Zone = Zone {zoneName :: Text}
    deriving (Eq, Ord, Show)

-- | A GCP region.
newtype Region = Region {regionName :: Text}
    deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------

data Report
    = RunGcloud !GcloudCommand !Binary.Report
    | RunAdc !Binary.Report
    deriving (Show)

-- | The various gcloud invocations that 'Core' knows how to run.
data GcloudCommand
    = AdcPrintAccessToken
    | ConfigGetAccount
    deriving (Show)

-- | Builds a 'CreateProcess' for a gcloud invocation.
gcloudCommand :: Command "gcloud" GcloudCommand
gcloudCommand = Command $ \cmd -> case cmd of
    AdcPrintAccessToken ->
        gcloudProc ["auth", "application-default", "print-access-token"]
    ConfigGetAccount ->
        gcloudProc activeAccountArgs

-- | A provider for the @gcloud@ binary. For Phase 1 we assume @gcloud@ is on
-- @PATH@; callers can override with a real installer if they prefer.
gcloud :: Track' (Binary "gcloud")
gcloud = Track $ \_ ->
    op "gcloud" nodeps $ \actions ->
        actions
            { help = "gcloud CLI on PATH"
            , ref = mkRef "gcloud" ("gcloud" :: Text)
            }

{- | Validates Application Default Credentials.

Mind what this does /not/ say: every @gcloud@ invocation in this tree, and
'printAccessToken', acts as gcloud's /active account/, which is a separate
credential from the application-default one and may belong to somebody else.
This node passing says a client library could authenticate; it says nothing
about who the other GCP nodes will act as. 'declaredAccount' is the node
that does.
-}
applicationDefaultCredentials :: Reporter Report -> Track' (Binary "gcloud") -> Op
applicationDefaultCredentials r gcloudTrack =
    withBinary gcloudTrack gcloudCommand AdcPrintAccessToken $ \up ->
        op "gcp-adc" nodeps $ \actions ->
            actions
                { help = "validates GCP Application Default Credentials"
                , ref = mkRef "gcp-adc" ("application-default-credentials" :: Text)
                , up = up r'
                , check = checkAdc
                }
  where
    r' = contramap RunAdc r

    checkAdc :: IO CheckResult
    checkAdc = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode
                (gcloudProc ["auth", "application-default", "print-access-token"])
                ""
        pure $ interpretAdc code

-- | The verdict drawn from @gcloud auth application-default
-- print-access-token@'s exit code, split out for testability.
interpretAdc :: ExitCode -> CheckResult
interpretAdc ExitSuccess = Success
interpretAdc (ExitFailure n) = Failure ("gcloud ADC not available (exit " <> Text.pack (show n) <> ")")

-------------------------------------------------------------------------------
-- The active account

-- | The account (a user's or a service account's email) gcloud should act as.
newtype Account = Account {accountEmail :: Text}
    deriving (Eq, Ord, Show)

{- | The arguments reading the account gcloud will act as. It is the
@core/account@ property, so it honours @CLOUDSDK_CORE_ACCOUNT@ and the active
configuration alike, which is exactly what every other invocation does.
-}
activeAccountArgs :: [String]
activeAccountArgs = ["config", "get-value", "account"]

-- | Append @--account@ to a gcloud argument list.
withAccount :: Account -> [String] -> [String]
withAccount a args = args <> ["--account", Text.unpack a.accountEmail]

{- | Asserts that gcloud's active account is the declared one, and refuses
otherwise.

Every GCP node shells out to @gcloud@, which acts as whichever account is
active on the machine; with several Google accounts logged in, a graph would
otherwise silently run as the wrong one. A recipe that knows who it means to
be puts this node under its other GCP nodes (where 'applicationDefaultCredentials'
used to stand alone), so a mismatch fails this node and blocks the rest
before anything is created.

It never /changes/ the active account: that is the operator's machine
configuration, shared with everything else on it. Its @up@ re-reads and
throws on anything but a match, saying what to run. Nothing is done on
@down@.
-}
declaredAccount :: Reporter Report -> Track' (Binary "gcloud") -> Account -> Op
declaredAccount _r gcloudTrack acct =
    withBinary gcloudTrack gcloudCommand ConfigGetAccount $ \_run ->
        op "gcp-account" nodeps $ \actions ->
            actions
                { help = "asserts the account gcloud acts as"
                , notes = ["declared account: " <> acct.accountEmail]
                , ref = mkRef "gcp-account" acct.accountEmail
                , up = refuseUnlessDeclared
                , check = checkAccount
                }
  where
    checkAccount :: IO CheckResult
    checkAccount = uncurry (interpretAccount acct) <$> readActiveAccount

    refuseUnlessDeclared :: IO ()
    refuseUnlessDeclared = do
        result <- checkAccount
        case result of
            Success -> pure ()
            Failure why ->
                throwIO . userError . Text.unpack $
                    why
                        <> "; refusing to act as another identity (gcloud config set account "
                        <> acct.accountEmail
                        <> ", or export CLOUDSDK_CORE_ACCOUNT)"
            other -> throwIO (userError ("could not establish gcloud's active account: " <> show other))

-- | Runs @gcloud config get-value account@: its exit code and stdout.
readActiveAccount :: IO (ExitCode, ByteString)
readActiveAccount = do
    (code, out, _err) <- readCreateProcessWithExitCode (gcloudProc activeAccountArgs) ""
    pure (code, out)

{- | The verdict drawn from @gcloud config get-value account@.

With no account set, gcloud exits 0 and prints @(unset)@ on stderr, leaving
stdout empty (older releases printed it on stdout), so both spellings count
as none. Addresses are compared without regard to case, as Google does.
-}
interpretAccount :: Account -> ExitCode -> ByteString -> CheckResult
interpretAccount _ (ExitFailure n) _ =
    Failure ("could not read gcloud's active account (exit " <> Text.pack (show n) <> ")")
interpretAccount acct ExitSuccess out
    | Text.null active || active == "(unset)" =
        Failure ("gcloud has no active account, declared " <> acct.accountEmail)
    | Text.toCaseFold active == Text.toCaseFold (Text.strip acct.accountEmail) = Success
    | otherwise =
        Failure ("gcloud's active account is " <> active <> ", declared " <> acct.accountEmail)
  where
    active = Text.strip (Text.decodeUtf8With TextError.lenientDecode out)

{- | Fetches a fresh OAuth2 access token for gcloud's /active account/ (not
the application-default credentials; see 'declaredAccount' for pinning who
that is).

This is the credential a container registry expects for username
@oauth2accesstoken@ -- see "SreBox.Gcp.CloudRunDeploy", which feeds this
straight into "Salmon.Builtin.Nodes.Podman".@login@'s @IO Text@ password
argument so a fresh token is fetched right when @up@ runs rather than
baked into the graph when it was built (tokens like this are short-lived,
typically ~1h).
-}
printAccessToken :: IO Text
printAccessToken = do
    (code, out, err) <-
        readCreateProcessWithExitCode
            (gcloudProc ["auth", "print-access-token"])
            ""
    case code of
        ExitSuccess -> pure (Text.strip (Text.decodeUtf8 out))
        ExitFailure n ->
            throwIO (userError ("gcloud auth print-access-token failed (exit " <> show n <> "): " <> Text.unpack (Text.decodeUtf8With TextError.lenientDecode err)))

-------------------------------------------------------------------------------
-- Teardown

{- | Runs a teardown action only if the node's own check says the effect is
actually there.

@gcloud@ treats "delete something absent" as an error (@404@, @Service ...
could not be found@, or even @API has not been used in project ...@ when the
service was never enabled), and "Salmon.Actions.UpDown" contains a failing
@down@ by leaving that node standing and marking every /predecessor/
'Blocked'. A node that was never created therefore blocks the teardown of
everything it was declared on top of -- including, for a recipe that owns its
project, the project delete that would have swept it all. That is how four
half-built sandbox projects survived their own @run down@.

A node's check is deliberately never consulted /for/ teardown (it answers
"does my effect need creating", not "is it still there"), so this is the node
author's own business rather than something the driver can do. Erring toward
not-running is the safe direction here: 'Failure' means the effect is gone or
unreachable, and re-running @down@ costs nothing.
-}
downIfPresent :: IO CheckResult -> IO () -> IO ()
downIfPresent runCheck act = do
    result <- runCheck
    case result of
        Success -> act
        _ -> pure ()

-------------------------------------------------------------------------------
-- Eventual consistency

{- | Runs an action, retrying on failure with a fixed delay, rethrowing the
last failure.

GCP grants access asynchronously, past the point where the thing granting it
reports success. Two cases hit this tree, both found by
@salmon-apps@'s @salmon-gcp-toy@ against a real project:

* a __freshly enabled API__ answers @PERMISSION_DENIED ... (or it may not
  exist)@ to the very next create, for up to about a minute, even for a
  project owner;
* a __freshly created service account__ is not yet resolvable by the service
  owning the resource a binding names it on.

So a node whose @up@ can run moments after such a grant retries rather than
failing the whole traversal, since a one-shot driver has no other way to
wait. A genuine permission error still fails the node, just later.
-}
retryingIO :: Int -> Int -> IO () -> IO ()
retryingIO attempts delay act = do
    result <- try act
    case result of
        Right () -> pure ()
        Left (e :: SomeException)
            | attempts <= 1 -> throwIO e
            | otherwise -> threadDelay delay >> retryingIO (attempts - 1) delay act

-- | Attempts for a create that may follow an API enablement: 6 over ~50s.
afterEnableRetries :: Int
afterEnableRetries = 6

-- | Delay between those attempts.
afterEnableDelay :: Int
afterEnableDelay = 10000000

-------------------------------------------------------------------------------
-- CLI helpers

{- | A @gcloud@ process with the given sub-command arguments, and @--quiet@.

Every invocation is non-interactive on purpose. No process a node starts
reads the pass's standard input (see "Salmon.Builtin.Nodes.Binary".'Binary.detachedStdin'),
so a prompt could never be answered anyway: without @--quiet@ gcloud prints
it, reads end-of-file, and the node fails with the prompt's text (\"Would you
like to enable and retry?\") for an error. With it, gcloud takes the default
or says plainly that input was required.
-}
gcloudProc :: [String] -> CreateProcess
gcloudProc args = proc "gcloud" (quietly args)

{- | Append @--quiet@ to a gcloud argument list unless it is already there.
It goes among gcloud's own flags: before a @--@, after which the words are
somebody else's (the remote command of a @gcloud compute ssh@).
-}
quietly :: [String] -> [String]
quietly args
    | "--quiet" `elem` flags = args
    | otherwise = flags <> ["--quiet"] <> rest
  where
    (flags, rest) = break (== "--") args

{- | The same for a script of @gcloud@ calls run by a shell, which
'gcloudProc' never sees: the line to put first.
-}
disablePromptsLine :: Text
disablePromptsLine = "export CLOUDSDK_CORE_DISABLE_PROMPTS=1"

-- | Append @--project@ to a gcloud argument list.
withProject :: Project -> [String] -> [String]
withProject p args = args <> ["--project", Text.unpack p.projectId]

-- | Append @--zone@ to a gcloud argument list.
withZone :: Zone -> [String] -> [String]
withZone z args = args <> ["--zone", Text.unpack z.zoneName]

-- | Append @--region@ to a gcloud argument list.
withRegion :: Region -> [String] -> [String]
withRegion rgn args = args <> ["--region", Text.unpack rgn.regionName]
