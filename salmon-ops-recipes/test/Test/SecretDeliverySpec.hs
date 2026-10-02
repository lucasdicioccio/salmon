{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.SecretDelivery" and the
Secret Manager file node built on it.

The upload's remote half is three shell scripts, and what a test can do
without a remote is run those very scripts under a local @sh@ -- no ssh, no
sudo, the placement owned by whoever runs the suite. That covers what the
scripts do and what their exit codes mean; the ssh leg itself is only
asserted on as a rendered command line.

Throughout, a sentinel stands in for the secret, and the assertions that
matter most are the ones saying where it does /not/ show up.
-}
module Test.SecretDeliverySpec (tests) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as C8
import Data.Either (isLeft)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO (stdin)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import System.Process (readProcess)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CmdSpec (..), cmdspec, proc)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Data.Bits ((.&.))
import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (prepare, prepareIO)
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.SecretManager as SecretManager
import Salmon.Builtin.Nodes.SecretDelivery
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter (silent)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.SecretDelivery"
        [ testGroup "validatePlacement" placementTests
        , testGroup "remote scripts, run locally" scriptTests
        , testGroup "interpretProbe" probeTests
        , testGroup "ssh command" commandTests
        , testGroup "node" nodeTests
        , testGroup "on-machine placement" installTests
        , testGroup "SecretManager.secretFile" secretFileTests
        ]

sentinel :: C8.ByteString
sentinel = "s3ntinel-not-a-real-secret"

sentinelText :: Text
sentinelText = "s3ntinel-not-a-real-secret"

-- | The user and group running the suite: the only owner a test can chown to.
whoami :: IO (Text, Text)
whoami = do
    u <- readProcess "id" ["-un"] ""
    g <- readProcess "id" ["-gn"] ""
    pure (Text.strip (Text.pack u), Text.strip (Text.pack g))

placementAt :: FilePath -> Text -> IO Placement
placementAt path mode = do
    (u, g) <- whoami
    either (\why -> assertFailure (Text.unpack why)) pure (validatePlacement (Placement path u g mode))

runScript :: Text -> C8.ByteString -> IO ExitCode
runScript script input = do
    (code, _out, _err) <- readCreateProcessWithExitCode (proc "sh" ["-c", Text.unpack script]) input
    pure code

modeOf :: FilePath -> IO Int
modeOf path = fromIntegral . (.&. 0o7777) . fileMode <$> getFileStatus path

placementTests :: [TestTree]
placementTests =
    [ testCase "normalizes the mode to what stat prints" $
        assertEqual "" (Right "600") (placeMode <$> validatePlacement (Placement "/etc/x/secret" "root" "root" "0600"))
    , testCase "keeps a group-readable mode" $
        assertEqual "" (Right "640") (placeMode <$> validatePlacement (Placement "/etc/x/secret" "app" "app" "640"))
    , testCase "refuses a mode granting anything to others" $
        assertBool "" (isLeft (validatePlacement (Placement "/etc/x/secret" "root" "root" "0644")))
    , testCase "refuses a mode that is not octal" $
        assertBool "" (isLeft (validatePlacement (Placement "/etc/x/secret" "root" "root" "u+r")))
    , testCase "refuses a relative path" $
        assertBool "" (isLeft (validatePlacement (Placement "secret" "root" "root" "0600")))
    , testCase "refuses a path with a newline" $
        assertBool "" (isLeft (validatePlacement (Placement "/etc/x\n/secret" "root" "root" "0600")))
    , testCase "refuses an owner that is shell rather than a name" $
        assertBool "" (isLeft (validatePlacement (Placement "/etc/x/secret" "root; rm -rf /" "root" "0600")))
    , testCase "refuses an owner that reads as an option" $
        assertBool "" (isLeft (validatePlacement (Placement "/etc/x/secret" "-R" "root" "0600")))
    , testCase "refuses an empty group" $
        assertBool "" (isLeft (validatePlacement (Placement "/etc/x/secret" "root" "" "0600")))
    ]

scriptTests :: [TestTree]
scriptTests =
    [ testCase "upload writes the bytes with the declared mode, creating the directory" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let path = dir </> "sub" </> "secret"
            place <- placementAt path "0640"
            code <- runScript (uploadScript place) sentinel
            assertEqual "exit" ExitSuccess code
            got <- C8.readFile path
            assertEqual "contents" sentinel got
            mode <- modeOf path
            assertEqual "mode" 0o640 mode
            dirMode <- modeOf (dir </> "sub")
            assertEqual "directory mode" 0o750 dirMode
            left <- listDirectory (dir </> "sub")
            assertEqual "no temporary file left behind" ["secret"] left
    , testCase "upload survives a path with a quote and a space in it" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let path = dir </> "it's here" </> "the secret"
            place <- placementAt path "0600"
            code <- runScript (uploadScript place) sentinel
            assertEqual "exit" ExitSuccess code
            got <- C8.readFile path
            assertEqual "contents" sentinel got
    , testCase "upload replaces what was there and leaves an existing directory alone" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let path = dir </> "secret"
            before <- modeOf dir
            C8.writeFile path "old"
            place <- placementAt path "0600"
            code <- runScript (uploadScript place) sentinel
            assertEqual "exit" ExitSuccess code
            got <- C8.readFile path
            assertEqual "contents" sentinel got
            after <- modeOf dir
            assertEqual "directory mode untouched" before after
    , testCase "upload refuses an empty secret and keeps the old one" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let path = dir </> "secret"
            C8.writeFile path "old"
            place <- placementAt path "0600"
            code <- runScript (uploadScript place) ""
            assertBool "non-zero exit" (code /= ExitSuccess)
            got <- C8.readFile path
            assertEqual "contents" "old" got
            left <- listDirectory dir
            assertEqual "no temporary file left behind" ["secret"] left
    , testCase "probe: absent, matching, differing, wrong mode" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let path = dir </> "secret"
            place <- placementAt path "0600"
            absent <- runScript (probeScript place) sentinel
            assertEqual "absent" (ExitFailure 3) absent
            _ <- runScript (uploadScript place) sentinel
            same <- runScript (probeScript place) sentinel
            assertEqual "matching" ExitSuccess same
            differing <- runScript (probeScript place) "something else"
            assertEqual "differing" (ExitFailure 4) differing
            setFileMode path 0o640
            moded <- runScript (probeScript place) sentinel
            assertEqual "wrong mode" (ExitFailure 5) moded
    , testCase "probe prints nothing, whatever it finds" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let path = dir </> "secret"
            place <- placementAt path "0600"
            _ <- runScript (uploadScript place) "another value"
            (_code, out, err) <- readCreateProcessWithExitCode (proc "sh" ["-c", Text.unpack (probeScript place)]) sentinel
            assertEqual "stdout" "" out
            assertEqual "stderr" "" err
    , testCase "the command handed to ssh survives the remote shell's own parsing" $
        -- ssh gives its argument to the remote user's shell, which is what
        -- `sh -c` does with it here: two levels of quoting, as on the wire.
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let path = dir </> "it's here" </> "the secret"
            place <- placementAt path "0600"
            code <- runScript (Text.pack (remoteCommand AsLoginUser (uploadScript place))) sentinel
            assertEqual "exit" ExitSuccess code
            got <- C8.readFile path
            assertEqual "contents" sentinel got
            probed <- runScript (Text.pack (remoteCommand AsLoginUser (probeScript place))) sentinel
            assertEqual "probe" ExitSuccess probed
    , testCase "remove deletes the file, and succeeds when there is none" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let path = dir </> "secret"
            place <- placementAt path "0600"
            _ <- runScript (uploadScript place) sentinel
            first <- runScript (removeScript place) ""
            assertEqual "exit" ExitSuccess first
            present <- doesFileExist path
            assertBool "gone" (not present)
            again <- runScript (removeScript place) ""
            assertEqual "exit again" ExitSuccess again
    ]

probeTests :: [TestTree]
probeTests =
    [ testCase "0 is Success" $ assertEqual "" Success (interpretProbe place ExitSuccess)
    , testCase "3 is a missing file" $ assertEqual "" (Failure "missing: /etc/x/secret") (interpretProbe place (ExitFailure 3))
    , testCase "4 is different contents" $ assertEqual "" (Failure "/etc/x/secret holds different contents") (interpretProbe place (ExitFailure 4))
    , testCase "5 is owner or mode" $ assertEqual "" (Failure "/etc/x/secret has the wrong owner or mode") (interpretProbe place (ExitFailure 5))
    , testCase "255, ssh not connecting, is Unknown and not a Failure" $ assertEqual "" Unknown (interpretProbe place (ExitFailure 255))
    , testCase "anything else is a Failure naming the exit code" $
        assertEqual "" (Failure "could not inspect /etc/x/secret (exit 1)") (interpretProbe place (ExitFailure 1))
    ]
  where
    place = Placement "/etc/x/secret" "root" "root" "600"

commandTests :: [TestTree]
commandTests =
    [ testCase "the script is one quoted word after sudo -n sh -c" $ do
        args <- argsOf WithSudo
        assertEqual "argv length" 8 (length args)
        assertEqual "login" "salmon@203.0.113.7" (args !! 6)
        assertBool "sudo -n sh -c '...'" ("sudo -n sh -c 'set -eu\n" `isPrefixOf` last args)
    , testCase "no sudo when writing as the login user" $ do
        args <- argsOf AsLoginUser
        assertBool "sh -c '...'" ("sh -c 'set -eu\n" `isPrefixOf` last args)
    , testCase "BatchMode, and the identity and known-hosts file of the caller" $ do
        args <- argsOf WithSudo
        assertEqual "" ["-o", "BatchMode=yes", "-i", "/keys/client", "-o", "IdentitiesOnly=yes"] (take 6 args)
    , testCase "shellQuote closes and reopens around a quote" $
        assertEqual "" "'it'\\''s'" (shellQuote "it's")
    ]
  where
    place = Placement "/etc/x/secret" "root" "root" "600"
    opts = Ssh.ClientOpts (Just "/keys/client") Nothing
    argsOf elevation = do
        p <- prepareIO deliveryCommand (DeliveryCommand opts (Ssh.Remote "salmon" "203.0.113.7") elevation (uploadScript place)) stdin
        pure $ case cmdspec p of
            RawCommand _ args -> args
            ShellCommand s -> [s]

nodeTests :: [TestTree]
nodeTests =
    [ testCase "nothing about the node quotes the file it uploads" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let src = dir </> "source"
            C8.writeFile src sentinel
            case opAct (node src) of
                Nothing -> assertFailure "the upload is not a node"
                Just act -> do
                    let said = Text.unwords (act.extension.help : act.extension.notes)
                    assertBool "help names the source path" (Text.pack src `Text.isInfixOf` said)
                    assertBool "help and notes do not hold the contents" (not (sentinelText `Text.isInfixOf` said))
    , testCase "the ref is the remote host and path, not the source or the login" $
        case (opAct (node "/a/source"), opAct (node "/b/other")) of
            (Just a, Just b) -> do
                assertEqual "same site" a.extension.ref b.extension.ref
                assertEqual "" (mkRef "secret-upload" ("203.0.113.7" :: Text, "/etc/x/secret" :: FilePath)) a.extension.ref
            _ -> assertFailure "the upload is not a node"
    , testCase "a missing source is a Failure naming it, before any connection" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let src = dir </> "absent"
            case opAct (node src) of
                Nothing -> assertFailure "the upload is not a node"
                Just act -> do
                    verdict <- act.extension.check
                    assertEqual "" (Failure ("nothing to upload: " <> Text.pack src)) verdict
                    result <- try act.extension.up
                    case result of
                        Left (e :: SomeException) -> assertBool "names the source" (src `isInfixOf` show e)
                        Right () -> assertFailure "up should have thrown"
    , testCase "an empty source is refused by up" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let src = dir </> "empty"
            C8.writeFile src ""
            case opAct (node src) of
                Nothing -> assertFailure "the upload is not a node"
                Just act -> do
                    result <- try act.extension.up
                    case result of
                        Left (e :: SomeException) -> assertBool "says why" ("empty secret" `isInfixOf` show e)
                        Right () -> assertFailure "up should have thrown"
    , testCase "an invalid placement is refused by check and up alike" $
        case opAct (uploadSecretFile Ssh.noClientOpts silent ignoreTrack (SecretUpload "/a/source" remote (Placement "/etc/x/secret" "root" "root" "0644") WithSudo)) of
            Nothing -> assertFailure "the upload is not a node"
            Just act -> do
                verdict <- act.extension.check
                assertEqual "" (Failure "/etc/x/secret: the mode grants access to others") verdict
                result <- try act.extension.up
                assertBool "up throws" (either (\(_ :: SomeException) -> True) (const False) result)
    , testCase "a report shows paths and an exit code" $ do
        let shown = show (UploadDone (upload "/a/source") (ExitFailure 1))
        assertBool "" ("/a/source" `isInfixOf` shown && "/etc/x/secret" `isInfixOf` shown)
    ]
  where
    remote = Ssh.Remote "salmon" "203.0.113.7"
    upload src = SecretUpload src remote (Placement "/etc/x/secret" "root" "root" "0600") WithSudo
    node src = uploadSecretFile Ssh.noClientOpts silent ignoreTrack (upload src)

installTests :: [TestTree]
installTests =
    [ testCase "install writes the bytes atomically with the declared mode" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let path = dir </> "secret"
            place <- placementAt path "0640"
            installSecretBytes place sentinel
            got <- C8.readFile path
            assertEqual "contents" sentinel got
            mode <- modeOf path
            assertEqual "mode" 0o640 mode
            left <- listDirectory dir
            assertEqual "no temporary file left behind" ["secret"] left
            verdict <- checkInstalledSecret place sentinel
            assertEqual "check" Success verdict
    , testCase "install does not create the directory" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            place <- placementAt (dir </> "missing" </> "secret") "0600"
            result <- try (installSecretBytes place sentinel)
            assertBool "throws" (either (\(_ :: SomeException) -> True) (const False) result)
            made <- doesDirectoryExist (dir </> "missing")
            assertBool "no directory" (not made)
    , testCase "install refuses an empty secret" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            place <- placementAt (dir </> "secret") "0600"
            result <- try (installSecretBytes place "")
            assertBool "throws" (either (\(_ :: SomeException) -> True) (const False) result)
            present <- doesFileExist (dir </> "secret")
            assertBool "nothing written" (not present)
    , testCase "check: missing, differing, wrong mode -- and never quoting either side" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let path = dir </> "secret"
            place <- placementAt path "0600"
            missing <- checkInstalledSecret place sentinel
            assertEqual "missing" (Failure ("missing: " <> Text.pack path)) missing
            C8.writeFile path "what-is-on-disk-instead"
            setFileMode path 0o600
            differing <- checkInstalledSecret place sentinel
            assertEqual "differing" (Failure (Text.pack path <> " holds different contents")) differing
            installSecretBytes place sentinel
            setFileMode path 0o640
            moded <- checkInstalledSecret place sentinel
            assertEqual "wrong mode" (Failure (Text.pack path <> " has the wrong owner or mode")) moded
    , testCase "a leftover temporary file from a killed pass is not reused with its old mode" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            let path = dir </> "secret"
            C8.writeFile (path <> ".salmon-tmp") "stale"
            setFileMode (path <> ".salmon-tmp") 0o644
            place <- placementAt path "0600"
            installSecretBytes place sentinel
            mode <- modeOf path
            assertEqual "mode" 0o600 mode
    , testCase "remove is quiet about a file that is not there" $
        withSystemTempDirectory "salmon-secret" $ \dir -> do
            createDirectory (dir </> "d")
            place <- placementAt (dir </> "d" </> "secret") "0600"
            removeInstalledSecret place
            installSecretBytes place sentinel
            removeInstalledSecret place
            present <- doesFileExist (dir </> "d" </> "secret")
            assertBool "gone" (not present)
    ]

secretFileTests :: [TestTree]
secretFileTests =
    [ testCase "reads the named version of the named secret" $
        assertEqual
            ""
            ["secrets", "versions", "access", "7", "--secret", "db-password", "--project", "acme-prod"]
            (argsOf (SecretManager.VersionsAccess (Core.Project "acme-prod") "db-password" "7"))
    , testCase "the node names the secret and the path, and its ref is the path" $
        case opAct (SecretManager.secretFile silent ignoreTrack file) of
            Nothing -> assertFailure "secretFile is not a node"
            Just act -> do
                assertEqual "" "writes secret db-password to /etc/app/db-password" act.extension.help
                assertEqual "" (mkRef "secret-file" ("/etc/app/db-password" :: FilePath)) act.extension.ref
    ]
  where
    file = SecretManager.SecretFile (Core.Project "acme-prod") "db-password" "latest" (Placement "/etc/app/db-password" "app" "app" "0600")
    argsOf cmd = case cmdspec (prepare SecretManager.secretManagerCommand cmd) of
        RawCommand _ args -> args
        ShellCommand s -> [s]
