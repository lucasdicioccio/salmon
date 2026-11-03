{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.RemoteFile".

As in "Test.SecretDeliverySpec", the remote half is a pair of shell scripts,
run here under a local @sh@ (no ssh, no sudo, the file owned by whoever runs
the suite); the ssh leg is asserted on as a rendered command line only.
-}
module Test.RemoteFileSpec (tests) where

import Control.Exception (SomeException, try)
import Data.Bits ((.&.))
import qualified Data.ByteString.Char8 as C8
import Data.Either (isLeft, isRight)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import System.Process (readProcess)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CmdSpec (..), cmdspec, proc)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (prepareIO)
import Salmon.Builtin.Nodes.RemoteFile
import Salmon.Builtin.Nodes.SecretDelivery (Elevation (..), Placement (..), DeliveryCommand (..), interpretProbe, remoteCommand, validateFilePlacement, validatePlacement)
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter (silent)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.RemoteFile"
        [ testGroup "validateFilePlacement" placementTests
        , testGroup "remote scripts, run locally" scriptTests
        , testGroup "ssh command" commandTests
        , testGroup "node" nodeTests
        ]

unit :: C8.ByteString
unit = "[Unit]\nDescription=it's a unit\n\n[Service]\nExecStart=/bin/true\n"

whoami :: IO (Text, Text)
whoami = do
    u <- readProcess "id" ["-un"] ""
    g <- readProcess "id" ["-gn"] ""
    pure (Text.strip (Text.pack u), Text.strip (Text.pack g))

placementAt :: FilePath -> Text -> IO Placement
placementAt path mode = do
    (u, g) <- whoami
    either (assertFailure . Text.unpack) pure (validateFilePlacement (Placement path u g mode))

runScript :: Text -> C8.ByteString -> IO ExitCode
runScript script input = do
    (code, _out, _err) <- readCreateProcessWithExitCode (proc "sh" ["-c", Text.unpack script]) input
    pure code

modeOf :: FilePath -> IO Int
modeOf path = fromIntegral . (.&. 0o7777) . fileMode <$> getFileStatus path

placementTests :: [TestTree]
placementTests =
    [ testCase "accepts a world-readable mode, normalized" $
        assertEqual "" (Right "644") (placeMode <$> validateFilePlacement (Placement "/etc/systemd/system/x.service" "root" "root" "0644"))
    , testCase "validatePlacement still refuses that same mode" $
        assertBool "" (isLeft (validatePlacement (Placement "/etc/systemd/system/x.service" "root" "root" "0644")))
    , testCase "and still accepts what it accepted" $
        assertBool "" (isRight (validatePlacement (Placement "/etc/x/secret" "root" "root" "0640")))
    , testCase "keeps the shape rules: relative path, shell in the owner, a non-octal mode" $ do
        assertBool "relative" (isLeft (validateFilePlacement (Placement "x.service" "root" "root" "0644")))
        assertBool "owner" (isLeft (validateFilePlacement (Placement "/etc/x" "root; rm -rf /" "root" "0644")))
        assertBool "mode" (isLeft (validateFilePlacement (Placement "/etc/x" "root" "root" "a+r")))
    ]

scriptTests :: [TestTree]
scriptTests =
    [ testCase "contentDigest is sha256sum's" $ do
        out <- readProcess "sh" ["-c", "printf 'abc' | sha256sum"] ""
        assertEqual "" (Text.pack (takeWhile (/= ' ') out)) (contentDigest "abc")
    , testCase "write puts the bytes in place with the declared mode, creating the directory" $
        withSystemTempDirectory "salmon-remote-file" $ \dir -> do
            let path = dir </> "it's here" </> "x.service"
            place <- placementAt path "0644"
            code <- runScript (writeScript (place, contentDigest unit)) unit
            assertEqual "exit" ExitSuccess code
            got <- C8.readFile path
            assertEqual "contents" unit got
            mode <- modeOf path
            assertEqual "mode" 0o644 mode
            dirMode <- modeOf (dir </> "it's here")
            assertEqual "directory mode" 0o755 dirMode
            left <- listDirectory (dir </> "it's here")
            assertEqual "no temporary file left behind" ["x.service"] left
    , testCase "write refuses bytes that are not the declared ones, keeping the old file" $
        withSystemTempDirectory "salmon-remote-file" $ \dir -> do
            let path = dir </> "x.service"
            C8.writeFile path "old"
            place <- placementAt path "0644"
            code <- runScript (writeScript (place, contentDigest unit)) (C8.take 10 unit)
            assertEqual "exit" (ExitFailure 7) code
            got <- C8.readFile path
            assertEqual "contents" "old" got
            left <- listDirectory dir
            assertEqual "no temporary file left behind" ["x.service"] left
    , testCase "an empty file is written" $
        withSystemTempDirectory "salmon-remote-file" $ \dir -> do
            let path = dir </> "empty"
            place <- placementAt path "0644"
            code <- runScript (writeScript (place, contentDigest "")) ""
            assertEqual "exit" ExitSuccess code
            probed <- runScript (digestProbeScript (place, contentDigest "")) ""
            assertEqual "probe" ExitSuccess probed
    , testCase "probe: absent, matching, differing, wrong mode" $
        withSystemTempDirectory "salmon-remote-file" $ \dir -> do
            let path = dir </> "x.service"
                arg place = (place, contentDigest unit)
            place <- placementAt path "0644"
            absent <- runScript (digestProbeScript (arg place)) ""
            assertEqual "absent" (ExitFailure 3) absent
            _ <- runScript (writeScript (arg place)) unit
            same <- runScript (digestProbeScript (arg place)) ""
            assertEqual "matching" ExitSuccess same
            differing <- runScript (digestProbeScript (place, contentDigest "something else")) ""
            assertEqual "differing" (ExitFailure 4) differing
            setFileMode path 0o600
            moded <- runScript (digestProbeScript (arg place)) ""
            assertEqual "wrong mode" (ExitFailure 5) moded
            assertEqual "verdict" (Failure (Text.pack path <> " has the wrong owner or mode")) (interpretProbe place moded)
    , testCase "the command handed to ssh survives the remote shell's own parsing" $
        withSystemTempDirectory "salmon-remote-file" $ \dir -> do
            let path = dir </> "it's here" </> "x.service"
            place <- placementAt path "0644"
            code <- runScript (Text.pack (remoteCommand AsLoginUser (writeScript (place, contentDigest unit)))) unit
            assertEqual "exit" ExitSuccess code
            probed <- runScript (Text.pack (remoteCommand AsLoginUser (digestProbeScript (place, contentDigest unit)))) ""
            assertEqual "probe" ExitSuccess probed
    ]

commandTests :: [TestTree]
commandTests =
    [ testCase "BatchMode, the caller's identity, then sudo -n sh -c and one quoted script" $ do
        p <- prepareIO remoteFileCommand (DeliveryCommand opts (Ssh.Remote "salmon" "203.0.113.7") WithSudo (writeScript (place, contentDigest unit))) ()
        case cmdspec p of
            RawCommand "ssh" args -> do
                assertEqual "" ["-o", "BatchMode=yes", "-i", "/keys/client", "-o", "IdentitiesOnly=yes"] (take 6 args)
                assertEqual "login" "salmon@203.0.113.7" (args !! 6)
                assertBool "sudo -n sh -c '...'" ("sudo -n sh -c 'set -eu\n" `isPrefixOf` last args)
            other -> assertFailure (show other)
    ]
  where
    place = Placement "/etc/systemd/system/x.service" "root" "root" "644"
    opts = Ssh.ClientOpts (Just "/keys/client") Nothing

nodeTests :: [TestTree]
nodeTests =
    [ testCase "help names the site, notes carry owner, mode and the digest" $
        case opAct (node unit) of
            Nothing -> assertFailure "not a node"
            Just act -> do
                assertEqual "help" "writes 62 bytes to salmon@203.0.113.7:/etc/systemd/system/x.service" act.extension.help
                assertEqual "notes" ["owner root:root, mode 0644", "sha256 " <> contentDigest unit] act.extension.notes
    , testCase "the ref is the remote host and path" $
        case (opAct (node unit), opAct (node "other")) of
            (Just a, Just b) -> do
                assertEqual "same site" a.extension.ref b.extension.ref
                assertEqual "" (mkRef "remote-file" ("203.0.113.7" :: Text, "/etc/systemd/system/x.service" :: FilePath)) a.extension.ref
            _ -> assertFailure "not a node"
    , testCase "changed contents change the notes" $
        case (opAct (node unit), opAct (node "other")) of
            (Just a, Just b) -> assertBool "" (a.extension.notes /= b.extension.notes)
            _ -> assertFailure "not a node"
    , testCase "an invalid placement is refused by check and up alike, before any connection" $
        case opAct (remoteFile Ssh.noClientOpts silent ignoreTrack (RemoteFile unit remote (Placement "relative" "root" "root" "0644") WithSudo)) of
            Nothing -> assertFailure "not a node"
            Just act -> do
                verdict <- act.extension.check
                assertEqual "" (Failure "relative: the path must be absolute") verdict
                result <- try act.extension.up
                assertBool "up throws" (either (\(_ :: SomeException) -> True) (const False) result)
    ]
  where
    remote = Ssh.Remote "salmon" "203.0.113.7"
    node bytes = remoteFile Ssh.noClientOpts silent ignoreTrack (RemoteFile bytes remote (Placement "/etc/systemd/system/x.service" "root" "root" "0644") WithSudo)
