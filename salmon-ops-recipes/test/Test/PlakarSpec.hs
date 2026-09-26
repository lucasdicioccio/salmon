{-# LANGUAGE OverloadedStrings #-}

{- | Coverage for "Salmon.Builtin.Nodes.Plakar".

Layer 0 is the verdicts, the refusals and the rendered script. Layer 2 runs a
real @plakar@ if there is one on PATH (skipped loudly otherwise): it makes a
store through the node, runs the generated script, and reads the freshness
verdict off a real listing, empty store included -- the parts whose format
this module could only get right by running plakar.
-}
module Test.PlakarSpec (tests) where

import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime (..), addUTCTime, getCurrentTime)
import Data.Time.Calendar (fromGregorian)
import GHC.IO.Exception (ExitCode (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Control.Exception (finally)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension (ignoreTrack)
import Salmon.Builtin.Nodes.CronTask (dailyAt)
import Salmon.Builtin.Nodes.Plakar
import Test.Harness

now :: UTCTime
now = UTCTime (fromGregorian 2026 9 26) 36000 -- 2026-09-26T10:00:00Z

store :: KlosetStore
store = KlosetStore "/var/backups/kloset" "/etc/plakar.key"

job :: PlakarJob
job = PlakarJob "nightly" store "/srv/data" (dailyAt "3" "17") "root" (keepDays 30) (26 * 3600) "/opt/salmon/plakar/nightly.sh"

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Plakar"
        [ testGroup
            "verdicts"
            [ testCase "version is found among the welcome text" $
                assertEqual "" Success (interpretVersion "1.1.7" "Welcome to plakar !\n\nEOT\nplakar/v1.1.7\n")
            , testCase "another version is not the wanted one" $
                assertEqual "" (Failure "plakar is not at 1.1.7") (interpretVersion "1.1.7" "plakar/v1.1.6\n")
            , testCase "a snapshot inside the limit is fresh" $
                assertEqual "" Success (interpretSnapshotList now (26 * 3600) ExitSuccess "2026-09-25T20:00:00Z   28ff740a       6 B        0s /srv/data\n" "")
            , testCase "an old one is reported with its age and the limit" $
                assertEqual
                    ""
                    (Failure "newest snapshot is 30h old, the limit is 26h")
                    (interpretSnapshotList now (26 * 3600) ExitSuccess "2026-09-25T04:00:00Z   28ff740a       6 B        0s /srv/data\n" "")
            , testCase "the welcome text before the listing is skipped" $
                assertEqual "" Success (interpretSnapshotList now (26 * 3600) ExitSuccess "Welcome to plakar !\n2026-09-26T09:00:00Z   28ff740a   6 B  0s /d\n" "")
            , testCase "nothing printed, nothing said: the store has no snapshot" $
                assertEqual "" (Failure "the store has no snapshot") (interpretSnapshotList now 3600 ExitSuccess "" "")
            , testCase "nothing printed and stderr complaining: cannot tell (plakar exits 0 when its cache process fails)" $
                assertEqual "" Unknown (interpretSnapshotList now 3600 ExitSuccess "" "plakar: failed to run cached\n")
            , testCase "a failing listing is cannot tell" $
                assertEqual "" Unknown (interpretSnapshotList now 3600 (ExitFailure 77) "" "failed to unlock repository")
            ]
        , testGroup
            "refusals"
            [ testCase "an empty keyfile" $ assertEqual "" (Just "is empty") (keyfileProblem 0 0o600)
            , testCase "a keyfile others can read" $
                assertBool "" (keyfileProblem 32 0o640 /= Nothing)
            , testCase "an owner-only keyfile is fine" $ assertEqual "" Nothing (keyfileProblem 32 0o400)
            , testCase "a retention policy needs a rule" $
                assertBool "" (either (const True) (const False) (pruneArgs (KeepPolicy Nothing Nothing Nothing)))
            , testCase "a retention rule must be positive" $
                assertBool "" (either (const True) (const False) (pruneArgs (keepDays 0)))
            , testCase "the rules become prune filters" $
                assertEqual "" (Right ["-hours", "6", "-days", "30"]) (pruneArgs (KeepPolicy (Just 6) (Just 30) Nothing))
            ]
        , testGroup
            "script"
            [ testCase "backs up, insists on the success line, then prunes with the declared policy" $ do
                let s = renderBackupScript job ["-days", "30"]
                    ls = Text.lines s
                assertBool "strict shell" ("set -euo pipefail" `elem` ls)
                assertBool "backup" (any ("backup '/srv/data'" `Text.isInfixOf`) ls)
                assertBool "success line" (any ("completed without errors" `Text.isInfixOf`) ls)
                assertEqual "prune last" "plakar -keyfile '/etc/plakar.key' at '/var/backups/kloset' prune -days 30 -apply" (last ls)
            , testCase "a path with a quote is quoted" $
                assertBool "" ("'/srv/it'\\''s'" `Text.isInfixOf` renderBackupScript job{jobSource = "/srv/it's"} ["-days", "1"])
            , testCase "the digest is lower-case hex" $
                assertEqual "" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" (sha256Hex "")
            ]
        , testCase "a job with no retention rule is refused rather than run" $
            assertBool "" (either (const True) (const False) (pruneArgs (jobKeep job{jobKeep = KeepPolicy Nothing Nothing Nothing})))
        , testCase "against a real plakar: store, backup script, freshness" realPlakar
        ]

realPlakar :: IO ()
realPlakar = requireExecutable "plakar" $ withTempDir $ \tmp -> do
  -- the suite runs in one process: put HOME back for whoever runs next
  oldHome <- lookupEnv "HOME"
  (`finally` maybe (unsetEnv "HOME") (setEnv "HOME") oldHome) $ do
      -- plakar's cache process listens on a unix socket under $HOME/.cache, so
      -- the path must stay short.
      let home = tmp </> "h"
          st = KlosetStore (tmp </> "s") (tmp </> "key")
          empty = KlosetStore (tmp </> "e") (tmp </> "key")
          src = tmp </> "d"
      createDirectoryIfMissing True home
      createDirectoryIfMissing True src
      setEnv "HOME" home
      writeFile (src </> "a.txt") "hello\n"
      writeFile (tmp </> "key") "correct horse battery staple, at some length\n"
      setFileMode (tmp </> "key") 0o600
      ok1 <- runUp (kloset ignoreTrack st)
      ok2 <- runUp (kloset ignoreTrack empty)
      assertBool "stores made" (ok1 && ok2)
      made <- doesFileExist (tmp </> "s" </> "CONFIG")
      assertBool "CONFIG written" made
      let script = tmp </> "job.sh"
          j = job{jobStore = st, jobSource = src, jobScriptPath = script}
      Text.writeFile script (renderBackupScript j ["-days", "1"])
      (code, _, err) <- readProcessWithExitCode "bash" [script] ""
      assertEqual ("script: " <> err) ExitSuccess code
      t <- getCurrentTime
      (_, out, lerr) <- readProcessWithExitCode "plakar" ["-keyfile", tmp </> "key", "at", tmp </> "s", "ls", "-latest"] ""
      assertEqual "fresh after the script" Success (interpretSnapshotList t 3600 ExitSuccess (Text.pack out) (Text.pack lerr))
      assertEqual "stale an hour on" (Failure "newest snapshot is 2h old, the limit is 1h") (interpretSnapshotList (addUTCTime 7200 t) 3600 ExitSuccess (Text.pack out) (Text.pack lerr))
      (_, out2, lerr2) <- readProcessWithExitCode "plakar" ["-keyfile", tmp </> "key", "at", tmp </> "e", "ls", "-latest"] ""
      assertEqual "empty store" (Failure "the store has no snapshot") (interpretSnapshotList t 3600 ExitSuccess (Text.pack out2) (Text.pack lerr2))
