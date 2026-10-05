{-# LANGUAGE OverloadedStrings #-}

{- | Layer 2 for "Salmon.Builtin.Nodes.Podman.Quadlet": 'Quadlet.quadletContainer'
itself, run through 'upTree' and 'downTree' against this user's own systemd
manager and podman, in user scope. No root: the quadlet goes in
@~\/.config\/containers\/systemd@, every @systemctl@ is @--user@, and the
container is rootless.

What is asserted is what the node claims in its haddock: @up@ writes the
file, reloads and leaves the generated service running a container; a second
pass is a 'Skip'; a changed image reference and a changed env file each
restart the service (a new @InvocationID@ and a new container) and an
unchanged one does not; an env file rotated under a pass that rewrote the
quadlet without restarting the service is still a restart at the next pass; a
container started from a quadlet written before the watched files' digest was
keyed is reloaded and /not/ restarted by the pass that keys it, and is
restarted by a rotated env file afterwards; @down@ stops the service, removes the file and the
generated unit goes with it; and a declaration moved onto an image that
cannot be pulled fails at the pull, with the running container, its unit and
its file as they were.

Skipped loudly when @podman@, @podman-user-generator@, a running
@systemd --user@ or the image is missing. The image is one that needs no
registry login, and is only ever used if it is already on the machine; the
second reference the image-change case needs is a local tag of it, made and
removed here. Every container gets a random name, and a bracket removes its
quadlet file, stops its unit, removes its container and the tag, and reloads,
whatever the case did.
-}
module Test.QuadletUserSpec (tests) where

import Control.Exception (bracket, finally, throwIO)
import Control.Monad (forM_, unless, void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Numeric (showHex)
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getModificationTime, getXdgDirectory, listDirectory, removeDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.User (getRealUserID)
import System.Process (readProcessWithExitCode)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.CPUTime (getCPUTime)
import System.Posix.Process (getProcessID)
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCaseSteps)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Systemd.Job as Job
import Salmon.Op.Actions (Act (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter (silent)
import Test.Harness (runDownCapturing, runUpCapturing, withTempDir)

tests :: TestTree
tests =
    -- one after the other: both share the user's quadlet directory, which
    -- whichever case created it removes again only once it is empty
    sequentialTestGroup
        "Salmon.Builtin.Nodes.Podman.Quadlet (user scope, Layer 2)"
        AllFinish
        [ testCaseSteps "up runs it, a second pass skips, down removes it" $ \step ->
            withUserQuadlets $ \unitDir -> withFreshContainer unitDir $ \c -> lifecycle step c
        , testCaseSteps "a new image or env file restarts it, the same ones do not" $ \step ->
            withUserQuadlets $ \unitDir -> withFreshContainer unitDir $ \c -> restarts step c
        , testCaseSteps "two changed quadlets, one reload: the one not restarted is not skipped" $ \step ->
            withUserQuadlets $ \unitDir ->
                withFreshContainer unitDir $ \a -> withFreshContainer unitDir $ \b -> sharedReload step a b
        , testCaseSteps "a rotated env file behind a failed predecessor, reloaded by a sibling, is not skipped" $ \step ->
            withUserQuadlets $ \unitDir ->
                withFreshContainer unitDir $ \a -> withFreshContainer unitDir $ \b -> rotatedEnv step a b
        , testCaseSteps "a quadlet from before the watched files' key is rewritten without a restart" $ \step ->
            withUserQuadlets $ \unitDir -> withFreshContainer unitDir $ \c -> keying step c
        , testCaseSteps "an image that cannot be pulled leaves the running container alone" $ \step ->
            withUserQuadlets $ \unitDir -> withFreshContainer unitDir $ \c -> unpullable step c
        , testCaseSteps "a stamped container job runs once, again after a change, and fails while its last run did" $ \step ->
            withUserQuadlets $ \unitDir -> withFreshContainer unitDir $ \c -> withTempDir $ \tmp -> completing step tmp c
        ]

-- | Small, long-running with no arguments, no published port, no login.
image :: Text
image = "docker.io/library/nginx:alpine"

-------------------------------------------------------------------------------

lifecycle :: (String -> IO ()) -> Quadlet.Container -> IO ()
lifecycle step c = do
    step "first up"
    reports <- runUpCapturing (node c)
    assertUp reports
    assertEqual "the quadlet node was not applied once" 1 (count isEval reports)
    present <- doesFileExist (Quadlet.quadletPath c)
    assertBool "no quadlet file was written" present
    active <- showProperty c "ActiveState"
    assertEqual "the generated service is not running" "active" active
    state <- showProperty c "UnitFileState"
    assertEqual "the service is not the generator's" "generated" state
    running <- containerRunning c
    assertBool "no container is running under the container's name" running
    invocation <- showProperty c "InvocationID"
    container <- containerId c
    -- where the generator put the unit, to check it is gone after down
    fragment <- Text.unpack <$> showProperty c "FragmentPath"
    generated <- doesFileExist fragment
    assertBool ("the generated unit is not at its FragmentPath " <> fragment) generated
    sourced <- showProperty c "SourcePath"
    assertEqual "the generated unit does not name the quadlet as its source" (Text.pack (Quadlet.quadletPath c)) sourced

    step "second up"
    again <- runUpCapturing (node c)
    assertUp again
    assertEqual "the second pass was not a skip" (1, 0) (count isSkip again, count isEval again)
    assertEqual "the second pass restarted the service" invocation =<< showProperty c "InvocationID"
    assertEqual "the second pass replaced the container" container =<< containerId c

    step "down"
    down <- runDownCapturing (node c)
    assertBool ("down failed: " <> show (failures down)) (null (failures down))
    gone <- not <$> doesFileExist (Quadlet.quadletPath c)
    assertBool "the quadlet file is still there" gone
    stillRunning <- containerRunning c
    assertBool "the container is still there" (not stillRunning)
    load <- showProperty c "LoadState"
    assertEqual "the generated unit outlived its file" "not-found" load
    leftover <- doesFileExist fragment
    assertBool ("the generator's output is still on disk at " <> fragment) (not leftover)

restarts :: (String -> IO ()) -> Quadlet.Container -> IO ()
restarts step c0 = withTempDir $ \dir -> withLocalTag $ \alias -> do
    let envFile = dir </> "env"
        c = c0{Quadlet.containerEnvFile = Just envFile}
    writeFile envFile "SALMON_QUADLET_TEST=one\n"

    step "first up"
    assertUp =<< runUpCapturing (node c)
    assertEqual "the env file was not handed to the container" "one" =<< containerEnv c
    (i1, k1) <- identity c

    step "unchanged up"
    again <- runUpCapturing (node c)
    assertUp again
    assertEqual "an unchanged declaration was applied" 0 (count isEval again)
    assertEqual "an unchanged declaration restarted it" (i1, k1) =<< identity c

    step "changed env file"
    writeFile envFile "SALMON_QUADLET_TEST=two\n"
    envChanged <- runUpCapturing (node c)
    assertUp envChanged
    assertEqual "a changed env file was not applied" 1 (count isEval envChanged)
    (i2, k2) <- identity c
    assertBool "a changed env file did not restart the service" (i2 /= i1)
    assertBool "a changed env file left the old container" (k2 /= k1)
    assertEqual "the restarted container has the old env" "two" =<< containerEnv c

    step "changed image reference"
    let moved = c{Quadlet.containerImage = alias}
    imageChanged <- runUpCapturing (node moved)
    assertUp imageChanged
    assertEqual "a changed image was not applied" 1 (count isEval imageChanged)
    (i3, k3) <- identity moved
    assertBool "a changed image did not restart the service" (i3 /= i2)
    assertBool "a changed image left the old container" (k3 /= k2)
    assertEqual "the container does not run the new reference" alias =<< containerImage moved

    step "unchanged up after the changes"
    settled <- runUpCapturing (node moved)
    assertUp settled
    assertEqual "an unchanged declaration was applied" 0 (count isEval settled)
    assertEqual "an unchanged declaration restarted it" (i3, k3) =<< identity moved

    step "down"
    down <- runDownCapturing (node moved)
    assertBool ("down failed: " <> show (failures down)) (null (failures down))
    stillRunning <- containerRunning moved
    assertBool "the container is still there" (not stillRunning)
  where
    -- the second reference the image change needs: a local tag of the same
    -- image, so the case needs nothing from a registry
    withLocalTag :: (Text -> IO a) -> IO a
    withLocalTag act = do
        let alias = "localhost/" <> Podman.getContainerName c0.containerName <> ":moved"
        (code, _, err) <- readProcessWithExitCode "podman" ["tag", Text.unpack image, Text.unpack alias] ""
        unless (code == ExitSuccess) $ assertFailure ("podman tag failed: " <> err)
        -- `podman untag IMAGE` with no name removes /every/ name the image
        -- has, the shared one included: name the alias twice, as the image
        -- and as the one name to remove
        act alias `finally` void (readProcessWithExitCode "podman" ["untag", Text.unpack alias, Text.unpack alias] "")

    identity c = (,) <$> showProperty c "InvocationID" <*> containerId c

{- | What an interrupted pass leaves behind: both quadlet files rewritten, and
a @daemon-reload@ run on behalf of one of them. systemd then reads
@NeedDaemonReload=no@ for /both/ units, and only the running container can
say that the second one was never restarted.
-}
sharedReload :: (String -> IO ()) -> Quadlet.Container -> Quadlet.Container -> IO ()
sharedReload step a0 b0 = do
    step "both up"
    assertUp =<< runUpCapturing (node a0)
    assertUp =<< runUpCapturing (node b0)
    invocation <- showProperty b0 "InvocationID"
    container <- containerId b0

    step "both files changed, one daemon-reload, neither service restarted"
    let a = a0{Quadlet.containerDescription = "changed (a)"}
        b = b0{Quadlet.containerDescription = "changed (b)"}
    Text.writeFile (Quadlet.quadletPath a) =<< Quadlet.renderQuadlet a
    Text.writeFile (Quadlet.quadletPath b) =<< Quadlet.renderQuadlet b
    (code, _, err) <- readProcessWithExitCode "systemctl" ["--user", "daemon-reload"] ""
    unless (code == ExitSuccess) $ assertFailure ("daemon-reload failed: " <> err)
    assertEqual "systemd still remembers the file changed; the case is not the one it means to be" "no" =<< showProperty b "NeedDaemonReload"
    assertEqual "the service was restarted by the reload" invocation =<< showProperty b "InvocationID"

    step "the check"
    verdict <- Quadlet.checkContainer b
    case verdict of
        UpDown.Failure _ -> pure ()
        other -> assertFailure ("a quadlet whose service was never restarted reads " <> show other)

    step "up"
    reports <- runUpCapturing (node b)
    assertUp reports
    assertEqual "the changed quadlet was skipped" (1, 0) (count isEval reports, count isSkip reports)
    assertBool "the service was not restarted" . (/= invocation) =<< showProperty b "InvocationID"
    assertBool "the old container is still the one running" . (/= container) =<< containerId b
    assertEqual "the restarted service is not satisfied" UpDown.Success =<< Quadlet.checkContainer b

    step "down"
    forM_ [a, b] $ \c -> do
        down <- runDownCapturing (node c)
        assertBool ("down failed: " <> show (failures down)) (null (failures down))

{- | 'sharedReload' as it was met in a deployment, with the env file and not
the declaration as what changed: two quadlets whose env files are rotated,
the second with a predecessor (a migration, say) that fails. Its quadlet
file is rewritten all the same -- the predecessor is injected on the node,
beside the file, not declared in the track -- its service is blocked, and
the first quadlet's @up@ reloads the machine. systemd then has no memory
that the second file changed, the image is the one it was, and only the
running container's label says it was started with the previous env file.

The two quadlets of the failing pass are walked one after the other, the
blocked one first, so that the order the case depends on is not left to the
walk.
-}
rotatedEnv :: (String -> IO ()) -> Quadlet.Container -> Quadlet.Container -> IO ()
rotatedEnv step a0 b0 = withTempDir $ \dir -> do
    let envOf name = dir </> name
        a = a0{Quadlet.containerEnvFile = Just (envOf "a.env")}
        b = b0{Quadlet.containerEnvFile = Just (envOf "b.env")}
        rotate value = forM_ ["a.env", "b.env"] $ \name -> writeFile (envOf name) ("SALMON_QUADLET_TEST=" <> value <> "\n")
    failing <- newIORef False
    let predecessor =
            op "quadlet-test-predecessor" nodeps $ \actions ->
                actions
                    { ref = mkRef "quadlet-test-predecessor" (Podman.getContainerName b.containerName)
                    , up = do
                        broken <- readIORef failing
                        when broken (throwIO (userError "the predecessor fails"))
                    }
        nodeB = node b `inject` predecessor

    step "both up"
    rotate "one"
    assertUp =<< runUpCapturing (node a)
    assertUp =<< runUpCapturing nodeB
    assertEqual "" "one" =<< containerEnv b
    invocation <- showProperty b "InvocationID"
    container <- containerId b

    step "both env files rotated; b's predecessor fails, a is brought up"
    rotate "two"
    writeIORef failing True
    blocked <- runUpCapturing nodeB
    assertEqual "the predecessor is not what failed" ["quadlet-test-predecessor"] [act.shorthand | UpDown.Failed act _ <- blocked]
    assertEqual "b's service was not blocked" 1 (length [() | UpDown.Blocked act <- blocked, act.shorthand == "podman-quadlet"])
    rotated <- Quadlet.renderQuadlet b
    assertEqual "b's quadlet was not rewritten; the case is not the one it means to be" rotated =<< Text.readFile (Quadlet.quadletPath b)
    assertUp =<< runUpCapturing (node a)
    assertEqual "" "two" =<< containerEnv a
    assertEqual "systemd still remembers b's file changed; the case is not the one it means to be" "no" =<< showProperty b "NeedDaemonReload"
    assertEqual "b was restarted by a's pass" invocation =<< showProperty b "InvocationID"
    assertEqual "" "one" =<< containerEnv b

    step "the check"
    verdict <- Quadlet.checkContainer b
    case verdict of
        UpDown.Failure _ -> pure ()
        other -> assertFailure ("a container still running on the previous env file reads " <> show other)

    step "the next pass, nothing failing"
    writeIORef failing False
    reports <- runUpCapturing nodeB
    assertUp reports
    assertEqual "b was skipped" (1, 0) (count isEval reports, count isSkip reports)
    assertBool "b's service was not restarted" . (/= invocation) =<< showProperty b "InvocationID"
    assertBool "b's old container is still the one running" . (/= container) =<< containerId b
    assertEqual "b still presents the previous env file" "two" =<< containerEnv b

    step "and then it is left alone"
    settled <- runUpCapturing nodeB
    assertUp settled
    assertEqual "a settled quadlet was applied again" (0, 1) (count isEval settled, count isSkip settled)

    step "down"
    forM_ [a, b] $ \c -> do
        down <- runDownCapturing (node c)
        assertBool ("down failed: " <> show (failures down)) (null (failures down))

{- | What an upgrade meets: a service running from a quadlet whose trailing
comment is a plain hash of its env file and whose label was made from it.
The file has to be rewritten (that hash is what must go) and the service has
no reason to restart: it runs this declaration with this env file.

The old file is spelled here as it was written then, from
'Quadlet.renderContainer' and 'Systemd.withWatchedFingerprint'.
-}
keying :: (String -> IO ()) -> Quadlet.Container -> IO ()
keying step c0 = withTempDir $ \dir -> do
    let envFile = dir </> "env"
        c = c0{Quadlet.containerEnvFile = Just envFile}
        labelled fingerprint = "Label=" <> Quadlet.quadletLabel <> "=" <> fingerprint
        identity = (,) <$> showProperty c "InvocationID" <*> containerId c
        label = podmanInspect c ("{{index .Config.Labels \"" <> Text.unpack Quadlet.quadletLabel <> "\"}}")
    writeFile envFile "SALMON_QUADLET_TEST=one\n"

    step "a service started from a quadlet written before the key"
    unkeyed <- Quadlet.unkeyedFingerprint c
    plain <- Text.strip <$> Systemd.withWatchedFingerprint [envFile] ""
    let old =
            Text.unlines $
                concat
                    [ [labelled unkeyed | "EnvironmentFile=" `Text.isPrefixOf` l] <> [l]
                    | l <- Text.lines (Quadlet.renderContainer c)
                    ]
                    <> [plain]
        target = Text.unpack (Quadlet.serviceTarget c)
    Text.writeFile (Quadlet.quadletPath c) old
    (reloaded, _, reloadErr) <- readProcessWithExitCode "systemctl" ["--user", "daemon-reload"] ""
    unless (reloaded == ExitSuccess) $ assertFailure ("daemon-reload failed: " <> reloadErr)
    (started, _, startErr) <- readProcessWithExitCode "systemctl" ["--user", "start", target] ""
    unless (started == ExitSuccess) $ assertFailure ("the old quadlet did not start: " <> startErr)
    assertEqual "" "one" =<< containerEnv c
    assertEqual "the container does not carry the old label" (Just unkeyed) =<< label
    before <- identity

    step "the pass that keys the digest"
    reports <- runUpCapturing (node c)
    assertUp reports
    assertEqual "the rewritten quadlet was not reloaded" 1 (count isEval reports)
    assertEqual "the service was restarted, or its container replaced" before =<< identity
    written <- Text.readFile (Quadlet.quadletPath c)
    declared <- Quadlet.quadletFingerprint c
    assertBool "the keyed fingerprint is the old one" (declared /= unkeyed)
    assertEqual "the file is not the keyed one" written =<< Quadlet.renderQuadlet c
    assertBool "the plain hash is still in the file" (not (plain `Text.isInfixOf` written))
    assertBool "the old label is still in the file" (not (unkeyed `Text.isInfixOf` written))
    execStart <- showProperty c "ExecStart"
    assertBool "the generated unit was not made from the keyed file" ((Quadlet.quadletLabel <> "=" <> declared) `Text.isInfixOf` execStart)
    assertBool "the generated unit still carries the old label" (not (unkeyed `Text.isInfixOf` execStart))
    assertEqual "" "no" =<< showProperty c "NeedDaemonReload"
    assertEqual "the adopted container is not satisfied" UpDown.Success =<< Quadlet.checkContainer c

    step "and then it is left alone"
    settled <- runUpCapturing (node c)
    assertUp settled
    assertEqual "a settled quadlet was applied again" (0, 1) (count isEval settled, count isSkip settled)
    assertEqual "a settled quadlet was restarted" before =<< identity

    step "a rotated env file still restarts it"
    writeFile envFile "SALMON_QUADLET_TEST=two\n"
    rotated <- runUpCapturing (node c)
    assertUp rotated
    assertEqual "a changed env file was not applied" 1 (count isEval rotated)
    after <- identity
    assertBool "a changed env file did not restart the service" (fst after /= fst before)
    assertBool "a changed env file left the old container" (snd after /= snd before)
    assertEqual "" "two" =<< containerEnv c
    now <- Quadlet.quadletFingerprint c
    assertEqual "the restarted container does not carry the keyed label" (Just now) =<< label

    step "down"
    down <- runDownCapturing (node c)
    assertBool ("down failed: " <> show (failures down)) (null (failures down))
    stillRunning <- containerRunning c
    assertBool "the container is still there" (not stillRunning)

{- | A healthy container re-declared onto an image no registry will hand
over. Left to the service's start, the pull came after the stop and the
restart took the container down; the pull is a node ahead of the file now,
so the pass fails there and nothing that was running or written changes.

The reference is on a port of this machine nothing listens on, so the pull
is refused at once and reaches nothing outside.
-}
unpullable :: (String -> IO ()) -> Quadlet.Container -> IO ()
unpullable step c = do
    step "first up"
    assertUp =<< runUpCapturing (node c)
    before <- identity
    written <- Text.readFile (Quadlet.quadletPath c)

    step "up onto an image that cannot be pulled"
    let moved = c{Quadlet.containerImage = "localhost:1/salmon-quadlet-test/unpullable:v1"}
    reports <- runUpCapturing (node moved)
    assertEqual
        "the pull is not what failed"
        ["podman-quadlet-image"]
        [act.shorthand | UpDown.Failed act _ <- reports]
    assertEqual "the quadlet node was not blocked" 1 (length [() | UpDown.Blocked act <- reports, act.shorthand == "podman-quadlet"])
    assertEqual "the quadlet node was applied" 0 (count isEval reports)
    assertEqual "the quadlet file was rewritten" written =<< Text.readFile (Quadlet.quadletPath c)
    assertEqual "the service is not running any more" "active" =<< showProperty c "ActiveState"
    assertEqual "the service was restarted, or its container replaced" before =<< identity
    assertEqual "systemd was told the unit changed" "no" =<< showProperty c "NeedDaemonReload"

    step "the previous declaration is still satisfied"
    again <- runUpCapturing (node c)
    assertUp again
    assertEqual "the previous declaration was applied again" 0 (count isEval again)
    assertEqual "the previous declaration restarted it" before =<< identity

    step "down"
    down <- runDownCapturing (node c)
    assertBool ("down failed: " <> show (failures down)) (null (failures down))
    stillRunning <- containerRunning c
    assertBool "the container is still there" (not stillRunning)
  where
    identity = (,) <$> showProperty c "InvocationID" <*> containerId c

{- | A container /job/ that leaves a stamp, under 'Quadlet.completedQuadletJob':
nothing keeps its generated unit loaded, so the stamp is all that remembers
a successful run.
-}
completing :: (String -> IO ()) -> FilePath -> Quadlet.Container -> IO ()
completing step tmp c0 = do
    let stamp = tmp </> "stamp"
        jobOf command =
            c0
                { Quadlet.containerExec = ["/bin/sh", "-c", command]
                , Quadlet.containerLifetime = Quadlet.RunToCompletion
                , Quadlet.containerStamp = Just stamp
                }
        c = jobOf "exit 0"
        completed = Quadlet.completedQuadletJob silent ignoreTrack ignoreTrack
        ran = "systemd-job-completed"
        countOf :: (UpDown.Report Extension -> Maybe (Act Extension)) -> [UpDown.Report Extension] -> Int
        countOf which reports = length [() | Just act <- map which reports, act.shorthand == ran]
        verdict = Job.checkLastRun . Quadlet.containerCompletion

    step "first up runs it"
    reports <- runUpCapturing (completed c)
    assertUp reports
    assertEqual "the job was not run once" 1 (countOf isEval reports)
    assertBool "the run left no stamp" =<< doesFileExist stamp
    assertBool "the run left its running stamp" . not =<< doesFileExist (Job.stampRunning stamp)
    assertBool "the job's container outlived its run" . not =<< containerRunning c
    -- nothing refers to the generated unit, so systemd has forgotten the run
    assertEqual "" "" =<< showProperty c "ExecMainStartTimestamp"
    assertEqual "" Completed =<< verdict c
    firstRun <- getModificationTime stamp

    step "second up skips it"
    again <- runUpCapturing (completed c)
    assertUp again
    assertEqual "the job was not skipped" (1, 0) (countOf isSkip again, countOf isEval again)
    assertEqual "a completed job was run again" firstRun =<< getModificationTime stamp

    step "a changed declaration runs it again"
    let changed = jobOf "true"
    rerun <- runUpCapturing (completed changed)
    assertUp rerun
    assertEqual "the job was not run again" 1 (countOf isEval rerun)
    assertEqual "the changed job was not installed again" 1 (length [() | UpDown.Eval act <- rerun, act.shorthand == "podman-quadlet-job"])
    secondRun <- getModificationTime stamp
    assertBool "the stamp did not move" (secondRun > firstRun)
    assertEqual "" Completed =<< verdict changed

    step "a failing command fails the pass"
    let failing = jobOf "exit 3"
    failed <- runUpCapturing (completed failing)
    -- systemd unloaded the unit when its last run ended; were the job not
    -- installed again, this would run the previous command and succeed
    assertEqual "the changed job was not installed again" 1 (length [() | UpDown.Eval act <- failed, act.shorthand == "podman-quadlet-job"])
    assertEqual "the failed run was not a failed up" [ran] [act.shorthand | UpDown.Failed act _ <- failed]
    said <- verdict failing
    case said of
        Failure why -> assertBool (Text.unpack why) ("exit status 3" `Text.isInfixOf` why)
        other -> assertFailure ("a failed job reads as " <> show other)
    assertEqual "a failed run moved the stamp" secondRun =<< getModificationTime stamp

    step "also once systemd has forgotten the failure"
    void (readProcessWithExitCode "systemctl" ["--user", "reset-failed", Text.unpack (Quadlet.serviceTarget c)] "")
    forgotten <- verdict failing
    case forgotten of
        Failure _ -> pure ()
        other -> assertFailure ("the failure was forgotten with systemd's record of it: " <> show other)

    step "a run that succeeds clears it"
    fixed <- runUpCapturing (completed c)
    assertUp fixed
    assertEqual "" Completed =<< verdict c

    step "down"
    down <- runDownCapturing (completed c)
    assertBool ("down failed: " <> show (failures down)) (null (failures down))
    assertBool "the stamp is still there" . not =<< doesFileExist stamp
    assertBool "the quadlet file is still there" . not =<< doesFileExist (Quadlet.quadletPath c)
    assertEqual "" "not-found" =<< showProperty c "LoadState"

-------------------------------------------------------------------------------

node :: Quadlet.Container -> Op
node = Quadlet.quadletContainer silent ignoreTrack ignoreTrack

assertUp :: [UpDown.Report Extension] -> IO ()
assertUp reports = do
    assertBool ("up failed: " <> show (failures reports)) (null (failures reports))
    assertEqual "nothing was blocked" 0 (length [() | UpDown.Blocked _ <- reports])

failures :: [UpDown.Report Extension] -> [String]
failures reports = [Text.unpack act.shorthand <> ": " <> show e | UpDown.Failed act e <- reports]

-- | How many reports about the quadlet node itself match.
count :: (UpDown.Report Extension -> Maybe (Act Extension)) -> [UpDown.Report Extension] -> Int
count which reports = length [() | Just act <- map which reports, act.shorthand == "podman-quadlet"]

isEval, isSkip :: UpDown.Report Extension -> Maybe (Act Extension)
isEval (UpDown.Eval act) = Just act
isEval _ = Nothing
isSkip (UpDown.Skip act) = Just act
isSkip _ = Nothing

-------------------------------------------------------------------------------

showProperty :: Quadlet.Container -> String -> IO Text
showProperty c property = do
    (_, out, _) <-
        readProcessWithExitCode
            "systemctl"
            ["--user", "show", Text.unpack (Quadlet.serviceTarget c), "--property=" <> property, "--value"]
            ""
    pure (Text.strip (Text.pack out))

podmanInspect :: Quadlet.Container -> String -> IO (Maybe Text)
podmanInspect c format = do
    (code, out, _) <-
        readProcessWithExitCode
            "podman"
            ["container", "inspect", "--format", format, Text.unpack (Podman.getContainerName c.containerName)]
            ""
    pure $ case code of
        ExitSuccess -> Just (Text.strip (Text.pack out))
        ExitFailure _ -> Nothing

containerRunning :: Quadlet.Container -> IO Bool
containerRunning c = (== Just "true") <$> podmanInspect c "{{.State.Running}}"

containerId :: Quadlet.Container -> IO Text
containerId c = maybe (assertFailure "no container to inspect") pure =<< podmanInspect c "{{.Id}}"

containerImage :: Quadlet.Container -> IO Text
containerImage c = maybe (assertFailure "no container to inspect") pure =<< podmanInspect c "{{.ImageName}}"

containerEnv :: Quadlet.Container -> IO Text
containerEnv c = do
    (code, out, err) <-
        readProcessWithExitCode
            "podman"
            ["exec", Text.unpack (Podman.getContainerName c.containerName), "printenv", "SALMON_QUADLET_TEST"]
            ""
    case code of
        ExitSuccess -> pure (Text.strip (Text.pack out))
        ExitFailure _ -> assertFailure ("printenv in the container failed: " <> err)

-------------------------------------------------------------------------------

userGenerator :: FilePath
userGenerator = "/usr/lib/systemd/user-generators/podman-user-generator"

{- | Runs the action with the user's quadlet directory, or skips loudly when
something it needs is missing. Creates the directory (and its parents under
@~\/.config@) if it is not there, and removes what it created again if it is
empty afterwards, along with the key of the watched files' digest if a case
made it.
-}
withUserQuadlets :: (FilePath -> IO ()) -> IO ()
withUserQuadlets act = do
    uid <- getRealUserID
    podman <- findExecutable "podman"
    generator <- doesFileExist userGenerator
    (managerCode, managerOut, _) <- readProcessWithExitCode "systemctl" ["--user", "is-system-running"] ""
    let manager = managerCode == ExitSuccess || Text.strip (Text.pack managerOut) `elem` ["degraded", "starting"]
    (imageCode, _, _) <-
        if podman == Nothing
            then pure (ExitFailure 1, "", "")
            else readProcessWithExitCode "podman" ["image", "exists", Text.unpack image] ""
    case () of
        _
            | uid == 0 -> skip "running as root; this test is the user-scope one"
            | podman == Nothing -> skip "`podman` not found on PATH"
            | not generator -> skip ("no " <> userGenerator <> "; podman's quadlet user generator is not installed")
            | not manager -> skip "no `systemd --user` manager is answering"
            | imageCode /= ExitSuccess -> skip (Text.unpack image <> " is not on this machine (`podman pull " <> Text.unpack image <> "` to run this test)")
            | otherwise -> do
                dir <- getXdgDirectory XdgConfig ("containers" </> "systemd")
                created <- missingAncestors dir
                createDirectoryIfMissing True dir
                -- the key of the watched files' digest is the directory's and
                -- outlives every `down`: one made here is removed here
                let key = Quadlet.watchKeyPath (Quadlet.container (Podman.ContainerName "any") image){Quadlet.containerUnitDir = dir}
                keyed <- doesFileExist key
                act dir `finally` do
                    unless keyed $ do
                        made <- doesFileExist key
                        when made (removeFile key)
                    mapM_ removeIfEmpty created
  where
    skip why = hPutStrLn stderr ("SKIPPED: " <> why <> "; the quadlet node was not run against a real systemd")

    -- the directories `createDirectoryIfMissing` is about to make, deepest first
    missingAncestors :: FilePath -> IO [FilePath]
    missingAncestors dir = do
        exists <- doesDirectoryExist dir
        if exists
            then pure []
            else do
                parents <- missingAncestors (parentOf dir)
                pure (dir : parents)

    parentOf :: FilePath -> FilePath
    parentOf = reverse . drop 1 . dropWhile (/= '/') . reverse

    removeIfEmpty :: FilePath -> IO ()
    removeIfEmpty dir = do
        exists <- doesDirectoryExist dir
        when exists $ do
            entries <- listDirectory dir
            when (null entries) (removeDirectory dir)

{- | A container under a name nothing real has, and a cleanup that does not
trust the node's own @down@: stop the unit, remove the file, reload, remove the
container.
-}
withFreshContainer :: FilePath -> (Quadlet.Container -> IO a) -> IO a
withFreshContainer unitDir act = bracket fresh cleanup act
  where
    fresh = do
        -- wall clock, CPU time and pid: two runs, or two cases of one run,
        -- never share a name
        now <- getPOSIXTime
        cpu <- getCPUTime
        pid <- getProcessID
        let suffix = showHex (floor (now * 1000000) :: Integer) "" <> "-" <> showHex (cpu `mod` 0xffffff) "" <> "-" <> show pid
            name = Podman.ContainerName (Text.pack ("salmon-quadlet-test-" <> suffix))
        pure
            (Quadlet.container name image)
                { Quadlet.containerScope = Systemd.User
                , Quadlet.containerUnitDir = unitDir
                , Quadlet.containerWantedBy = Nothing
                , Quadlet.containerRestart = Quadlet.RestartNo
                }
    cleanup c = do
        let name = Text.unpack (Podman.getContainerName c.containerName)
            quiet cmd args = void (readProcessWithExitCode cmd args "")
        quiet "systemctl" ["--user", "stop", Text.unpack (Quadlet.serviceTarget c)]
        present <- doesFileExist (Quadlet.quadletPath c)
        when present (removeFile (Quadlet.quadletPath c))
        quiet "systemctl" ["--user", "daemon-reload"]
        quiet "systemctl" ["--user", "reset-failed", Text.unpack (Quadlet.serviceTarget c)]
        quiet "podman" ["rm", "--force", "--ignore", name]
