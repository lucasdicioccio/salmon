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
unchanged one does not; @down@ stops the service, removes the file and the
generated unit goes with it.

Skipped loudly when @podman@, @podman-user-generator@, a running
@systemd --user@ or the image is missing. The image is one that needs no
registry login, and is only ever used if it is already on the machine; the
second reference the image-change case needs is a local tag of it, made and
removed here. Every container gets a random name, and a bracket removes its
quadlet file, stops its unit, removes its container and the tag, and reloads,
whatever the case did.
-}
module Test.QuadletUserSpec (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, unless, void, when)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Numeric (showHex)
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getXdgDirectory, listDirectory, removeDirectory, removeFile)
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

import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Actions (Act (..))
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
empty afterwards.
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
                act dir `finally` mapM_ removeIfEmpty created
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
