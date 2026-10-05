{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Podman.Quadlet" and for the
instance login in "Salmon.Builtin.Nodes.Gcp.ArtifactRegistry": what is
rendered, what the check concludes, and what a re-declaration can see.

One group is not pure: where podman's own quadlet generator is installed, the
rendered file is handed to it in dry-run mode, which is the only local
evidence that the keys written are keys this podman accepts. It reads a
scratch directory and writes nothing; it is skipped loudly without the
generator. Nothing here starts a container, talks to systemd, or reaches a
registry or a metadata server; "Test.QuadletUserSpec" runs the node against
this user's own systemd, and the system scope, the pull from a registry and
the instance login are Layer 3 claims not made in either.
-}
module Test.QuadletSpec (tests) where

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Map.Strict as Map
import Data.Foldable (toList)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isInfixOf, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Gcp.ArtifactRegistry as ArtifactRegistry
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)
import Test.Harness (withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Podman.Quadlet"
        [ testGroup "rendering" renderTests
        , testGroup "watched files" watchedTests
        , testGroup "refusals" problemTests
        , testGroup "check" checkTests
        , testGroup "node" nodeTests
        , testGroup "generator" generatorTests
        , testGroup "instance login" loginTests
        ]

app :: Quadlet.Container
app =
    (Quadlet.container (Podman.ContainerName "app") "europe-west1-docker.pkg.dev/acme/repo/app:v3")
        { Quadlet.containerDescription = "the app"
        , Quadlet.containerAfter = ["network-online.target"]
        , Quadlet.containerEnvFile = Just "/etc/app/env"
        , Quadlet.containerPorts = [Podman.PortMapping "8080" "80" Podman.TCPPort]
        , Quadlet.containerVolumes = [Podman.VolumeMount "/srv/app" "/data" Podman.ReadOnly]
        , Quadlet.containerAuthFile = Just (Podman.AuthFile "/etc/app/auth.json")
        , Quadlet.containerStartTimeout = Just 300
        }

renderTests :: [TestTree]
renderTests =
    [ testCase "a full container, key by key" $
        assertEqual
            ""
            ( Text.unlines
                [ "[Unit]"
                , "Description=the app"
                , "After=network-online.target"
                , ""
                , "[Container]"
                , "ContainerName=app"
                , "Image=europe-west1-docker.pkg.dev/acme/repo/app:v3"
                , "EnvironmentFile=/etc/app/env"
                , "PublishPort=8080:80/tcp"
                , "Volume=/srv/app:/data:ro"
                , "PodmanArgs=--authfile=/etc/app/auth.json"
                , ""
                , "[Service]"
                , "Restart=on-failure"
                , "TimeoutStartSec=300"
                , ""
                , "[Install]"
                , "WantedBy=multi-user.target"
                ]
            )
            (Quadlet.renderContainer app)
    , testCase "the starting point renders only what it must" $
        assertEqual
            ""
            ( Text.unlines
                [ "[Unit]"
                , "Description=container web (salmon)"
                , ""
                , "[Container]"
                , "ContainerName=web"
                , "Image=docker.io/library/nginx:1.27"
                , ""
                , "[Service]"
                , "Restart=on-failure"
                , ""
                , "[Install]"
                , "WantedBy=multi-user.target"
                ]
            )
            (Quadlet.renderContainer (Quadlet.container (Podman.ContainerName "web") "docker.io/library/nginx:1.27"))
    , testCase "no WantedBy, no [Install] section" $
        assertBool "" $
            not ("[Install]" `Text.isInfixOf` Quadlet.renderContainer app{Quadlet.containerWantedBy = Nothing})
    , testCase "restart policies and udp ports are spelled as systemd and podman spell them" $ do
        let rendered =
                Quadlet.renderContainer
                    app
                        { Quadlet.containerRestart = Quadlet.RestartAlways
                        , Quadlet.containerPorts = [Podman.PortMapping "5353" "53" Podman.UDPPort]
                        }
        assertBool "" ("Restart=always\n" `Text.isInfixOf` rendered)
        assertBool "" ("PublishPort=5353:53/udp\n" `Text.isInfixOf` rendered)
    , testCase "the file and the unit are named after the container" $ do
        assertEqual "" "/etc/containers/systemd/app.container" (Quadlet.quadletPath app)
        assertEqual "" "app.service" (Quadlet.serviceTarget app)
    , testCase "a new image reference is a different file" $
        assertBool "" $
            Quadlet.renderContainer app /= Quadlet.renderContainer app{Quadlet.containerImage = "europe-west1-docker.pkg.dev/acme/repo/app:v4"}
    ]

watchedTests :: [TestTree]
watchedTests =
    [ testCase "the env file is watched, before anything else named" $
        assertEqual "" ["/etc/app/env", "/etc/app/extra.conf"] (Quadlet.watchedFiles app{Quadlet.containerWatched = ["/etc/app/extra.conf"]})
    , testCase "a changed env file changes the quadlet, an unchanged one does not" $ withTempDir $ \dir -> do
        let env = dir </> "env"
            c = app{Quadlet.containerEnvFile = Just env}
        writeFile env "PORT=80\n"
        before <- Quadlet.renderContainerWatching c
        again <- Quadlet.renderContainerWatching c
        writeFile env "PORT=81\n"
        after <- Quadlet.renderContainerWatching c
        assertEqual "" before again
        assertBool "the quadlet did not change with the env file" (before /= after)
        assertBool "the fingerprint replaced the declaration" (Quadlet.renderContainer c `Text.isPrefixOf` after)
    , testCase "the env file's contents are not in the quadlet" $ withTempDir $ \dir -> do
        let env = dir </> "env"
        writeFile env "API_KEY=hunter2hunter2\n"
        rendered <- Quadlet.renderContainerWatching app{Quadlet.containerEnvFile = Just env}
        assertBool "" (not ("hunter2" `Text.isInfixOf` rendered))
    , testCase "nothing watched renders the declaration exactly" $ do
        let c = app{Quadlet.containerEnvFile = Nothing}
        rendered <- Quadlet.renderContainerWatching c
        assertEqual "" (Quadlet.renderContainer c) rendered
    , testCase "the written file is the declaration plus the label naming it" $ do
        let c = app{Quadlet.containerEnvFile = Nothing}
        fingerprint <- Quadlet.quadletFingerprint c
        written <- Quadlet.renderQuadlet c
        let label = "Label=" <> Quadlet.quadletLabel <> "=" <> fingerprint
        assertEqual "not exactly one label line" [label] (filter ("Label=" `Text.isPrefixOf`) (Text.lines written))
        assertEqual "the label is not the only difference" (Text.lines (Quadlet.renderContainer c)) (filter (/= label) (Text.lines written))
        assertBool "the label is not in the [Container] section" $
            "[Container]" `elem` takeWhile (/= label) (Text.lines written)
                && "[Service]" `notElem` takeWhile (/= label) (Text.lines written)
    , testCase "the fingerprint moves with the image and with the env file, and not otherwise" $ withTempDir $ \dir -> do
        let env = dir </> "env"
            c = app{Quadlet.containerEnvFile = Just env}
        writeFile env "PORT=80\n"
        before <- Quadlet.quadletFingerprint c
        again <- Quadlet.quadletFingerprint c
        moved <- Quadlet.quadletFingerprint c{Quadlet.containerImage = "europe-west1-docker.pkg.dev/acme/repo/app:v4"}
        writeFile env "PORT=81\n"
        after <- Quadlet.quadletFingerprint c
        assertEqual "" before again
        assertBool "a new image left the fingerprint alone" (before /= moved)
        assertBool "a new env file left the fingerprint alone" (before /= after)
    ]

problemTests :: [TestTree]
problemTests =
    [ testCase "a well-formed container has no problems" $
        assertEqual "" [] (Quadlet.containerProblems app)
    , testCase "a line break in the image is refused, not written as a second key" $
        assertBool "" (not (null (Quadlet.containerProblems app{Quadlet.containerImage = "img:v1\nPodmanArgs=--privileged"})))
    , testCase "an empty image or name is refused" $ do
        assertBool "" (not (null (Quadlet.containerProblems app{Quadlet.containerImage = " "})))
        assertBool "" (not (null (Quadlet.containerProblems app{Quadlet.containerName = Podman.ContainerName ""})))
    , testCase "a name that is a path is refused" $
        assertBool "" (not (null (Quadlet.containerProblems app{Quadlet.containerName = Podman.ContainerName "../x"})))
    , testCase "a stamp on a container that stays up is refused" $
        -- its ExecStartPost would run when the container is ready, not when
        -- it finished, and there is no run to record
        assertEqual
            ""
            ["a stamp records a run that finished, and this container is not a job"]
            (Quadlet.containerProblems app{Quadlet.containerStamp = Just "/var/lib/app/stamp"})
    ]

checkTests :: [TestTree]
checkTests =
    [ testCase "a running generated unit is satisfied" $
        assertEqual "" Success (Quadlet.interpretShow ["ActiveState=active", "UnitFileState=generated", "NeedDaemonReload=no"])
    , testCase "the same lines are not satisfied for an authored unit" $
        assertBool "" (Systemd.interpretShow ["ActiveState=active", "UnitFileState=generated", "NeedDaemonReload=no"] /= Success)
    , testCase "a changed quadlet needs bringing up, running or not" $
        assertBool "" (isFailure (Quadlet.interpretShow ["ActiveState=active", "UnitFileState=generated", "NeedDaemonReload=yes"]))
    , testCase "a quadlet systemd has not generated yet needs bringing up" $
        -- what `systemctl show` prints between the file being written and the reload
        assertBool "" (isFailure (Quadlet.interpretShow ["ActiveState=inactive", "UnitFileState=", "NeedDaemonReload=no"]))
    , testCase "a stopped or failed container needs bringing up" $ do
        assertBool "" (isFailure (Quadlet.interpretShow ["ActiveState=inactive", "UnitFileState=generated", "NeedDaemonReload=no"]))
        assertBool "" (isFailure (Quadlet.interpretShow ["ActiveState=failed", "UnitFileState=generated", "NeedDaemonReload=no"]))
    , testCase "a container still starting (pulling) is Unknown, not Failure" $
        assertEqual "" Unknown (Quadlet.interpretShow ["ActiveState=activating", "UnitFileState=generated", "NeedDaemonReload=no"])
    , testCase "a container started from the declared quadlet is satisfied" $
        assertEqual "" Success (Quadlet.interpretRunning "abc123" "abc123\n")
    , testCase "a container started from another quadlet needs bringing up" $
        -- the unit reads active, generated and reloaded: something else ran
        -- the daemon-reload, and nothing restarted this service
        assertBool "" (isFailure (Quadlet.interpretRunning "abc123" "def456\n"))
    , testCase "a container started before its env file was rotated needs bringing up, whatever systemd remembers" $ withTempDir $ \dir -> do
        -- the image and the declaration are the ones it was started with;
        -- only the env file moved, and a daemon-reload run for another
        -- quadlet has already cleared NeedDaemonReload
        let env = dir </> "env"
            c = app{Quadlet.containerEnvFile = Just env}
        writeFile env "TOKEN=one\n"
        started <- Quadlet.quadletFingerprint c
        assertEqual "" Success (Quadlet.interpretRunning started (started <> "\n"))
        writeFile env "TOKEN=two\n"
        declared <- Quadlet.quadletFingerprint c
        assertBool "" (isFailure (Quadlet.interpretRunning declared (started <> "\n")))
        written <- Quadlet.renderQuadlet c
        assertBool "the rewritten file does not label the container with the new fingerprint" $
            ("Label=" <> Quadlet.quadletLabel <> "=" <> declared) `elem` Text.lines written
    , testCase "a container that does not say what it was started from needs bringing up" $ do
        assertBool "" (isFailure (Quadlet.interpretRunning "abc123" "\n"))
        assertBool "" (isFailure (Quadlet.interpretRunning "abc123" "<no value>\n"))
    ]
  where
    isFailure (Failure _) = True
    isFailure _ = False

nodeTests :: [TestTree]
nodeTests =
    [ testCase "the node is keyed on the unit, not on the image" $ do
        let a = refOf (node app)
            b = refOf (node app{Quadlet.containerImage = "other:v1"})
        assertEqual "moving the image declared a second node" a b
        assertBool "two containers are one node" (a /= refOf (node app{Quadlet.containerName = Podman.ContainerName "other"}))
    , testCase "a new image is visible in the node's description" $
        -- so `run serve` sees the re-declaration as a change
        assertBool "" (notesOf (node app) /= notesOf (node app{Quadlet.containerImage = "europe-west1-docker.pkg.dev/acme/repo/app:v4"}))
    , testCase "the same declaration describes itself the same way" $
        assertEqual "" (notesOf (node app)) (notesOf (node app))
    , testCase "a declaration that sets no stamp is described, keyed and rendered as it was before there was one" $ do
        -- the hash is that of the text "a full container, key by key" pins,
        -- worked out outside this code (sha256, base64url, 12 characters): a
        -- long-running container whose file or description moved would be
        -- restarted by the pass after an upgrade
        assertEqual "" Nothing app.containerStamp
        assertEqual
            ""
            (Just ["image: europe-west1-docker.pkg.dev/acme/repo/app:v3", "quadlet: aTe0H65oUH_B"])
            (notesOf (node app))
        assertEqual "" (Just "runs europe-west1-docker.pkg.dev/acme/repo/app:v3 as app.service") (helpOf (node app))
        assertEqual "" (Just (mkRef "systemd-unit" ("app.service" :: Text))) (refOf (node app))
        assertBool "" (not ("ExecStart" `Text.isInfixOf` Quadlet.renderContainer app))
    , testCase "two quadlets in one directory share it without a conflict" $ do
        let other = app{Quadlet.containerName = Podman.ContainerName "other"}
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps (op "both" (deps [node app, node other]) id))
        assertEqual "the shared directory was described two ways" 0 (length (Dag.dagConflicts dag))
    , testCase "the shared directory carries nothing of one container's" $ do
        let dag = Dag.foldDag Dag.sameRepresentative (evalDeps (node app))
            dirs = [act | act <- Map.elems (Dag.dagNodes dag), act.shorthand == "podman-quadlet-dir"]
        assertEqual "not exactly one directory node" 1 (length dirs)
        assertBool "the directory carries the container's notes" $
            not (any (Text.isPrefixOf "quadlet:") (concatMap (\act -> act.extension.notes) dirs))
    , testCase "the image is asked for, and pulled with the container's credentials" $ do
        assertEqual "" ["image", "exists", "europe-west1-docker.pkg.dev/acme/repo/app:v3"] (Quadlet.imagePresentArgs app)
        assertEqual
            ""
            ["pull", "--quiet", "--authfile=/etc/app/auth.json", "europe-west1-docker.pkg.dev/acme/repo/app:v3"]
            (Quadlet.imagePullArgs app)
        assertEqual
            "a public image is pulled with no auth file"
            ["pull", "--quiet", "docker.io/library/nginx:alpine"]
            (Quadlet.imagePullArgs (Quadlet.container (Podman.ContainerName "web") "docker.io/library/nginx:alpine"))
    , testCase "an image on the machine is a skip, a missing one is pulled, a podman that cannot say is unknown" $ do
        assertEqual "" Success (Quadlet.interpretImagePresent "app:v3" ExitSuccess)
        assertEqual "" (Failure "the image app:v3 is not on the machine") (Quadlet.interpretImagePresent "app:v3" (ExitFailure 1))
        assertEqual "" Unknown (Quadlet.interpretImagePresent "app:v3" (ExitFailure 125))
    , testCase "the quadlet file stands on the image, so a failed pull blocks the rewrite and the restart" $ do
        let dag = Dag.foldDag Dag.sameRepresentative (evalDeps (node app))
            refsOf short = [r | (r, act) <- Map.toList (Dag.dagNodes dag), act.shorthand == short]
        image <- one "image node" (refsOf "podman-quadlet-image")
        unit <- one "quadlet node" (refsOf "podman-quadlet")
        let dependants = Map.findWithDefault mempty image (Dag.dagDependants dag)
            files = [r | r <- toList dependants, r /= unit]
        file <- one "dependant of the image other than the unit" files
        assertBool "the unit does not stand on the file that stands on the image" $
            file `elem` toList (Map.findWithDefault mempty unit (Dag.dagDependencies dag))
    , testCase "what the caller's track declares comes before the pull" $ do
        let login = op "the-login" nodeps (\ext -> ext{ref = mkRef "test-login" ("x" :: Text)})
            tracked = Quadlet.quadletContainer silent ignoreTrack (Track (const login)) app
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps tracked)
            refsOf short = [r | (r, act) <- Map.toList (Dag.dagNodes dag), act.shorthand == short]
        image <- one "image node" (refsOf "podman-quadlet-image")
        loginRef <- one "login node" (refsOf "the-login")
        assertBool "the pull does not wait for the login" $
            loginRef `elem` toList (Map.findWithDefault mempty image (Dag.dagDependencies dag))
    , testCase "what the caller's track declares comes before the file, so a failed one leaves the old quadlet" $ do
        let migration = op "the-migration" nodeps (\ext -> ext{ref = mkRef "test-migration" ("x" :: Text)})
            tracked = Quadlet.quadletContainer silent ignoreTrack (Track (const migration)) app
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps tracked)
            refsOf short = [r | (r, act) <- Map.toList (Dag.dagNodes dag), act.shorthand == short]
            dependenciesOf r = toList (Map.findWithDefault mempty r (Dag.dagDependencies dag))
        migrationRef <- one "migration node" (refsOf "the-migration")
        file <- one "file node" (refsOf "file-contents")
        unit <- one "quadlet node" (refsOf "podman-quadlet")
        assertBool "the file does not wait for the track" (migrationRef `elem` dependenciesOf file)
        assertBool "the unit stopped standing on the track" (migrationRef `elem` dependenciesOf unit)
        assertEqual "the track made a cycle" [] (toList (Dag.stuck (\d r -> toList (Map.findWithDefault mempty r (Dag.dagDependencies d))) dag))
    , testCase "a job's file waits for the caller's track too" $ do
        let migration = op "the-migration" nodeps (\ext -> ext{ref = mkRef "test-migration" ("x" :: Text)})
            job = Quadlet.quadletJob silent ignoreTrack (Track (const migration)) (Quadlet.containerJob (Podman.ContainerName "job") "app:v3" ["run"])
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps job)
            refsOf short = [r | (r, act) <- Map.toList (Dag.dagNodes dag), act.shorthand == short]
        migrationRef <- one "migration node" (refsOf "the-migration")
        file <- one "file node" (refsOf "file-contents")
        assertBool "the file does not wait for the track" $
            migrationRef `elem` toList (Map.findWithDefault mempty file (Dag.dagDependencies dag))
    , testCase "the track changes nothing about how the file and the unit are described" $ do
        let migration = op "the-migration" nodeps (\ext -> ext{ref = mkRef "test-migration" ("x" :: Text)})
            described o =
                let dag = Dag.foldDag Dag.sameRepresentative (evalDeps o)
                 in sort
                        [ (show r, act.extension.help, act.extension.notes)
                        | (r, act) <- Map.toList (Dag.dagNodes dag)
                        , act.shorthand /= "the-migration"
                        ]
        assertEqual "" (described (node app)) (described (Quadlet.quadletContainer silent ignoreTrack (Track (const migration)) app))
    , testCase "two containers of one image share the pull without a conflict" $ do
        let other = app{Quadlet.containerName = Podman.ContainerName "other", Quadlet.containerDescription = "another"}
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps (op "both" (deps [node app, node other]) id))
        assertEqual "" 1 (length [() | act <- Map.elems (Dag.dagNodes dag), act.shorthand == "podman-quadlet-image"])
        assertEqual "the shared image was described two ways" 0 (length (Dag.dagConflicts dag))
    , testCase "a job's file does not stand on a pull" $ do
        let job = Quadlet.quadletJob silent ignoreTrack ignoreTrack (Quadlet.containerJob (Podman.ContainerName "job") "app:v3" ["run"])
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps job)
        assertEqual "" 0 (length [() | act <- Map.elems (Dag.dagNodes dag), act.shorthand == "podman-quadlet-image"])
    ]
  where
    one :: String -> [a] -> IO a
    one _ [x] = pure x
    one what xs = assertFailure ("not exactly one " <> what <> ": " <> show (length xs))
    node = Quadlet.quadletContainer silent ignoreTrack ignoreTrack
    refOf o = fmap (\act -> act.extension.ref) (opAct o)
    notesOf o = fmap (\act -> act.extension.notes) (opAct o)
    helpOf o = fmap (\act -> act.extension.help) (opAct o)

generatorPath :: FilePath
generatorPath = "/usr/libexec/podman/quadlet"

generatorTests :: [TestTree]
generatorTests =
    [ testCase "podman's own generator accepts the rendered file" $ withGenerator $ withTempDir $ \dir -> do
        let envFile = dir </> "env"
            c = app{Quadlet.containerUnitDir = dir, Quadlet.containerEnvFile = Just envFile}
        writeFile envFile "PORT=80\n"
        Text.writeFile (Quadlet.quadletPath c) =<< Quadlet.renderQuadlet c
        fingerprint <- Quadlet.quadletFingerprint c
        environment <- getEnvironment
        (code, out, err) <-
            readCreateProcessWithExitCode
                (proc generatorPath ["--user", "--dryrun"]){env = Just (("QUADLET_UNIT_DIRS", dir) : environment)}
                ""
        let said = out <> err
        case code of
            ExitSuccess -> pure ()
            ExitFailure n -> assertFailure ("the generator exited " <> show n <> ": " <> said)
        assertBool said ("---app.service---" `isInfixOf` said)
        assertBool said (not ("unsupported key" `isInfixOf` said))
        assertBool said ("--authfile=/etc/app/auth.json" `isInfixOf` said)
        assertBool said (("--env-file " <> envFile) `isInfixOf` said)
        assertBool said ("--publish 8080:80/tcp" `isInfixOf` said)
        assertBool said (("--label " <> Text.unpack (Quadlet.quadletLabel <> "=" <> fingerprint)) `isInfixOf` said)
        assertBool said ("europe-west1-docker.pkg.dev/acme/repo/app:v3" `isInfixOf` said)
        assertBool said (("SourcePath=" <> Quadlet.quadletPath c) `isInfixOf` said)
    ]
  where
    withGenerator :: IO () -> IO ()
    withGenerator act = do
        present <- doesFileExist generatorPath
        if present
            then act
            else hPutStrLn stderr ("SKIPPED: no quadlet generator at " <> generatorPath <> "; the rendered file was not checked against podman")

loginTests :: [TestTree]
loginTests =
    [ testCase "the registry is the region's docker host" $
        assertEqual "" (Podman.Registry "europe-west1-docker.pkg.dev") (ArtifactRegistry.dockerRegistry (Core.Region "europe-west1"))
    , testCase "the metadata server's answer is read for the token and its lifetime" $
        case ArtifactRegistry.parseInstanceToken (LC8.pack "{\"access_token\":\"ya29.abc\",\"expires_in\":3599,\"token_type\":\"Bearer\"}") of
            Right token -> do
                assertEqual "" ("ya29.abc" :: Text) token.tokenValue
                assertEqual "" 3599 token.tokenLifetime
            Left err -> assertFailure err
    , testCase "an answer with no token, or an empty one, is not a token" $ do
        assertBool "" (isLeft' (ArtifactRegistry.parseInstanceToken (LC8.pack "{\"error\":\"nope\"}")))
        assertBool "" (isLeft' (ArtifactRegistry.parseInstanceToken (LC8.pack "{\"access_token\":\"\",\"expires_in\":10}")))
        assertBool "" (isLeft' (ArtifactRegistry.parseInstanceToken (LC8.pack "<html>")))
    , testCase "no stamp is not logged in" $
        assertBool "" (isFailure (ArtifactRegistry.interpretTokenStamp now Nothing))
    , testCase "a token with life left is left alone" $
        assertEqual "" Success (ArtifactRegistry.interpretTokenStamp now (Just "1700003000\n"))
    , testCase "a token inside the margin, or past it, is renewed" $ do
        assertBool "" (isFailure (ArtifactRegistry.interpretTokenStamp now (Just "1700000060")))
        assertBool "" (isFailure (ArtifactRegistry.interpretTokenStamp now (Just "1699990000")))
        assertBool "" (isFailure (ArtifactRegistry.interpretTokenStamp (addUTCTime 2900 now) (Just "1700003000")))
    , testCase "a token the metadata server would not yet replace outlives the margin" $
        -- the server hands out a new token once the cached one has under five
        -- minutes left, so a login never yields one the check refuses at once
        assertBool "" (ArtifactRegistry.refreshMargin < 300)
    , testCase "an unreadable stamp is renewed, not trusted" $
        assertBool "" (isFailure (ArtifactRegistry.interpretTokenStamp now (Just "soon")))
    , testCase "the stamp sits beside the auth file" $
        assertEqual "" "/etc/app/auth.json.expires" (ArtifactRegistry.tokenStampPath (Podman.AuthFile "/etc/app/auth.json"))
    , testCase "the stamp's check and write are the login node's alone" $
        -- the enclosing directory is a predecessor: carrying them, it renewed
        -- the stamp without logging in, and the login was then skipped
        withTempDir $ \tmp -> do
            asked <- newIORef (0 :: Int)
            let authfile = Podman.AuthFile (tmp </> "auth" </> "auth.json")
                stamp = ArtifactRegistry.tokenStampPath authfile
                token = modifyIORef asked (+ 1) >> pure (ArtifactRegistry.InstanceToken "ya29.abc" 3599)
                login = ArtifactRegistry.instanceLoginWith token silent ignoreTrack authfile (Core.Region "europe-west1")
                dag = Dag.foldDag Dag.sameRepresentative (evalDeps login)
                acts = Map.elems (Dag.dagNodes dag)
            assertEqual "the graph is not the login and its directory" ["directory", "podman-login"] (sort (map (\act -> act.shorthand) acts))
            dirAct <- case [act | act <- acts, act.shorthand == "directory"] of
                [act] -> pure act
                _ -> assertFailure "not exactly one directory node"
            -- a login from hours ago: credentials, and a stamp long expired
            dirAct.extension.up
            writeFile (Podman.getAuthFile authfile) "{\"auths\":{}}"
            writeFile stamp "1700000000\n"
            dirCheck <- dirAct.extension.check
            assertEqual "the directory answers for the stamp" Immaterial dirCheck
            dirAct.extension.up
            after <- readFile stamp
            assertEqual "the directory's up renewed the stamp" "1700000000\n" after
            readIORef asked >>= assertEqual "the directory's up asked for a token" 0
            loginCheck <- maybe (assertFailure "no login node") (\act -> act.extension.check) (opAct login)
            assertBool "an expired stamp does not ask for a login" (isFailure loginCheck)
    ]
  where
    now = posixSecondsToUTCTime 1700000000
    isFailure (Failure _) = True
    isFailure _ = False
    isLeft' :: Either String ArtifactRegistry.InstanceToken -> Bool
    isLeft' = either (const True) (const False)
