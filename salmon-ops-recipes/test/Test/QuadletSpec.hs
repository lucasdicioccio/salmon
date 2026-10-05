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
import Control.Exception (SomeException, try)
import qualified Data.ByteString as ByteString
import Data.Bits ((.&.))
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef, newIORef, readIORef)
import Data.List (isInfixOf, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import System.Posix.User (getEffectiveUserID)
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
        , testGroup "watched files' key" keyTests
        , testGroup "refusals" problemTests
        , testGroup "check" checkTests
        , testGroup "node" nodeTests
        , testGroup "bind volumes" bindTests
        , testGroup "readiness" readinessTests
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
            c = app{Quadlet.containerUnitDir = dir, Quadlet.containerEnvFile = Just env}
        Quadlet.ensureWatchKey c
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
            c = app{Quadlet.containerUnitDir = dir, Quadlet.containerEnvFile = Just env}
        Quadlet.ensureWatchKey c
        writeFile env "API_KEY=hunter2hunter2\n"
        rendered <- Quadlet.renderContainerWatching c
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
            c = app{Quadlet.containerUnitDir = dir, Quadlet.containerEnvFile = Just env}
        Quadlet.ensureWatchKey c
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

{- | The key the watched files' digest is made under: what is in the
quadlet without it, and the key file itself in a scratch directory.
-}
keyTests :: [TestTree]
keyTests =
    [ testCase "the quadlet carries no plain hash of the env file, and no fingerprint made from one" $ withTempDir $ \dir -> do
        let env = dir </> "env"
            c = app{Quadlet.containerUnitDir = dir, Quadlet.containerEnvFile = Just env}
        Quadlet.ensureWatchKey c
        writeFile env "PASSWORD=hunter2\n"
        -- what anybody who can read the env file's path and guess its
        -- contents can compute: the line as it was written before the key
        plain <- Systemd.withWatchedFingerprint [env] ""
        unkeyed <- Quadlet.unkeyedFingerprint c
        written <- Quadlet.renderQuadlet c
        assertBool "the file has no watched-files line" (any ("# salmon-watches: hmac-sha256:" `Text.isPrefixOf`) (Text.lines written))
        assertBool "the plain hash is in the file" (not (Text.strip plain `Text.isInfixOf` written))
        assertBool "the plain hash's digest is in the file" (not (Text.drop (Text.length "# salmon-watches: ") (Text.strip plain) `Text.isInfixOf` written))
        assertBool "the label is the fingerprint from before the key" (not (unkeyed `Text.isInfixOf` written))
    , testCase "the same files under another key are another line, under the same key the same" $ do
        let frames = ["/etc/app/env:17:", "PASSWORD=hunter2\n"]
        assertEqual "" (Quadlet.keyedWatchLine "key one" frames) (Quadlet.keyedWatchLine "key one" frames)
        assertBool "" (Quadlet.keyedWatchLine "key one" frames /= Quadlet.keyedWatchLine "key two" frames)
        assertBool "" (Quadlet.keyedWatchLine "key one" frames /= Quadlet.keyedWatchLine "key one" ["/etc/app/env:17:", "PASSWORD=hunter3\n"])
        -- HMAC-SHA256, worked out outside this code (python's hmac): a
        -- changed construction is a restart of every watching quadlet
        assertEqual
            ""
            "# salmon-watches: hmac-sha256:1gecq1WMRz2R6OrB2pYA4e6aQiTPtJ9JB4aWUWugcz0=\n"
            (Quadlet.keyedWatchLine "key one" frames)
    , testCase "two machines' quadlets for one env file do not match" $ withTempDir $ \dir -> do
        let env = dir </> "env"
            on name = app{Quadlet.containerUnitDir = dir </> name, Quadlet.containerEnvFile = Just env}
        writeFile env "PASSWORD=hunter2\n"
        createDirectory (dir </> "one")
        createDirectory (dir </> "two")
        Quadlet.ensureWatchKey (on "one")
        Quadlet.ensureWatchKey (on "two")
        one <- Quadlet.renderContainerWatching (on "one")
        two <- Quadlet.renderContainerWatching (on "two")
        assertBool "" (one /= two)
    , testCase "without the key nothing is rendered, and there is no falling back to a plain hash" $ withTempDir $ \dir -> do
        let c = app{Quadlet.containerUnitDir = dir, Quadlet.containerEnvFile = Just (dir </> "env")}
        rendered <- try (Quadlet.renderQuadlet c)
        case rendered of
            Left (Quadlet.WatchKeyUnusable path _) -> assertEqual "" (dir </> ".salmon-watch.key") path
            Right text -> assertFailure ("rendered without a key: " <> Text.unpack text)
        writeFile (Quadlet.watchKeyPath c) "\n"
        empty <- try (Quadlet.renderQuadlet c)
        case empty of
            Left (Quadlet.WatchKeyUnusable _ _) -> pure ()
            Right text -> assertFailure ("rendered under an empty key: " <> Text.unpack text)
    , testCase "a quadlet that watches nothing reads no key" $ withTempDir $ \dir -> do
        let c = app{Quadlet.containerUnitDir = dir, Quadlet.containerEnvFile = Nothing}
        written <- Quadlet.renderQuadlet c
        assertBool "" (not ("salmon-watches" `Text.isInfixOf` written))
        assertEqual "" [] =<< listDirectory dir
    , testCase "the key is made once, owner-only, and never replaced" $ withTempDir $ \dir -> do
        let c = app{Quadlet.containerUnitDir = dir}
            path = Quadlet.watchKeyPath c
        Quadlet.ensureWatchKey c
        first <- ByteString.readFile path
        assertBool "a short key" (ByteString.length first >= 32)
        mode <- fileMode <$> getFileStatus path
        assertEqual "the key is readable by others" 0o600 (mode .&. 0o777)
        assertEqual "something was left beside the key" [".salmon-watch.key"] =<< listDirectory dir
        Quadlet.ensureWatchKey c
        assertEqual "the key was replaced" first =<< ByteString.readFile path
        setFileMode path 0o644
        Quadlet.ensureWatchKey c
        again <- fileMode <$> getFileStatus path
        assertEqual "a key opened to others was not closed" 0o600 (again .&. 0o777)
        assertEqual "closing the key replaced it" first =<< ByteString.readFile path
    , testCase "the key node: missing, open to others, empty, as it should be" $ do
        let isFailure r = case r of Failure _ -> True; _ -> False
        assertBool "" (isFailure (Quadlet.interpretWatchKey "/k" Nothing))
        assertBool "" (isFailure (Quadlet.interpretWatchKey "/k" (Just (0o100644, 45))))
        assertBool "" (isFailure (Quadlet.interpretWatchKey "/k" (Just (0o100640, 45))))
        assertBool "" (isFailure (Quadlet.interpretWatchKey "/k" (Just (0o100600, 0))))
        assertEqual "" Success (Quadlet.interpretWatchKey "/k" (Just (0o100600, 45)))
    , testCase "the key node makes the key, then skips; down leaves it" $ withTempDir $ \dir -> do
        let c = app{Quadlet.containerUnitDir = dir}
        act <- maybe (assertFailure "no node") pure (opAct (Quadlet.watchKeyNode c))
        before <- act.extension.check
        assertBool (show before) (case before of Failure _ -> True; _ -> False)
        act.extension.up
        assertEqual "" Success =<< act.extension.check
        act.extension.down
        assertBool "down removed the key" =<< doesFileExist (Quadlet.watchKeyPath c)
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
            c = app{Quadlet.containerUnitDir = dir, Quadlet.containerEnvFile = Just env}
        Quadlet.ensureWatchKey c
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
    , testCase "the verdict on another quadlet's container does not quote its label" $
        -- it may be a label from before the digest was keyed, and the text is a report
        case Quadlet.interpretRunning "abc123" "def456\n" of
            Failure why -> assertBool (Text.unpack why) (not ("def456" `Text.isInfixOf` why))
            other -> assertFailure (show other)
    , testCase "a container started before the digest was keyed, from this declaration and these files, is satisfied" $ do
        assertEqual "" Success (Quadlet.interpretStarted "keyed1" "plain1" "plain1\n")
        assertEqual "" Success (Quadlet.interpretStarted "keyed1" "plain1" "keyed1\n")
        assertBool "" (isFailure (Quadlet.interpretStarted "keyed1" "plain1" "plain0\n"))
        assertBool "" (isFailure (Quadlet.interpretStarted "keyed1" "plain1" "<no value>\n"))
    , testCase "a container started before the digest was keyed and before its env file was rotated needs bringing up" $ withTempDir $ \dir -> do
        let env = dir </> "env"
            c = app{Quadlet.containerUnitDir = dir, Quadlet.containerEnvFile = Just env}
        Quadlet.ensureWatchKey c
        writeFile env "TOKEN=one\n"
        started <- Quadlet.unkeyedFingerprint c
        declaredOne <- Quadlet.quadletFingerprint c
        assertBool "the keyed fingerprint is the plain one" (started /= declaredOne)
        assertEqual "" Success (Quadlet.interpretStarted declaredOne started (started <> "\n"))
        writeFile env "TOKEN=two\n"
        declared <- Quadlet.quadletFingerprint c
        unkeyed <- Quadlet.unkeyedFingerprint c
        assertBool "" (isFailure (Quadlet.interpretStarted declared unkeyed (started <> "\n")))
    , testCase "the fingerprint from before the key is the one such a container carries" $ do
        -- worked out outside this code from the text "a full container, key
        -- by key" pins and the trailing comment as it was written then
        -- (sha256 of "PATH:0:" for a file that is not there, base64): were it
        -- to move, every container started before the key would be restarted
        let c = app{Quadlet.containerEnvFile = Just "/nonexistent/salmon/env"}
        assertEqual "" "rOdZlNjoUBdb" =<< Quadlet.unkeyedFingerprint c
    , testCase "a file rewritten for the key alone is a reload and no restart" $ do
        let active = Just (Quadlet.parseSample ["ActiveState=active", "SubState=running", "NRestarts=0", "Result=success"])
            stopped = Just (Quadlet.parseSample ["ActiveState=inactive", "SubState=dead", "NRestarts=0", "Result=success"])
        assertBool "" (Quadlet.interpretAdoptable "keyed1" "plain1" active (Just "plain1\n"))
        assertBool "a container already on the keyed label was adopted" (not (Quadlet.interpretAdoptable "keyed1" "plain1" active (Just "keyed1\n")))
        assertBool "a container from another declaration was adopted" (not (Quadlet.interpretAdoptable "keyed1" "plain1" active (Just "plain0\n")))
        assertBool "a stopped unit was adopted" (not (Quadlet.interpretAdoptable "keyed1" "plain1" stopped (Just "plain1\n")))
        assertBool "a unit systemd cannot describe was adopted" (not (Quadlet.interpretAdoptable "keyed1" "plain1" Nothing (Just "plain1\n")))
        assertBool "a container podman cannot describe was adopted" (not (Quadlet.interpretAdoptable "keyed1" "plain1" active Nothing))
        assertBool "a quadlet watching nothing was adopted" (not (Quadlet.interpretAdoptable "same" "same" active (Just "same\n")))
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
    , testCase "a quadlet that watches a file stands on the directory's key, and one that watches nothing has none" $ do
        let dagOf o = Dag.foldDag Dag.sameRepresentative (evalDeps o)
            refsOf dag short = [r | (r, act) <- Map.toList (Dag.dagNodes dag), act.shorthand == short]
            watching = dagOf (node app)
        key <- one "key node" (refsOf watching "podman-quadlet-key")
        file <- one "file node" (refsOf watching "file-contents")
        dir <- one "directory node" (refsOf watching "podman-quadlet-dir")
        assertEqual "" (mkRef "podman-quadlet-key" ("/etc/containers/systemd/.salmon-watch.key" :: FilePath)) key
        assertBool "the file does not wait for the key" (key `elem` toList (Map.findWithDefault mempty file (Dag.dagDependencies watching)))
        assertBool "the key does not wait for the directory" (dir `elem` toList (Map.findWithDefault mempty key (Dag.dagDependencies watching)))
        assertEqual "" [] (refsOf (dagOf (node app{Quadlet.containerEnvFile = Nothing})) "podman-quadlet-key")
        let job = Quadlet.containerJob (Podman.ContainerName "job") "app:v3" ["run"]
            jobNode = Quadlet.quadletJob silent ignoreTrack ignoreTrack
        assertEqual "" [] (refsOf (dagOf (jobNode job)) "podman-quadlet-key")
        assertEqual "" 1 (length (refsOf (dagOf (jobNode job{Quadlet.containerWatched = ["/etc/job.conf"]})) "podman-quadlet-key"))
    , testCase "two quadlets in one directory share its key without a conflict" $ do
        let other = app{Quadlet.containerName = Podman.ContainerName "other"}
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps (op "both" (deps [node app, node other]) id))
        assertEqual "" 1 (length [() | act <- Map.elems (Dag.dagNodes dag), act.shorthand == "podman-quadlet-key"])
        assertEqual "the shared key was described two ways" 0 (length (Dag.dagConflicts dag))
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

{- | 'Quadlet.containerBinds': what is rendered, what is refused, where the
directory's node sits, and the node itself against a scratch directory (no
podman, no systemd).
-}
bindTests :: [TestTree]
bindTests =
    [ testCase "a declaration with no binds is rendered, keyed and described as it was, with no node added" $ do
        -- "a full container, key by key" pins the text and "a declaration
        -- that sets no stamp" its hash; this is the rest of the graph
        assertEqual "" [] app.containerBinds
        assertEqual "" [] (map shorthandOf (Quadlet.hostDirNodes app))
        -- the key is there for the env file `app` watches, not for a bind
        assertEqual
            ""
            ["file-contents", "podman-quadlet", "podman-quadlet-dir", "podman-quadlet-image", "podman-quadlet-key"]
            (sort (shorthands (node app)))
        assertEqual
            ""
            ["file-contents", "podman-quadlet", "podman-quadlet-dir", "podman-quadlet-image"]
            (sort (shorthands (node app{Quadlet.containerEnvFile = Nothing})))
        assertEqual "" 1 (length (filter ("Volume=" `Text.isPrefixOf`) (Text.lines (Quadlet.renderContainer app))))
    , testCase "a bind is one more Volume line, after the plain ones, with its options" $ do
        let c = app{Quadlet.containerBinds = [(Quadlet.bind "/srv/pg" "/var/lib/postgresql/data"){Quadlet.bindChown = True, Quadlet.bindRelabel = Just Quadlet.RelabelPrivate}]}
        assertEqual
            ""
            ["Volume=/srv/app:/data:ro", "Volume=/srv/pg:/var/lib/postgresql/data:rw,U,Z"]
            (filter ("Volume=" `Text.isPrefixOf`) (Text.lines (Quadlet.renderContainer c)))
        assertEqual
            "the bind is not the only difference"
            (Text.lines (Quadlet.renderContainer app))
            (filter (/= "Volume=/srv/pg:/var/lib/postgresql/data:rw,U,Z") (Text.lines (Quadlet.renderContainer c)))
    , testCase "the options are spelled as podman spells them" $ do
        let b = Quadlet.bind "/srv/pg" "/data"
        assertEqual "" "/srv/pg:/data:rw" (Quadlet.renderBind b)
        assertEqual "" "/srv/pg:/data:ro" (Quadlet.renderBind b{Quadlet.bindMode = Podman.ReadOnly})
        assertEqual "" "/srv/pg:/data:rw,U" (Quadlet.renderBind b{Quadlet.bindChown = True})
        assertEqual "" "/srv/pg:/data:rw,z" (Quadlet.renderBind b{Quadlet.bindRelabel = Just Quadlet.RelabelShared})
        assertEqual "" "/srv/pg:/data:ro,Z" (Quadlet.renderBind b{Quadlet.bindMode = Podman.ReadOnly, Quadlet.bindRelabel = Just Quadlet.RelabelPrivate})
    , testCase "the directory's owner and mode are not in the quadlet, so stating them restarts nothing" $ do
        let with d = app{Quadlet.containerBinds = [(Quadlet.bind "/srv/pg" "/data"){Quadlet.bindCreate = d}]}
            stated = Just Quadlet.HostDir{Quadlet.hostDirOwner = Just "70:70", Quadlet.hostDirMode = Just "0700"}
        assertEqual "" (Quadlet.renderContainer (with Nothing)) (Quadlet.renderContainer (with stated))
        assertEqual "" (notesOf (node (with Nothing))) (notesOf (node (with stated)))
    , testCase "a well-formed bind has no problems" $ do
        assertEqual "" [] (Quadlet.bindProblems (Quadlet.bind "/srv/pg" "/data"))
        assertEqual
            ""
            []
            ( Quadlet.containerProblems
                app{Quadlet.containerBinds = [(Quadlet.bind "/srv/pg" "/data"){Quadlet.bindCreate = Just (Quadlet.HostDir (Just "postgres:postgres") (Just "750"))}]}
            )
    , testCase "a relative path, a colon, an owner beside :U, a mode that is not octal are refused" $ do
        let b = Quadlet.bind "/srv/pg" "/data"
            owned o = b{Quadlet.bindCreate = Just Quadlet.hostDir{Quadlet.hostDirOwner = Just o}}
            moded m = b{Quadlet.bindCreate = Just Quadlet.hostDir{Quadlet.hostDirMode = Just m}}
            refused what x = assertBool what (not (null (Quadlet.bindProblems x)))
        refused "a volume name" b{Quadlet.bindHostPath = "pgdata"}
        refused "a relative guest path" b{Quadlet.bindGuestPath = "data"}
        refused "a colon in the host path" b{Quadlet.bindHostPath = "/srv/pg:ro"}
        refused "an owner and :U" (owned "70:70"){Quadlet.bindChown = True}
        refused "an empty owner" (owned "")
        refused "an owner that is an option" (owned "-R")
        refused "an owner of two words" (owned "postgres postgres")
        refused "a symbolic mode" (moded "u+rwx")
        refused "a mode of two digits" (moded "75")
        refused "a mode with a 9" (moded "0790")
        assertBool "a line break in a bind was not refused" $
            not (null (Quadlet.containerProblems app{Quadlet.containerBinds = [b{Quadlet.bindGuestPath = "/data\nImage=evil"}]}))
        assertBool "one directory created two ways was not refused" $
            not (null (Quadlet.containerProblems app{Quadlet.containerBinds = [owned "70:70", (moded "0700"){Quadlet.bindGuestPath = "/other"}]}))
    , testCase "the quadlet file stands on the directory, for a service and for a job" $ do
        let binds = [Quadlet.bind "/srv/pg" "/data"]
            job = (Quadlet.containerJob (Podman.ContainerName "job") "app:v3" ["run"]){Quadlet.containerBinds = binds}
            standing o = do
                let dag = Dag.foldDag Dag.sameRepresentative (evalDeps o)
                    refsOf short = [r | (r, act) <- Map.toList (Dag.dagNodes dag), act.shorthand == short]
                dirRef <- one "bind directory node" (refsOf "podman-quadlet-bind-dir")
                file <- one "file node" (refsOf "file-contents")
                assertBool "the file does not wait for the directory" $
                    dirRef `elem` toList (Map.findWithDefault mempty file (Dag.dagDependencies dag))
                assertEqual "a conflict" 0 (length (Dag.dagConflicts dag))
        standing (node app{Quadlet.containerBinds = binds})
        standing (Quadlet.quadletJob silent ignoreTrack ignoreTrack job)
    , testCase "only a bind that says so gets a directory node, and one path gets one" $ do
        let b = Quadlet.bind "/srv/pg" "/data"
            c = app{Quadlet.containerBinds = [b, b{Quadlet.bindGuestPath = "/again"}, (Quadlet.bind "/srv/theirs" "/x"){Quadlet.bindCreate = Nothing}]}
        assertEqual "" 1 (length (Quadlet.hostDirNodes c))
        assertEqual "" [Just (mkRef "podman-quadlet-bind-dir" ("/srv/pg" :: FilePath))] (map refOf (Quadlet.hostDirNodes c))
    , testCase "two containers binding one directory the same way share its node" $ do
        let binds = [Quadlet.bind "/srv/shared" "/data"]
            a = app{Quadlet.containerBinds = binds}
            b = a{Quadlet.containerName = Podman.ContainerName "other"}
            dag = Dag.foldDag Dag.sameRepresentative (evalDeps (op "both" (deps [node a, node b]) id))
        assertEqual "" 1 (length [() | act <- Map.elems (Dag.dagNodes dag), act.shorthand == "podman-quadlet-bind-dir"])
        assertEqual "the shared directory was described two ways" 0 (length (Dag.dagConflicts dag))
    , testCase "the directory's verdict: there, something else there, missing" $ do
        assertEqual "" Success (Quadlet.interpretHostDir "/srv/pg" True True)
        assertBool "" (isFailure (Quadlet.interpretHostDir "/srv/pg" False True))
        assertBool "" (isFailure (Quadlet.interpretHostDir "/srv/pg" False False))
        assertEqual "" ["--", "70:70", "/srv/pg"] (Quadlet.chownArgs "70:70" "/srv/pg")
    , testCase "a missing directory is created with its parents and its mode, then skipped" $ withTempDir $ \tmp -> do
        let path = tmp </> "data" </> "pg"
        act <- dirAct path Quadlet.hostDir{Quadlet.hostDirMode = Just "0750"}
        act.extension.check >>= assertBool "a missing directory is satisfied" . isFailure
        act.extension.up
        modeOf path >>= assertEqual "" 0o750
        act.extension.check >>= assertEqual "" Success
        act.extension.up
        modeOf path >>= assertEqual "a second up changed the mode" 0o750
    , testCase "an existing directory is left with the mode it has" $ withTempDir $ \tmp -> do
        let path = tmp </> "pg"
        createDirectory path
        setFileMode path 0o700
        act <- dirAct path Quadlet.hostDir{Quadlet.hostDirMode = Just "0755"}
        act.extension.check >>= assertEqual "" Success
        act.extension.up
        modeOf path >>= assertEqual "the image's own mode was overwritten" 0o700
    , testCase "an owner one may name is applied at creation" $ withTempDir $ \tmp -> do
        let path = tmp </> "pg"
        uid <- getEffectiveUserID
        act <- dirAct path Quadlet.hostDir{Quadlet.hostDirOwner = Just (Text.pack (show uid))}
        act.extension.up
        doesDirectoryExist path >>= assertBool "no directory"
    , testCase "an owner that cannot be given fails up and leaves no directory behind" $ withTempDir $ \tmp -> do
        let path = tmp </> "pg"
        act <- dirAct path Quadlet.hostDir{Quadlet.hostDirOwner = Just "no-such-user-salmon-test"}
        outcome <- try act.extension.up :: IO (Either SomeException ())
        assertBool "up did not throw" (either (const True) (const False) outcome)
        doesPathExist path >>= assertBool "the half-made directory would read as satisfied next pass" . not
        act.extension.check >>= assertBool "" . isFailure
    , testCase "a file where the directory should be is a failure, and up throws" $ withTempDir $ \tmp -> do
        let path = tmp </> "pg"
        writeFile path "not a directory"
        act <- dirAct path Quadlet.hostDir
        act.extension.check >>= assertBool "" . isFailure
        outcome <- try act.extension.up :: IO (Either SomeException ())
        assertBool "up did not throw" (either (const True) (const False) outcome)
        readFile path >>= assertEqual "the file was touched" "not a directory"
    , testCase "an invalid declaration creates nothing" $ withTempDir $ \tmp -> do
        let path = tmp </> "pg"
        act <- dirAct path Quadlet.hostDir{Quadlet.hostDirMode = Just "rwx"}
        outcome <- try act.extension.up :: IO (Either SomeException ())
        assertBool "up did not throw" (either (const True) (const False) outcome)
        doesPathExist path >>= assertBool "a directory was made for a refused declaration" . not
    , testCase "down leaves the directory and what is in it" $ withTempDir $ \tmp -> do
        let path = tmp </> "pg"
        act <- dirAct path Quadlet.hostDir
        act.extension.up
        writeFile (path </> "PG_VERSION") "16\n"
        act.extension.down
        readFile (path </> "PG_VERSION") >>= assertEqual "the data went with the container" "16\n"
        act.extension.check >>= assertEqual "" Success
    ]
  where
    one :: String -> [a] -> IO a
    one _ [x] = pure x
    one what xs = assertFailure ("not exactly one " <> what <> ": " <> show (length xs))
    node = Quadlet.quadletContainer silent ignoreTrack ignoreTrack
    refOf o = fmap (\act -> act.extension.ref) (opAct o)
    notesOf o = fmap (\act -> act.extension.notes) (opAct o)
    shorthandOf o = fmap (\act -> act.shorthand) (opAct o)
    shorthands o = [act.shorthand | act <- Map.elems (Dag.dagNodes (Dag.foldDag Dag.sameRepresentative (evalDeps o)))]
    isFailure (Failure _) = True
    isFailure _ = False
    modeOf path = (.&. 0o7777) . fileMode <$> getFileStatus path
    dirAct path d = do
        let b = (Quadlet.bind path "/data"){Quadlet.bindCreate = Just d}
        case Quadlet.hostDirNodes app{Quadlet.containerBinds = [b]} of
            [o] -> maybe (assertFailure "the directory node has no actions") pure (opAct o)
            os -> assertFailure ("not exactly one directory node: " <> show (length os))

generatorPath :: FilePath
generatorPath = "/usr/libexec/podman/quadlet"

generatorTests :: [TestTree]
generatorTests =
    [ testCase "podman's own generator accepts the rendered file" $ withGenerator $ withTempDir $ \dir -> do
        let envFile = dir </> "env"
            c = app{Quadlet.containerUnitDir = dir, Quadlet.containerEnvFile = Just envFile}
        writeFile envFile "PORT=80\n"
        -- the key sits in the directory the generator reads, which must not mind it
        Quadlet.ensureWatchKey c
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
        assertBool ("the generator said something about the key: " <> said) (not (".salmon-watch.key" `isInfixOf` said))
        assertBool said ("--authfile=/etc/app/auth.json" `isInfixOf` said)
        assertBool said (("--env-file " <> envFile) `isInfixOf` said)
        assertBool said ("--publish 8080:80/tcp" `isInfixOf` said)
        assertBool said (("--label " <> Text.unpack (Quadlet.quadletLabel <> "=" <> fingerprint)) `isInfixOf` said)
        assertBool said ("europe-west1-docker.pkg.dev/acme/repo/app:v3" `isInfixOf` said)
        assertBool said (("SourcePath=" <> Quadlet.quadletPath c) `isInfixOf` said)
    , testCase "podman's own generator passes a bind's options through" $ withGenerator $ withTempDir $ \dir -> do
        let hostPath = dir </> "pg"
            b = (Quadlet.bind hostPath "/var/lib/postgresql/data"){Quadlet.bindChown = True, Quadlet.bindRelabel = Just Quadlet.RelabelPrivate}
            c = app{Quadlet.containerUnitDir = dir, Quadlet.containerEnvFile = Nothing, Quadlet.containerBinds = [b]}
        Text.writeFile (Quadlet.quadletPath c) =<< Quadlet.renderQuadlet c
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
        assertBool said (("-v " <> hostPath <> ":/var/lib/postgresql/data:rw,U,Z") `isInfixOf` said)
        assertBool said ("-v /srv/app:/data:ro" `isInfixOf` said)
        doesPathExist hostPath >>= assertBool "the generator made the directory" . not
    ]
  where
    withGenerator :: IO () -> IO ()
    withGenerator act = do
        present <- doesFileExist generatorPath
        if present
            then act
            else hPutStrLn stderr ("SKIPPED: no quadlet generator at " <> generatorPath <> "; the rendered file was not checked against podman")

{- | 'Quadlet.awaitReady' against a scripted unit and a clock that only
moves when the wait sleeps: the unit's samples are consumed one per look (the
last one repeating), and the probe says yes once the clock has reached a
given time.
-}
scripted :: [Maybe Quadlet.UnitSample] -> Maybe Int -> IO (Quadlet.Waiting, IORef Int, IORef Int)
scripted samples readyAt = do
    clock <- newIORef 0
    looks <- newIORef 0
    remaining <- newIORef samples
    let next = atomicModifyIORef' remaining $ \xs -> case xs of
            [x] -> ([x], x)
            x : rest -> (rest, x)
            [] -> ([], Nothing)
    pure
        ( Quadlet.Waiting
            { Quadlet.waitSample = modifyIORef looks (+ 1) >> next
            , Quadlet.waitProbe = \_ -> do
                now <- readIORef clock
                pure (maybe False (<= now) readyAt)
            , Quadlet.waitSleep = \ms -> modifyIORef clock (+ ms)
            , Quadlet.waitNow = readIORef clock
            }
        , clock
        , looks
        )

standing :: Int -> Maybe Quadlet.UnitSample
standing n = Just (Quadlet.UnitSample "active" "running" (Just n) "success")

restarting :: Int -> Maybe Quadlet.UnitSample
restarting n = Just (Quadlet.UnitSample "activating" "auto-restart" (Just n) "exit-code")

readinessTests :: [TestTree]
readinessTests =
    [ testCase "a declaration that asks for no readiness is the node it was" $ do
        assertEqual "" Nothing app.containerReady
        assertEqual "" Nothing (Quadlet.container (Podman.ContainerName "x") "img:v1").containerReady
    , testCase "declaring readiness changes neither the file nor its fingerprint, only the node's notes" $ do
        let ready = app{Quadlet.containerReady = Just (Quadlet.stillUp 5)}
        assertEqual "" (Quadlet.renderContainer app) (Quadlet.renderContainer ready)
        plain <- Quadlet.quadletFingerprint app{Quadlet.containerEnvFile = Nothing}
        withIt <- Quadlet.quadletFingerprint ready{Quadlet.containerEnvFile = Nothing}
        assertEqual "the running container would read as started from another quadlet" plain withIt
        assertEqual "" (refOf (node app)) (refOf (node ready))
        assertEqual "" (helpOf (node app)) (helpOf (node ready))
        assertEqual
            ""
            (fmap (<> ["ready: active with no restart for 5s"]) (notesOf (node app)))
            (notesOf (node ready))
    , testCase "readiness is described with its probe" $ do
        assertEqual
            ""
            "tcp 127.0.0.1:8080 accepts within 30s, then active with no restart for 3s"
            (Quadlet.describeReadiness (Quadlet.readyWhen (Quadlet.ProbeTcp "127.0.0.1" 8080) 30))
        assertEqual
            ""
            "`curl -fsS http://127.0.0.1:8080/health` exits 0 within 10s, then active with no restart for 3s"
            (Quadlet.describeReadiness (Quadlet.readyWhen (Quadlet.ProbeCommand "curl" ["-fsS", "http://127.0.0.1:8080/health"]) 10))
    , testCase "readiness on a job, a probe with no time, a port that is not one are refused" $ do
        let job = Quadlet.containerJob (Podman.ContainerName "job") "img:v1" ["true"]
        assertBool "" (not (null (Quadlet.containerProblems job{Quadlet.containerReady = Just (Quadlet.stillUp 3)})))
        assertBool "" (not (null (Quadlet.containerProblems app{Quadlet.containerReady = Just (Quadlet.readyWhen (Quadlet.ProbeTcp "h" 80) 0)})))
        assertBool "" (not (null (Quadlet.containerProblems app{Quadlet.containerReady = Just (Quadlet.readyWhen (Quadlet.ProbeTcp "h" 70000) 5)})))
        assertBool "" (not (null (Quadlet.containerProblems app{Quadlet.containerReady = Just (Quadlet.readyWhen (Quadlet.ProbeCommand "" []) 5)})))
        assertBool "" (not (null (Quadlet.containerProblems app{Quadlet.containerReady = Just (Quadlet.stillUp (-1))})))
        assertEqual "" [] (Quadlet.containerProblems app{Quadlet.containerReady = Just (Quadlet.readyWhen (Quadlet.ProbeTcp "127.0.0.1" 8080) 30)})
    , testCase "the unit is asked for its state and restart count, in its scope" $ do
        assertEqual "" ["show", "app.service", "--property=ActiveState,SubState,NRestarts,Result"] (Quadlet.sampleArgs app)
        assertEqual
            ""
            ["--user", "show", "app.service", "--property=ActiveState,SubState,NRestarts,Result"]
            (Quadlet.sampleArgs app{Quadlet.containerScope = Systemd.User})
    , testCase "systemctl's lines are read whatever their order" $ do
        assertEqual
            ""
            (Quadlet.UnitSample "active" "running" (Just 2) "success")
            (Quadlet.parseSample ["Result=success", "NRestarts=2", "ActiveState=active", "SubState=running"])
        assertEqual "" Nothing (Quadlet.parseSample ["ActiveState=active"]).sampleRestarts
    , testCase "an active unit systemd has not restarted is standing" $ do
        assertEqual "" (Right ()) (Quadlet.interpretStanding [0] (Quadlet.UnitSample "active" "running" (Just 0) "success"))
        assertEqual "no counter to judge by" (Right ()) (Quadlet.interpretStanding [0] (Quadlet.UnitSample "active" "running" Nothing "success"))
    , testCase "a unit being restarted, failed, stopped or already restarted is not" $ do
        assertBool "" (isLeft (Quadlet.interpretStanding [0] (Quadlet.UnitSample "activating" "auto-restart" (Just 0) "exit-code")))
        assertBool "" (isLeft (Quadlet.interpretStanding [0] (Quadlet.UnitSample "failed" "failed" (Just 5) "exit-code")))
        assertBool "" (isLeft (Quadlet.interpretStanding [0] (Quadlet.UnitSample "inactive" "dead" (Just 0) "success")))
        -- the report's case: sampled while active, between two deaths
        assertBool "" (isLeft (Quadlet.interpretStanding [0] (Quadlet.UnitSample "active" "running" (Just 2) "success")))
    , testCase "a container that stays up through the hold is ready, and the hold is waited out" $ do
        (w, clock, _) <- scripted [standing 0] Nothing
        verdict <- Quadlet.awaitReady w (Quadlet.stillUp 3)
        assertEqual "" (Right ()) verdict
        waited <- readIORef clock
        assertEqual "the hold was cut short or overrun" 3000 waited
    , testCase "a container that dies at boot fails the wait at the first look that sees it" $ do
        -- active when restart returned and at the first look, then systemd's auto-restart
        (w, clock, _) <- scripted [standing 0, standing 0, restarting 1] Nothing
        verdict <- Quadlet.awaitReady w (Quadlet.stillUp 5)
        assertBool ("not a failure: " <> show verdict) (isLeft verdict)
        waited <- readIORef clock
        assertBool "it waited the whole hold for a unit already seen down" (waited < 5000)
    , testCase "a container sampled active between two deaths is caught by its restart count" $ do
        (w, _, _) <- scripted [standing 0, standing 0, standing 1] Nothing
        verdict <- Quadlet.awaitReady w (Quadlet.stillUp 5)
        assertBool ("not a failure: " <> show verdict) (isLeft verdict)
    , testCase "a unit already down at the first look is not waited for" $ do
        (w, clock, looks) <- scripted [restarting 0] Nothing
        verdict <- Quadlet.awaitReady w (Quadlet.stillUp 5)
        assertBool ("not a failure: " <> show verdict) (isLeft verdict)
        n <- readIORef looks
        assertEqual "it kept looking" 1 n
        waited <- readIORef clock
        assertEqual "" 0 waited
    , testCase "a restart count left over from before the restart is not a restart" $ do
        -- `systemctl restart` does not reset NRestarts on a unit systemd was
        -- restarting: the baseline is what the first look sees
        (w, _, _) <- scripted [standing 3] Nothing
        verdict <- Quadlet.awaitReady w (Quadlet.stillUp 2)
        assertEqual "" (Right ()) verdict
    , testCase "the probe is waited for, then the hold starts" $ do
        (w, clock, _) <- scripted [standing 0] (Just 2000)
        verdict <- Quadlet.awaitReady w (Quadlet.readyWhen (Quadlet.ProbeTcp "127.0.0.1" 8080) 30)
        assertEqual "" (Right ()) verdict
        waited <- readIORef clock
        assertEqual "2s until the probe held, then the 3s hold" 5000 waited
    , testCase "a probe that never holds fails the wait at its timeout, naming the probe" $ do
        (w, clock, _) <- scripted [standing 0] Nothing
        verdict <- Quadlet.awaitReady w (Quadlet.readyWhen (Quadlet.ProbeTcp "127.0.0.1" 8080) 4)
        case verdict of
            Left why -> assertBool (Text.unpack why) ("tcp 127.0.0.1:8080" `Text.isInfixOf` why)
            Right () -> assertFailure "a container that never answered was called ready"
        waited <- readIORef clock
        assertEqual "" 4000 waited
    , testCase "a container that dies while its probe is waited for does not wait out the timeout" $ do
        (w, clock, _) <- scripted [standing 0, standing 0, restarting 1] Nothing
        verdict <- Quadlet.awaitReady w (Quadlet.readyWhen (Quadlet.ProbeTcp "127.0.0.1" 8080) 60)
        assertBool ("not a failure: " <> show verdict) (isLeft verdict)
        waited <- readIORef clock
        assertBool "it waited for a probe of a unit already seen down" (waited < 5000)
    , testCase "a container that dies during the hold, after its probe held, is not ready" $ do
        (w, _, _) <- scripted [standing 0, standing 0, standing 0, restarting 1] (Just 0)
        verdict <- Quadlet.awaitReady w (Quadlet.readyWhen (Quadlet.ProbeTcp "127.0.0.1" 8080) 30)
        assertBool ("not a failure: " <> show verdict) (isLeft verdict)
    , testCase "a systemd that cannot be asked is not a ready container" $ do
        (w, _, _) <- scripted [Nothing] Nothing
        verdict <- Quadlet.awaitReady w (Quadlet.stillUp 1)
        assertBool ("not a failure: " <> show verdict) (isLeft verdict)
    ]
  where
    isLeft :: Either a b -> Bool
    isLeft = either (const True) (const False)
    node = Quadlet.quadletContainer silent ignoreTrack ignoreTrack
    refOf o = fmap (\act -> act.extension.ref) (opAct o)
    notesOf o = fmap (\act -> act.extension.notes) (opAct o)
    helpOf o = fmap (\act -> act.extension.help) (opAct o)

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
