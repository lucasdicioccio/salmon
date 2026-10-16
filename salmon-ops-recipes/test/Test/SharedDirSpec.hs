{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for 'FS.sharedDir' and 'FS.filecontentsInSharedDir':
a file in a directory salmon does not own alone (a package's @\/etc\/haproxy@,
@\/etc\/sudoers.d@, a config directory next to a pre-provisioned secret).

'FS.filecontents' stands on 'FS.dir', whose @down@ refuses a non-empty
directory, so a @run down@ removed the file and then reported the directory
@Failed@. The teardown cases here run real file IO in a temporary directory;
the graph-shape cases check that the builtins that write into such
directories no longer declare a plain directory node for them. Nothing here
touches the real @\/etc@ or starts a service.
-}
module Test.SharedDirSpec (tests, assertOnSharedDir) where

import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Test.QuickCheck (Gen, elements, forAll, listOf1, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.Harness (runDown, runDownCapturing, runUp, withTempDir)

import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Haproxy as Haproxy
import qualified Salmon.Builtin.Nodes.Nginx as Nginx
import qualified Salmon.Builtin.Nodes.PgBouncer as PgBouncer
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Reporter (silent)
import qualified SreBox.Initialize as Initialize

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Filesystem.sharedDir"
        [ testCase "tearing down a file in a shared directory is clean and leaves the directory and its other files" $ withTempDir $ \tmp -> do
            let dir = tmp </> "etc-haproxy"
                other = dir </> "errors.http"
                node = FS.filecontentsInSharedDir (FS.FileContents (dir </> "haproxy.cfg") ("global\n" :: Text))
            upOk <- runUp node
            assertBool "up failed" upOk
            assertBool "the file was not written" =<< doesFileExist (dir </> "haproxy.cfg")
            writeFile other "not ours\n"
            reports <- runDownCapturing node
            assertEqual "failed or blocked nodes" [] [shorthandOf rep | rep <- reports, isBad rep]
            assertBool "our file is still there" . not =<< doesFileExist (dir </> "haproxy.cfg")
            assertBool "another file was removed" =<< doesFileExist other
            assertBool "the shared directory was removed" =<< doesDirectoryExist dir
            downAgain <- runDown node
            assertBool "a second down is not clean" downAgain
        , testCase "an empty shared directory is left too" $ withTempDir $ \tmp -> do
            let dir = tmp </> "sudoers.d"
                node = FS.filecontentsInSharedDir (FS.FileContents (dir </> "salmon") ("x\n" :: Text))
            _ <- runUp node
            reports <- runDownCapturing node
            assertEqual "failed or blocked nodes" [] [shorthandOf rep | rep <- reports, isBad rep]
            assertBool "the shared directory was removed" =<< doesDirectoryExist dir
        , testCase "up creates a missing shared directory, parents included" $ withTempDir $ \tmp -> do
            let dir = tmp </> "a" </> "b"
            ok <- runUp (FS.filecontentsInSharedDir (FS.FileContents (dir </> "f") ("x" :: Text)))
            assertBool "up failed" ok
            assertBool "not created" =<< doesFileExist (dir </> "f")
        , testCase "the same file over Filesystem.dir is the shape that failed" $ withTempDir $ \tmp -> do
            -- pins why the predecessor was swapped
            let dir = tmp </> "etc-haproxy"
                plain = FS.filecontents (FS.FileContents (dir </> "haproxy.cfg") ("global\n" :: Text))
            _ <- runUp plain
            writeFile (dir </> "errors.http") "not ours\n"
            reports <- runDownCapturing plain
            assertEqual "" ["directory"] [shorthandOf rep | rep <- reports, isBad rep]
        , testProperty "the file node renders what filecontents rendered" $
            forAll genPath $ \(d, name) ->
                let fc = FS.FileContents (d </> name) ("contents\n" :: Text)
                 in describe (FS.filecontentsInSharedDir fc) === describe (FS.filecontents fc)
        , testProperty "the file stands on exactly one shared-directory, keyed on its directory" $
            forAll genPath $ \(d, name) ->
                let preds = [fmap describe' (opAct o) | o <- predecessorsOf (FS.filecontentsInSharedDir (FS.FileContents (d </> name) ("x" :: Text)))]
                 in preds === [Just ("shared-directory", mkRef "shared-directory" d)]
        , testCase "a shared directory and a plain one of the same path do not conflict" $ do
            let both = op "both" (deps [FS.sharedDir (FS.Directory "/srv/x"), FS.dir (FS.Directory "/srv/x")]) id
            assertEqual "" 0 (length (Dag.dagConflicts (Dag.foldDag Dag.sameRepresentative (evalDeps both))))
        , testGroup
            "builtins writing into shared system directories"
            [ testCase "haproxy's config stands on a shared /etc/haproxy" $
                assertOnSharedDir "/etc/haproxy" (Haproxy.configFiles (Haproxy.patroniRouter [Haproxy.Member "n1" "10.0.0.1" 5432 8008]))
            , testCase "pgbouncer's ini and userlist stand on a shared /etc/pgbouncer" $
                assertOnSharedDir "/etc/pgbouncer" (PgBouncer.configFiles bouncer)
            , testCase "nginx's vhosts stand on a shared sites directory" $
                assertOnSharedDir "/etc/nginx/conf.d" (Nginx.configFiles nginx)
            , testCase "Initialize's sudoers file stands on a shared /etc/sudoers.d" $
                assertOnSharedDir "/etc/sudoers.d" (Initialize.initialize silent Nothing)
            ]
        ]
  where
    describe :: Op -> Maybe (Text, Ref, Text, [Text])
    describe o = fmap (\act -> (act.shorthand, act.extension.ref, act.extension.help, act.extension.notes)) (opAct o)

    describe' :: Act Extension -> (Text, Ref)
    describe' act = (act.shorthand, act.extension.ref)

    predecessorsOf :: Op -> [Op]
    predecessorsOf o = toList (runIdentity o.predecessors)

    genPath :: Gen (FilePath, FilePath)
    genPath = do
        segs <- listOf1 (listOf1 (elements "abcxyz-_"))
        name <- listOf1 (elements "abcxyz-_.")
        pure ("/" <> foldr1 (\a b -> a <> "/" <> b) segs, name)

    bouncer :: PgBouncer.BouncerConfig
    bouncer =
        PgBouncer.BouncerConfig
            { PgBouncer.bouncer_config_dir = "/etc/pgbouncer"
            , PgBouncer.bouncer_listen_addr = "0.0.0.0"
            , PgBouncer.bouncer_listen_port = 6432
            , PgBouncer.bouncer_databases = [PgBouncer.BouncerDatabase "app" (PgBouncer.UpstreamDb "10.0.0.1" 5432 "app")]
            , PgBouncer.bouncer_users = [PgBouncer.AuthUser "router" "hunter2"]
            , PgBouncer.bouncer_pool_mode = PgBouncer.TransactionPooling
            , PgBouncer.bouncer_max_client_conn = 100
            , PgBouncer.bouncer_default_pool_size = 20
            , PgBouncer.bouncer_admin_users = []
            , PgBouncer.bouncer_routing_file = Just "/etc/pgbouncer/routing.ini"
            }

    nginx :: Nginx.NginxConfig
    nginx =
        Nginx.NginxConfig
            { Nginx.nginx_sites_dir = "/etc/nginx/conf.d"
            , Nginx.nginx_vhosts =
                [ Nginx.VHost "a.example" 80 Nginx.Http [Nginx.Upstream "10.0.0.1" 8080]
                , Nginx.VHost "b.example" 80 Nginx.Http [Nginx.Upstream "10.0.0.2" 8080]
                ]
            }

{- | The node's graph declares one 'FS.sharedDir' of @path@ and no 'FS.dir'
of it.
-}
assertOnSharedDir :: FilePath -> Op -> Assertion
assertOnSharedDir path o = do
    let acts = Map.elems (Dag.dagNodes (Dag.foldDag Dag.sameRepresentative (evalDeps o)))
        refs = [(act.shorthand, act.extension.ref) | act <- acts]
    assertEqual "shared-directory nodes of the path" 1 (length (filter (== ("shared-directory", mkRef "shared-directory" path)) refs))
    assertEqual "plain directory nodes of the path" 0 (length (filter ((== mkRef "directory" path) . snd) refs))

shorthandOf :: UpDown.Report Extension -> Text
shorthandOf rep = case rep of
    UpDown.Failed act _ -> act.shorthand
    UpDown.Blocked act -> act.shorthand
    _ -> ""

isBad :: UpDown.Report Extension -> Bool
isBad rep = case rep of
    UpDown.Failed _ _ -> True
    UpDown.Blocked _ -> True
    _ -> False

