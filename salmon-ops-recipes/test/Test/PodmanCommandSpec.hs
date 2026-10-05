{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Podman"'s command rendering --
pure, so it needs no real @podman@ (that's what @Test.PodmanSpec@, Layer 2,
is for).
-}
module Test.PodmanCommandSpec (tests) where

import Control.Exception (SomeException, throwIO, try)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.Process (CmdSpec (..), cmdspec, cwd)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Command (..))
import qualified Salmon.Builtin.Nodes.Podman as Podman
import Salmon.Op.Actions (Act (..))
import Salmon.Reporter (silent)
import Test.Harness (withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Podman.podmanCommand"
        [ testCase "push with no authfile runs `podman push <tag>`, the same tag build produced" pushRendersTagNoAuth
        , testCase "push with an authfile passes --authfile to push, not to podman itself" pushRendersTagWithAuth
        , testCase "logout targets the same authfile login wrote to" logoutRendersAuthFile
        , testCase "rmi tolerates an image that is already gone" rmiIgnoresMissing
        , testCase "build with no options runs in the Containerfile's directory, as it always did" buildDefault
        , testCase "the options-less Build constructor renders the same as BuildWith defaultBuildOptions" buildConstructorsAgree
        , testCase "build with a target adds --target and still runs in the Containerfile's directory" buildTargetOnly
        , testCase "build with a context passes it as the positional argument and leaves the working directory alone" buildContextOnly
        , testCase "build with a context and a target" buildContextAndTarget
        , testGroup "login renewal" loginTests
        ]

{- | 'Podman.loginExpiring' and 'Podman.pushLoggingIn', as far as they go
without a registry: the stamp's reading, and the node's check\/up\/down
around an authentication that is a stand-in ('Podman.expiring' takes it as
an argument for this). No @podman login@ runs here.
-}
loginTests :: [TestTree]
loginTests =
    [ testCase "no stamp is not logged in" $
        assertBool "" (isFailure (Podman.interpretLoginStamp now Nothing))
    , testCase "a credential with life left is left alone" $
        assertEqual "" Success (Podman.interpretLoginStamp now (Just "1700003000\n"))
    , testCase "a credential inside the margin, or past it, is renewed" $ do
        assertBool "" (isFailure (Podman.interpretLoginStamp now (Just "1700000120")))
        assertBool "" (isFailure (Podman.interpretLoginStamp now (Just "1699990000")))
        assertEqual "" Success (Podman.interpretLoginStamp now (Just "1700000121"))
    , testCase "an hour-long credential is renewed within the hour, not after it" $ do
        let stamp = Just (Text.pack (Podman.renderLoginStamp (addUTCTime 3600 now)))
        assertEqual "" Success (Podman.interpretLoginStamp (addUTCTime 3400 now) stamp)
        assertBool "" (isFailure (Podman.interpretLoginStamp (addUTCTime 3500 now) stamp))
    , testCase "an unreadable stamp is renewed, not trusted" $
        assertBool "" (isFailure (Podman.interpretLoginStamp now (Just "soon")))
    , testCase "the node is not satisfied until it logged in, then is, with the expiry recorded" $
        withLogin (Podman.Credential "tok-1" 3600) $ \authfile said ext -> do
            before <- ext.check
            assertBool "nothing is logged in yet" (isFailure before)
            started <- getCurrentTime
            ext.up
            readIORef said >>= assertEqual "one login, with the credential's secret" ["tok-1"]
            recorded <- readFile (Podman.loginStampPath authfile)
            assertEqual "the expiry is an hour on" Success (Podman.interpretLoginStamp (addUTCTime 3400 started) (Just (Text.pack recorded)))
            assertBool "and no later" (isFailure (Podman.interpretLoginStamp (addUTCTime 3500 started) (Just (Text.pack recorded))))
            after <- ext.check
            assertEqual "a fresh login is left alone" Success after
    , testCase "a recorded expiry that has come is a login again" $
        withLogin (Podman.Credential "tok-2" 3600) $ \authfile said ext -> do
            ext.up
            -- an hour later, as far as the stamp can tell
            writeFile (Podman.loginStampPath authfile) "1700000000\n"
            lapsed <- ext.check
            assertBool "the lapsed credential is noticed" (isFailure lapsed)
            ext.up
            readIORef said >>= assertEqual "logged in a second time" ["tok-2", "tok-2"]
            renewed <- ext.check
            assertEqual "" Success renewed
    , testCase "a stamp with no auth file beside it vouches for nothing" $
        withLogin (Podman.Credential "tok" 3600) $ \authfile _ ext -> do
            createDirectoryIfMissing True (takeDirectory (Podman.getAuthFile authfile))
            writeFile (Podman.loginStampPath authfile) "9999999999\n"
            verdict <- ext.check
            assertBool "" (isFailure verdict)
    , testCase "a login that is refused throws and records nothing" $
        withTempDir $ \tmp -> do
            let authfile = Podman.AuthFile (tmp </> "auth.json")
                refused _ = throwIO (userError "unauthorized")
                ext = Podman.expiring authfile (pure (Podman.Credential "tok" 3600)) refused (loginExtension authfile)
            outcome <- try ext.up :: IO (Either SomeException ())
            assertBool "the failure is not swallowed" (either (const True) (const False) outcome)
            doesFileExist (Podman.loginStampPath authfile) >>= assertEqual "no stamp vouches for a login that failed" False
    , testCase "a credential the margin already covers is refused before it is used" $
        withLogin (Podman.Credential "tok" Podman.loginRefreshMargin) $ \authfile said ext -> do
            outcome <- try ext.up :: IO (Either SomeException ())
            assertBool "" (either (const True) (const False) outcome)
            readIORef said >>= assertEqual "it was not logged in with" []
            doesFileExist (Podman.loginStampPath authfile) >>= assertEqual "" False
    , testCase "going down takes the stamp with the credentials" $
        withLogin (Podman.Credential "tok" 3600) $ \authfile _ ext -> do
            ext.up
            ext.down
            doesFileExist (Podman.loginStampPath authfile) >>= assertEqual "the stamp" False
            doesFileExist (Podman.getAuthFile authfile) >>= assertEqual "the auth file" False
    , testCase "an expiring login is the same effect site as a plain one, described differently" $ do
        let authfile = Podman.AuthFile "/etc/app/auth.json"
            plain = Podman.login silent ignoreTrack authfile registry user (pure "tok")
            tended = Podman.loginExpiring silent ignoreTrack authfile registry user (pure (Podman.Credential "tok" 3600))
        assertEqual "" (refOf plain) (refOf tended)
        assertBool "" (notesOf plain /= notesOf tended)
        plainCheck <- maybe (assertFailure "no node") (\act -> act.extension.check) (opAct plain)
        assertEqual "the plain login still has nothing to ask" Immaterial plainCheck
    , testCase "a push that logs in is the same effect site as a push on that auth file" $ do
        let authfile = Podman.AuthFile "/etc/app/auth.json"
            plain = Podman.push silent ignoreTrack (Just authfile) "europe-docker.pkg.dev/p/r/img:1"
            renewing = Podman.pushLoggingIn silent ignoreTrack authfile registry user (pure "tok") "europe-docker.pkg.dev/p/r/img:1"
        assertEqual "" (refOf plain) (refOf renewing)
        assertBool "" (notesOf plain /= notesOf renewing)
    , testCase "the login comes before the push, and a refused login is not followed by one" $ do
        order <- newIORef ([] :: [String])
        Podman.loggingInThen (modifyIORef order (<> ["login"])) (modifyIORef order (<> ["push"]))
        readIORef order >>= assertEqual "" ["login", "push"]
        writeIORef order []
        outcome <- try (Podman.loggingInThen (throwIO (userError "unauthorized")) (modifyIORef order (<> ["push"]))) :: IO (Either SomeException ())
        assertBool "" (either (const True) (const False) outcome)
        readIORef order >>= assertEqual "nothing was pushed on the old credentials" []
    ]
  where
    now = posixSecondsToUTCTime 1700000000
    registry = Podman.Registry "europe-docker.pkg.dev"
    user = Podman.Username "oauth2accesstoken"

    isFailure (Failure _) = True
    isFailure _ = False

    refOf o = fmap (\act -> act.extension.ref) (opAct o)
    notesOf o = fmap (\act -> act.extension.notes) (opAct o)

    -- the plain login node's own extension: its @down@ is the one under test
    loginExtension :: Podman.AuthFile -> Extension
    loginExtension authfile =
        case opAct (Podman.login silent ignoreTrack authfile registry user (pure "unused")) of
            Just act -> act.extension
            Nothing -> error "Podman.login is not a node"

    -- an expiring login whose authentication writes the auth file and
    -- remembers the password it was given, in place of @podman login@
    withLogin :: Podman.Credential -> (Podman.AuthFile -> IORef [Text] -> Extension -> IO ()) -> IO ()
    withLogin credential body =
        withTempDir $ \tmp -> do
            said <- newIORef []
            let authfile = Podman.AuthFile (tmp </> "auth" </> "auth.json")
                authenticate pw = do
                    createDirectoryIfMissing True (takeDirectory (Podman.getAuthFile authfile))
                    writeFile (Podman.getAuthFile authfile) "{\"auths\":{}}"
                    modifyIORef said (<> [pw])
            body authfile said (Podman.expiring authfile (pure credential) authenticate (loginExtension authfile))

build :: Podman.BuildOptions -> (CmdSpec, Maybe FilePath)
build opts =
    let p = prepare Podman.podmanCommand (Podman.BuildWith opts "/repo/deploy/Containerfile" "img:1")
     in (cmdspec p, cwd p)

buildDefault :: IO ()
buildDefault =
    assertEqual
        ""
        (RawCommand "podman" ["build", "-t", "img:1", "-f", "Containerfile"], Just "/repo/deploy")
        (build Podman.defaultBuildOptions)

buildConstructorsAgree :: IO ()
buildConstructorsAgree =
    let p = prepare Podman.podmanCommand (Podman.Build "/repo/deploy/Containerfile" "img:1")
     in assertEqual "" (build Podman.defaultBuildOptions) (cmdspec p, cwd p)

buildTargetOnly :: IO ()
buildTargetOnly =
    assertEqual
        ""
        (RawCommand "podman" ["build", "-t", "img:1", "-f", "Containerfile", "--target", "api"], Just "/repo/deploy")
        (build Podman.defaultBuildOptions{Podman.buildTarget = Just "api"})

buildContextOnly :: IO ()
buildContextOnly =
    assertEqual
        ""
        (RawCommand "podman" ["build", "-t", "img:1", "-f", "/repo/deploy/Containerfile", "/repo"], Nothing)
        (build Podman.defaultBuildOptions{Podman.buildContext = Just "/repo"})

buildContextAndTarget :: IO ()
buildContextAndTarget =
    assertEqual
        ""
        (RawCommand "podman" ["build", "-t", "img:1", "-f", "/repo/deploy/Containerfile", "--target", "api", "/repo"], Nothing)
        (build Podman.defaultBuildOptions{Podman.buildContext = Just "/repo", Podman.buildTarget = Just "api"})

pushRendersTagNoAuth :: IO ()
pushRendersTagNoAuth =
    assertEqual
        ""
        (RawCommand "podman" ["push", "us-docker.pkg.dev/p/r/img:1"])
        (cmdspec (prepare Podman.podmanCommand (Podman.Push Nothing "us-docker.pkg.dev/p/r/img:1")))

pushRendersTagWithAuth :: IO ()
pushRendersTagWithAuth =
    assertEqual
        ""
        (RawCommand "podman" ["push", "--authfile", "/tmp/auth.json", "us-docker.pkg.dev/p/r/img:1"])
        (cmdspec (prepare Podman.podmanCommand (Podman.Push (Just (Podman.AuthFile "/tmp/auth.json")) "us-docker.pkg.dev/p/r/img:1")))

logoutRendersAuthFile :: IO ()
logoutRendersAuthFile =
    assertEqual
        ""
        (RawCommand "podman" ["logout", "--authfile", "/tmp/auth.json", "us-docker.pkg.dev"])
        (cmdspec (prepare Podman.podmanCommand (Podman.Logout (Podman.AuthFile "/tmp/auth.json") (Podman.Registry "us-docker.pkg.dev"))))

rmiIgnoresMissing :: IO ()
rmiIgnoresMissing =
    assertEqual
        ""
        (RawCommand "podman" ["rmi", "--ignore", "us-docker.pkg.dev/p/r/img:1"])
        (cmdspec (prepare Podman.podmanCommand (Podman.RmiTag "us-docker.pkg.dev/p/r/img:1")))
