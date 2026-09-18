{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Podman"'s command rendering --
pure, so it needs no real @podman@ (that's what @Test.PodmanSpec@, Layer 2,
is for).
-}
module Test.PodmanCommandSpec (tests) where

import System.Process (CmdSpec (..), cmdspec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Salmon.Builtin.Nodes.Binary (Command (..))
import qualified Salmon.Builtin.Nodes.Podman as Podman

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Podman.podmanCommand"
        [ testCase "push with no authfile runs `podman push <tag>`, the same tag build produced" pushRendersTagNoAuth
        , testCase "push with an authfile passes --authfile to push, not to podman itself" pushRendersTagWithAuth
        , testCase "logout targets the same authfile login wrote to" logoutRendersAuthFile
        , testCase "rmi tolerates an image that is already gone" rmiIgnoresMissing
        ]

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
