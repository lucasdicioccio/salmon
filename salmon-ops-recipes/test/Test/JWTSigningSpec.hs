-- | Layer 0 (structural) + Layer 1 (sandboxed real IO) tests for
-- "SreBox.JWTSigning", picked as the first target because it is
-- self-contained: it only touches the filesystem, no daemons/root needed.
module Test.JWTSigningSpec (tests) where

import Control.Comonad.Cofree (Cofree (..))
import qualified Data.ByteString.Char8 as C8
import qualified Salmon.Actions.UpDown as UpDown
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import Salmon.Builtin.Extension (Op, evalDeps, ignoreTrack)
import Salmon.Op.Track (pureTracked)
import Salmon.Reporter (silent)
import qualified SreBox.JWTSigning as JWT
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Harness
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "SreBox.JWTSigning"
        [ testCase "Layer 0: ref is stable and derived from the output path" layer0Ref
        , testCase "Layer 1: up writes a token file, a second up is a no-op" layer1UpIsIdempotent
        ]

-- A secret that is entirely test-managed: we write the raw key bytes
-- ourselves and hand it in via 'ignoreTrack', so the op under test has no
-- dependency to provision (mirrors how a real caller would pass in an
-- already-existing secret file).
mkOp :: FilePath -> FilePath -> Op
mkOp secretPath jwtPath =
    JWT.signHmac silent secretTracked "{\"sub\":\"test\"}" jwtPath
  where
    secret = Secrets.Secret Secrets.Hex 32 secretPath
    secretTracked = pureTracked ignoreTrack secret

layer0Ref :: IO ()
layer0Ref = do
    let o1 = mkOp "/tmp/does-not-matter-key" "/tmp/a.jwt"
        o2 = mkOp "/tmp/does-not-matter-key" "/tmp/a.jwt"
        o3 = mkOp "/tmp/does-not-matter-key" "/tmp/b.jwt"
        r1 :< _ = evalDeps o1
        r2 :< _ = evalDeps o2
        r3 :< _ = evalDeps o3
    -- same output path => same identity (so upTree dedups correctly)
    assertEqual "same jwtPath yields the same node" (show r1) (show r2)
    -- different output path => different identity
    assertBool "different jwtPath yields a different node" (show r1 /= show r3)

layer1UpIsIdempotent :: IO ()
layer1UpIsIdempotent = withTempDir $ \dir -> do
    let secretPath = dir </> "hmac.key"
        jwtPath = dir </> "token.jwt"
    -- test manages the secret material directly: 32 raw bytes, hex-decodable
    C8.writeFile secretPath (C8.replicate 64 'a')

    let o = mkOp secretPath jwtPath

    -- first up: the file doesn't exist yet, prelim must be Required, and the
    -- token gets written for real
    reports1 <- runUpCapturing o
    assertBool "first up evaluates (file did not exist)" (any isEval reports1)
    exists1 <- doesFileExist jwtPath
    exists1 @?= True

    -- second up: skipIfFileExists now sees the file, prelim must skip, and
    -- content is left untouched (no re-signing)
    contentsAfterFirstUp <- C8.readFile jwtPath
    reports2 <- runUpCapturing o
    assertBool "second up is skipped (file now exists)" (any isSkip reports2 && not (any isEval reports2))
    contentsAfterSecondUp <- C8.readFile jwtPath
    contentsAfterSecondUp @?= contentsAfterFirstUp
  where
    isEval (UpDown.Eval _) = True
    isEval _ = False
    isSkip (UpDown.Skip _) = True
    isSkip _ = False
