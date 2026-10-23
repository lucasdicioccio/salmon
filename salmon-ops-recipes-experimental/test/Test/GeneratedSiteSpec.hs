{- | "SreBox.GeneratedSite"'s 'autoPublishSite': the pure decisions, and one
run of the node against real @git@ on local bare repositories (no network,
no kitchen-sink: the producer is a @cp@). Skipped, loudly, without @git@ on
the @PATH@.
-}
module Test.GeneratedSiteSpec (tests) where

import Data.Functor.Identity (runIdentity)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (proc, readProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..))
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Git as Git
import Salmon.Op.Actions (Act (..), Actions (..))
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Reporter (silent)

import SreBox.GeneratedSite

tests :: TestTree
tests =
    testGroup
        "SreBox.GeneratedSite"
        [ testGroup
            "decisions"
            [ testCase "ls-remote: the branch's own head, not a look-alike" $
                assertEqual
                    ""
                    (Just "bbb")
                    (parseLsRemote (Git.Branch "main") "aaa\trefs/heads/main-old\nbbb\trefs/heads/main\n")
            , testCase "ls-remote: no such branch" $
                assertEqual "" Nothing (parseLsRemote (Git.Branch "main") "")
            , testCase "never published is a failure" $
                assertEqual "" (Failure "never published") (freshness Nothing (Just "abc"))
            , testCase "an unreachable or empty source is unknown, not a restart" $
                assertEqual "" Unknown (freshness (Just "abc") Nothing)
            , testCase "same revision is fresh" $
                assertEqual "" Success (freshness (Just "abc") (Just "abc"))
            , testCase "a moved source is a failure" $
                assertEqual "" (Failure "source moved from abc to def") (freshness (Just "abc") (Just "def"))
            , testCase "an empty porcelain status commits nothing" $ do
                assertBool "empty" (not (outputChanged ""))
                assertBool "whitespace" (not (outputChanged "\n"))
                assertBool "a change" (outputChanged " M index.html\n")
            , testCase "push only where the remote is not" $ do
                assertBool "same head" (not (needsPush "abc" (Just "abc")))
                assertBool "other head" (needsPush "abc" (Just "def"))
                assertBool "no remote branch" (needsPush "abc" Nothing)
            , testCase "the commit message carries the source revision" $
                assertEqual
                    ""
                    "publish\nbody\n\nSalmon-Source-Revision: abc\n"
                    (publishCommitMessage (Git.CommitMessage (Git.Headline "publish") "body") "abc")
            ]
        , testCase "against local git: publishes, skips when fresh, follows the source" localGit
        ]

localGit :: IO ()
localGit = do
    found <- findExecutable "git"
    case found of
        Nothing -> putStrLn "SKIPPED: GeneratedSiteSpec local git run, no git on PATH"
        Just _ -> withSystemTempDirectory "salmon-generated-site" run
  where
    run tmp = do
        mapM_
            (uncurry setEnv)
            [ ("GIT_AUTHOR_NAME", "salmon-test")
            , ("GIT_AUTHOR_EMAIL", "salmon-test@example.invalid")
            , ("GIT_COMMITTER_NAME", "salmon-test")
            , ("GIT_COMMITTER_EMAIL", "salmon-test@example.invalid")
            ]
        let srcBare = tmp </> "src.git"
            outBare = tmp </> "out.git"
            seed = tmp </> "seed"
        _ <- git tmp ["init", "-q", "--bare", "-b", "main", srcBare]
        _ <- git tmp ["init", "-q", "--bare", "-b", "main", outBare]
        _ <- git tmp ["clone", "-q", srcBare, seed]
        writeFile (seed </> "index.txt") "v1\n"
        commitAndPush seed "v1"
        let outSeed = tmp </> "out-seed"
        _ <- git tmp ["clone", "-q", outBare, outSeed]
        writeFile (outSeed </> "README") "published site\n"
        commitAndPush outSeed "init"

        let clones = tmp </> "clones"
            ap =
                AutoPublish
                    { autoPublishName = "toy"
                    , autoPublishStateDir = tmp </> "state"
                    , autoPublishGit = ignoreTrack
                    , autoPublishSourceRepo = Git.Repo clones "src" (Git.Remote (Text.pack srcBare)) (Git.Branch "main")
                    , autoPublishOutputRepo = Git.Repo clones "out" (Git.Remote (Text.pack outBare)) (Git.Branch "main")
                    , autoPublishAuthor = Git.Author "salmon <salmon@example.invalid>"
                    , autoPublishCommitMessage = Git.CommitMessage (Git.Headline "auto-publish") ""
                    , autoPublishTagName = Just (Git.TagName "published")
                    , autoPublishPrereqs = []
                    , autoPublishProduce = \src out -> proc "cp" [src </> "index.txt", out </> "index.txt"]
                    }
            node = autoPublishSite silent ap
            check' = checkOf node
            commits = length . lines <$> git tmp ["--git-dir", outBare, "rev-list", "main"]
        createDirectoryIfMissing True clones

        before <- check'
        assertEqual "nothing published yet" (Failure "never published") before
        ok1 <- upTree node
        assertBool "first pass" ok1
        published1 <- git tmp ["--git-dir", outBare, "show", "main:index.txt"]
        assertEqual "v1 pushed" "v1\n" published1
        tagged <- git tmp ["--git-dir", outBare, "rev-parse", "published^{commit}"]
        headOut <- git tmp ["--git-dir", outBare, "rev-parse", "main"]
        assertEqual "the tag was pushed at the head" headOut tagged
        n1 <- commits
        assertEqual "one commit on top of the seed" 2 n1
        fresh <- check'
        assertEqual "fresh after publishing" Success fresh

        ok2 <- upTree node
        assertBool "second pass" ok2
        upOf node
        n2 <- commits
        assertEqual "an unchanged source, even forced, commits and pushes nothing" n1 n2

        writeFile (seed </> "index.txt") "v2\n"
        commitAndPush seed "v2"
        moved <- check'
        assertBool "a moved source fails the check" (isFailure moved)
        ok3 <- upTree node
        assertBool "third pass" ok3
        published3 <- git tmp ["--git-dir", outBare, "show", "main:index.txt"]
        assertEqual "v2 pushed" "v2\n" published3
        n3 <- commits
        assertEqual "one more commit" (n1 + 1) n3
        fresh' <- check'
        assertEqual "fresh again" Success fresh'

    commitAndPush dir msg = do
        _ <- git dir ["add", "-A"]
        _ <- git dir ["commit", "-q", "-m", msg]
        _ <- git dir ["push", "-q", "origin", "HEAD:main"]
        pure ()

    git :: FilePath -> [String] -> IO String
    git dir args = readProcess "git" (["-C", dir] <> args) ""

    upTree :: Op -> IO Bool
    upTree = UpDown.upTree silent (pure . runIdentity)

    isFailure (Failure _) = True
    isFailure _ = False

extensionOf :: Op -> Extension
extensionOf o = case o.node of
    Actions act -> act.extension
    Actionless -> error "an auto-publish node has actions"

checkOf :: Op -> IO CheckResult
checkOf = (.check) . extensionOf

upOf :: Op -> IO ()
upOf = (.up) . extensionOf
