{-# LANGUAGE OverloadedStrings #-}

{- | Coverage for "Salmon.Builtin.Nodes.Rsync", written with 'receiveDir'.

Layer 0, always run: the argv of every command, the verdict 'receiveDir''s
check draws from @rsync --dry-run --itemize-changes@, the shape of the node,
and that 'sendFile', 'sendDir' and 'receiveFile' render what they rendered
before 'receiveDir' existed. The pure parts carry QuickCheck properties.

Two groups run a real @rsync@:

* "a real rsync, local paths" runs the module's own flags between two local
  directories, skipped loudly without @rsync@. It is what ties the parser to
  what rsync really prints and shows that @--times@ makes a second probe come
  back empty. No ssh is involved, so it says nothing about the remote half.
* "the node, end to end" runs the node itself, and is __opt-in__: it needs an
  @ssh@ on @PATH@ that runs the command locally instead of connecting, which
  this suite will not install for itself (@PATH@ is process-wide and other
  groups, running in parallel, use the real @ssh@). To run it, put this in a
  directory as an executable named @ssh@:

  > #!/bin/sh
  > while [ $# -gt 0 ]; do case "$1" in -l|-i|-o) shift 2;; -*) shift;; *) shift; break;; esac; done
  > exec "$@"

  then run the suite with that directory first on @PATH@ and
  @SALMON_TEST_RSYNC_SSH_SHIM=1@, selecting this group (@-p Rsync@) so that
  nothing else sees the shim.

__No real ssh connection is made by anything here.__ The itemized lines in
the Layer 0 cases were copied from rsync 3.2.7's output.
-}
module Test.RsyncSpec (tests) where

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as C8
import Data.Functor.Identity (runIdentity)
import Data.List (isPrefixOf, (\\))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import System.Process.ListLike (CmdSpec (..), CreateProcess (..))
import Test.QuickCheck (Gen, Property, choose, counterexample, elements, forAll, listOf, listOf1, oneof, shuffle, vectorOf, (.&&.), (=/=), (===), (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Salmon.Actions.Query (pathedNodes)
import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Command (..))
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Builtin.Nodes.Rsync
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.Actions (Act (..))
import Salmon.Op.Eval (expand)
import Salmon.Op.Ref (Ref, mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (silent)

import Test.Harness (requireExecutable, runUp, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Rsync"
        [ commandTests
        , verdictTests
        , nodeTests
        , unchangedTests
        , testGroup
            "properties"
            [ testProperty "a fetch names the remote directory, then the local one, last" prop_receiveOperands
            , testProperty "the probe is the fetch plus the two flags that make it a question" prop_probeIsFetchPlusFlags
            , testProperty "ssh options travel through --rsh, and only when there are some" prop_rshOnlyWithOptions
            , testProperty "no command carries --delete" prop_neverDeletes
            , testProperty "send-file, receive-file and send-dir render what they did" prop_olderCommandsUnchanged
            , testProperty "an item rsync would create or update is pending" prop_changedItemsArePending
            , testProperty "an item with a retouched attribute is pending" prop_retouchedItemsArePending
            , testProperty "an item with no attribute named is unchanged" prop_untouchedItemsAreUnchanged
            , testProperty "only what is recognised is harmless" prop_onlyRecognisedIsHarmless
            , testProperty "a failed rsync is Unknown whatever it printed" prop_failedProbeIsUnknown
            , testProperty "in place exactly when nothing is pending, and the count is the reason" prop_verdictCountsPending
            , testProperty "the verdict ignores order and harmless lines" prop_verdictIgnoresNoiseAndOrder
            , testProperty "the reason names no path" prop_reasonNamesNoPath
            , testProperty "the ref is the four coordinates, not the ssh options" prop_refIgnoresOptions
            , testProperty "a different coordinate is a different ref" prop_refSeparatesCoordinates
            , testProperty "the ref is not the one of a node going the other way" prop_refDistinctFromSiblings
            , testProperty "the older nodes keep their refs and help" prop_olderNodesUnchanged
            ]
        , realRsyncTests
        , endToEndTests
        ]

-------------------------------------------------------------------------------
-- Fixtures.

remote :: Remote
remote = Remote "salmon" "1.2.3.4"

opts :: Ssh.ClientOpts
opts = Ssh.noClientOpts{Ssh.optIdentity = Just "/w/ssh/client", Ssh.optKnownHosts = Just "/w/ssh/known_hosts"}

rshValue :: String
rshValue = "ssh -i /w/ssh/client -o IdentitiesOnly=yes -o UserKnownHostsFile=/w/ssh/known_hosts -o StrictHostKeyChecking=accept-new"

argv :: RsyncCommand -> [String]
argv c = case cmdspec (prepare rsyncRun c) of
    RawCommand "rsync" args -> args
    other -> error ("not an rsync argv: " <> show other)

mkdirTrack :: Track' FS.Directory
mkdirTrack = Track FS.dir

fetch :: Ssh.ClientOpts -> Remote -> FilePath -> FilePath -> Op
fetch o rem src dst = receiveDirWith o silent ignoreTrack mkdirTrack rem src (FS.Directory dst)

rootRef :: Op -> Maybe Ref
rootRef o = (\act -> act.extension.ref) <$> opAct o

rootHelp :: Op -> Maybe Text
rootHelp o = (\act -> act.extension.help) <$> opAct o

rootCheck :: Op -> IO CheckResult
rootCheck o = maybe (fail "the op has no node") (\act -> act.extension.check) (opAct o)

helps :: Op -> [Text]
helps o = [h | (_, _, h) <- pathedNodes (runIdentity (expand o))]

-------------------------------------------------------------------------------
-- Unit: commands.

commandTests :: TestTree
commandTests =
    testGroup
        "commands"
        [ testCase "a directory fetch is recursive, follows links and keeps times" $
            assertEqual
                ""
                ["--copy-links", "--recursive", "--times", "salmon@1.2.3.4:/srv/results", "/var/in"]
                (argv (ReceiveDir remote "/srv/results" "/var/in" Ssh.noClientOpts))
        , testCase "a directory fetch carries ssh options through --rsh" $
            assertEqual
                ""
                ["--copy-links", "--recursive", "--times", "--rsh", rshValue, "salmon@1.2.3.4:/srv/results", "/var/in"]
                (argv (ReceiveDir remote "/srv/results" "/var/in" opts))
        , testCase "the probe is the same transfer, dry and itemized" $
            assertEqual
                ""
                ["--copy-links", "--recursive", "--times", "--dry-run", "--itemize-changes", "--rsh", rshValue, "salmon@1.2.3.4:/srv/results", "/var/in"]
                (argv (ProbeReceiveDir remote "/srv/results" "/var/in" opts))
        , testCase "a trailing slash on the remote path is passed as written" $
            assertEqual
                "it is rsync's way of saying 'the contents of'"
                ["salmon@1.2.3.4:/srv/results/", "/var/in"]
                (drop 3 (argv (ReceiveDir remote "/srv/results/" "/var/in" Ssh.noClientOpts)))
        ]

-------------------------------------------------------------------------------
-- Unit: the verdict.

verdictTests :: TestTree
verdictTests =
    testGroup
        "the check's verdict"
        [ testCase "nothing printed: the copy is in place" $
            assertEqual "" Success (interpretProbe ExitSuccess "")
        , testCase "a first fetch: every item is to be created" $
            assertEqual
                ""
                (Failure "4 item(s) differ from the remote directory")
                (interpretProbe ExitSuccess "cd+++++++++ src/\n>f+++++++++ src/a\ncd+++++++++ src/sub/\n>f+++++++++ src/sub/b\n")
        , testCase "one file grew on the remote" $
            assertEqual
                ""
                (Failure "1 item(s) differ from the remote directory")
                (interpretProbe ExitSuccess ">f.s....... src/a\n")
        , testCase "a directory whose time alone differs still differs" $
            assertEqual
                ""
                (Failure "1 item(s) differ from the remote directory")
                (interpretProbe ExitSuccess ".d..t...... src/\n")
        , testCase "a non-regular file rsync never copies does not keep the node unsatisfied" $
            assertEqual "" Success (interpretProbe ExitSuccess "skipping non-regular file \"src/fifo\"\n")
        , testCase "items reported as unchanged are not differences" $
            assertEqual
                "what a doubled --itemize-changes prints"
                Success
                (interpretProbe ExitSuccess ".d          src/\n.d          src/sub/\n.f          src/sub/b\n")
        , testCase "a missing local directory is a difference" $
            assertEqual
                "an unrecognised line is never read as harmless"
                (Failure "2 item(s) differ from the remote directory")
                (interpretProbe ExitSuccess "created directory /var/in\ncd+++++++++ src/\n")
        , testCase "a deletion notice is a difference" $
            assertEqual "" Pending (classifyProbeLine "*deleting   src/gone")
        , testCase "an unreachable remote could not be looked at" $
            assertEqual "" Unknown (interpretProbe (ExitFailure 255) "")
        , testCase "a partial comparison is not a verdict" $
            assertEqual
                "exit 23, as with a dangling symlink under --copy-links"
                Unknown
                (interpretProbe (ExitFailure 23) ">f+++++++++ src/a\n")
        , testCase "a path that happens to start with a dot is not an unchanged item" $
            assertEqual "" Pending (classifyProbeLine ".hidden")
        ]

-------------------------------------------------------------------------------
-- Unit: the node.

nodeTests :: TestTree
nodeTests =
    testGroup
        "the receive-dir node"
        [ testCase "help says what is copied, from whom, and where" $
            assertEqual
                ""
                (Just "copies /srv/results from salmon@1.2.3.4 under /var/in over rsync")
                (rootHelp node)
        , testCase "the ref is keyed on both paths and the remote" $
            assertEqual
                ""
                (Just (mkRef "rsync-receivedir" ("/srv/results" :: FilePath, "/var/in" :: FilePath, "salmon" :: Text, "1.2.3.4" :: Text)))
                (rootRef node)
        , testCase "the local directory is made first, by the caller's track" $
            assertEqual
                ""
                ["copies /srv/results from salmon@1.2.3.4 under /var/in over rsync", "ensures /var/in exists, including subdirs"]
                (helps node)
        , testCase "receiveDir is receiveDirWith without options" $
            assertEqual
                ""
                (rootRef node, rootHelp node, helps node)
                (let n = receiveDir silent ignoreTrack mkdirTrack remote "/srv/results" (FS.Directory "/var/in") in (rootRef n, rootHelp n, helps n))
        ]
  where
    node = fetch Ssh.noClientOpts remote "/srv/results" "/var/in"

-------------------------------------------------------------------------------
-- Unit: the declarations that existed before render the same.

unchangedTests :: TestTree
unchangedTests =
    testGroup
        "declarations older than receive-dir"
        [ testCase "sendDir: ref, help, and still no check" $ do
            let n = sendDir silent ignoreTrack mkdirTrack (FS.Directory "/local/files") remote "/home/salmon/files"
            assertEqual "" (Just (mkRef "rsync-senddir" ("/local/files" :: FilePath, "salmon" :: Text, "1.2.3.4" :: Text))) (rootRef n)
            assertEqual "" (Just "copies /local/files to /home/salmon/files over rsync") (rootHelp n)
            verdict <- rootCheck n
            assertEqual "adding one would change run up for every caller" Immaterial verdict
        , testCase "receiveFile: ref, help, and still no check" $ do
            let n = receiveFile silent ignoreTrack mkdirTrack remote "/srv/dump.sql" "/var/in/dump.sql"
            assertEqual "" (Just (mkRef "rsync-receivefile" ("/srv/dump.sql" :: FilePath, "/var/in/dump.sql" :: FilePath, "salmon" :: Text, "1.2.3.4" :: Text))) (rootRef n)
            assertEqual "" (Just "copies /srv/dump.sql from salmon@1.2.3.4 over rsync") (rootHelp n)
            verdict <- rootCheck n
            assertEqual "" Immaterial verdict
        , testCase "send-dir argv" $
            assertEqual
                ""
                ["--copy-links", "--recursive", "--rsh", rshValue, "/local/files", "salmon@1.2.3.4:/home/salmon/files"]
                (argv (SendDir "/local/files" remote "/home/salmon/files" opts))
        , testCase "receive-file argv" $
            assertEqual
                ""
                ["--copy-links", "salmon@1.2.3.4:/srv/dump.sql", "/var/in/dump.sql"]
                (argv (ReceiveFile remote "/srv/dump.sql" "/var/in/dump.sql" Ssh.noClientOpts))
        , testCase "send-file argv" $
            assertEqual
                ""
                ["--copy-links", "/local/bin", "salmon@1.2.3.4:/home/salmon/bin"]
                (argv (SendFile "/local/bin" remote "/home/salmon/bin" Ssh.noClientOpts))
        ]

-------------------------------------------------------------------------------
-- Generators.

genName :: Gen String
genName = listOf1 (elements (['a' .. 'z'] <> ['0' .. '9'] <> "._-"))

genPath :: Gen FilePath
genPath = do
    parts <- listOf1 genName
    slash <- elements ["", "/"]
    pure (concatMap ('/' :) (take 4 parts) <> slash)

genRemote :: Gen Remote
genRemote = Remote <$> (Text.pack <$> genName) <*> (Text.pack <$> genName)

genOpts :: Gen Ssh.ClientOpts
genOpts = do
    identity <- oneof [pure Nothing, Just <$> genPath]
    hosts <- oneof [pure Nothing, Just <$> genPath]
    pure Ssh.noClientOpts{Ssh.optIdentity = identity, Ssh.optKnownHosts = hosts}

-- | A remote, a path on it, a local path, and ssh options.
genCoordinates :: Gen (Remote, FilePath, FilePath, Ssh.ClientOpts)
genCoordinates = (,,,) <$> genRemote <*> genPath <*> genPath <*> genOpts

-- | The path part of an itemized line: anything rsync could print on one line.
genItemPath :: Gen String
genItemPath = listOf1 (elements (['a' .. 'z'] <> "/. _-+<>*"))

-- | An item rsync would transfer or create: any update kind but @.@.
genChangedItem :: Gen Text
genChangedItem = do
    update <- elements "<>ch"
    kind <- elements "fdLDS"
    attrs <- vectorOf 9 (elements ".+cstTpoguax? ")
    path <- genItemPath
    pure (Text.pack (update : kind : attrs <> " " <> path))

-- | An item not transferred, with at least one attribute to set.
genRetouchedItem :: Gen Text
genRetouchedItem = do
    kind <- elements "fdLDS"
    before <- choose (0, 8)
    lead <- vectorOf before (elements ". ")
    named <- elements "cstTpoguax+?"
    trail <- vectorOf (8 - before) (elements ". cstTpoguax")
    path <- genItemPath
    pure (Text.pack ('.' : kind : lead <> [named] <> trail <> " " <> path))

-- | An item rsync looked at and would leave alone.
genUntouchedItem :: Gen Text
genUntouchedItem = do
    kind <- elements "fdLDS"
    attrs <- vectorOf 9 (elements ". ")
    path <- genItemPath
    pure (Text.pack ('.' : kind : attrs <> " " <> path))

genNoise :: Gen Text
genNoise =
    oneof
        [ pure ""
        , Text.pack <$> listOf (elements " \t")
        , (\p -> "skipping non-regular file \"" <> Text.pack p <> "\"") <$> genItemPath
        , (\p -> "skipping directory " <> Text.pack p) <$> genItemPath
        ]

genPending :: Gen Text
genPending = oneof [genChangedItem, genRetouchedItem, ("*deleting   " <>) . Text.pack <$> genItemPath]

genHarmless :: Gen Text
genHarmless = oneof [genNoise, genUntouchedItem]

render :: [Text] -> C8.ByteString
render = Text.encodeUtf8 . Text.unlines

-- | Any one line: printable characters, a few of them from the itemized alphabet.
genAnyLine :: Gen Text
genAnyLine = Text.pack <$> listOf (elements (['a' .. 'z'] <> " .<>ch*fdLDS+st/\"-"))

-------------------------------------------------------------------------------
-- Properties: commands.

prop_receiveOperands :: Property
prop_receiveOperands = forAll genCoordinates $ \(rem, src, dst, o) ->
    let expected = [Text.unpack rem.remoteUser <> "@" <> Text.unpack rem.remoteHost <> ":" <> src, dst]
        lastTwo xs = drop (length xs - 2) xs
     in lastTwo (argv (ReceiveDir rem src dst o)) === expected
            .&&. lastTwo (argv (ProbeReceiveDir rem src dst o)) === expected

prop_probeIsFetchPlusFlags :: Property
prop_probeIsFetchPlusFlags = forAll genCoordinates $ \(rem, src, dst, o) ->
    let fetchArgv = argv (ReceiveDir rem src dst o)
        probeArgv = argv (ProbeReceiveDir rem src dst o)
     in filter (`notElem` probeFlags) probeArgv === fetchArgv
            .&&. (probeArgv \\ fetchArgv) === ["--dry-run", "--itemize-changes"]
            .&&. counterexample "the fetch itself is not a dry run" (all (`notElem` fetchArgv) probeFlags)
            .&&. counterexample "both keep times, or the second probe is never empty" ("--times" `elem` fetchArgv && "--times" `elem` probeArgv)

prop_rshOnlyWithOptions :: Property
prop_rshOnlyWithOptions = forAll genCoordinates $ \(rem, src, dst, o) ->
    let args = argv (ReceiveDir rem src dst o)
        sshArgs = Ssh.clientArgs o
     in if null sshArgs
            then counterexample (show args) ("--rsh" `notElem` args)
            else dropWhile (/= "--rsh") args === ["--rsh", unwords ("ssh" : sshArgs)] <> drop (length args - 2) args

prop_neverDeletes :: Property
prop_neverDeletes = forAll genCoordinates $ \(rem, src, dst, o) ->
    let flagsOf c = filter ("--" `isPrefixOf`) (takeWhile (/= "--rsh") (argv c))
        all5 = [SendFile src rem dst o, ReceiveFile rem src dst o, SendDir src rem dst o, ReceiveDir rem src dst o, ProbeReceiveDir rem src dst o]
     in counterexample (show (map flagsOf all5)) (all (not . any ("--del" `isPrefixOf`) . flagsOf) all5)

{- | The three commands that predate 'ReceiveDir', against their rendering
spelled out again here.
-}
prop_olderCommandsUnchanged :: Property
prop_olderCommandsUnchanged = forAll genCoordinates $ \(rem, a, b, o) ->
    let rsh = case Ssh.clientArgs o of [] -> []; args -> ["--rsh", unwords ("ssh" : args)]
        at p = Text.unpack rem.remoteUser <> "@" <> Text.unpack rem.remoteHost <> ":" <> p
     in argv (SendFile a rem b o) === (["--copy-links"] <> rsh <> [a, at b])
            .&&. argv (ReceiveFile rem a b o) === (["--copy-links"] <> rsh <> [at a, b])
            .&&. argv (SendDir a rem b o) === (["--copy-links", "--recursive"] <> rsh <> [a, at b])

-------------------------------------------------------------------------------
-- Properties: the verdict.

prop_changedItemsArePending :: Property
prop_changedItemsArePending = forAll genChangedItem $ \l -> classifyProbeLine l === Pending

prop_retouchedItemsArePending :: Property
prop_retouchedItemsArePending = forAll genRetouchedItem $ \l -> classifyProbeLine l === Pending

prop_untouchedItemsAreUnchanged :: Property
prop_untouchedItemsAreUnchanged = forAll genUntouchedItem $ \l -> classifyProbeLine l === Unchanged

{- | The lopsidedness the parser promises: a line is read as harmless only
for one of the two reasons the module gives.
-}
prop_onlyRecognisedIsHarmless :: Property
prop_onlyRecognisedIsHarmless = forAll (oneof [genAnyLine, genPending, genHarmless]) $ \l ->
    case classifyProbeLine l of
        Pending -> counterexample "" True
        Noise -> counterexample (show l) (Text.null (Text.strip l) || "skipping " `Text.isPrefixOf` l)
        Unchanged ->
            counterexample
                (show l)
                ( Text.length l > itemizedWidth
                    && Text.take 1 l == "."
                    && Text.all (`elem` (" ." :: String)) (Text.take 9 (Text.drop 2 l))
                    && Text.index l itemizedWidth == ' '
                )

prop_failedProbeIsUnknown :: Property
prop_failedProbeIsUnknown =
    forAll (choose (1, 255)) $ \code ->
        forAll (listOf (oneof [genPending, genHarmless, genAnyLine])) $ \ls ->
            interpretProbe (ExitFailure code) (render ls) === Unknown

prop_verdictCountsPending :: Property
prop_verdictCountsPending =
    forAll (listOf genPending) $ \pending ->
        forAll (listOf genHarmless) $ \harmless ->
            forAll (shuffle (pending <> harmless)) $ \ls ->
                interpretProbe ExitSuccess (render ls)
                    === case length pending of
                        0 -> Success
                        n -> Failure (Text.pack (show n) <> " item(s) differ from the remote directory")

prop_verdictIgnoresNoiseAndOrder :: Property
prop_verdictIgnoresNoiseAndOrder =
    forAll (listOf (oneof [genPending, genHarmless, genAnyLine])) $ \ls ->
        forAll (listOf genHarmless) $ \extra ->
            forAll (shuffle (ls <> extra)) $ \mixed ->
                interpretProbe ExitSuccess (render mixed) === interpretProbe ExitSuccess (render ls)

-- | Failure text is public: it says how many, never which.
prop_reasonNamesNoPath :: Property
prop_reasonNamesNoPath =
    forAll (listOf1 genName) $ \names ->
        let paths = map (\n -> "dir-" <> n <> "/file-" <> n) names
            out = render (map (\p -> ">f+++++++++ " <> Text.pack p) paths)
         in case interpretProbe ExitSuccess out of
                Failure why -> counterexample (show why) (not (any (\p -> Text.pack p `Text.isInfixOf` why) paths))
                other -> counterexample ("expected a Failure, got " <> show other) False

-------------------------------------------------------------------------------
-- Properties: refs.

prop_refIgnoresOptions :: Property
prop_refIgnoresOptions = forAll genCoordinates $ \(rem, src, dst, o) ->
    forAll genOpts $ \o' ->
        rootRef (fetch o rem src dst) === rootRef (fetch o' rem src dst)
            .&&. rootRef (fetch o rem src dst) === Just (mkRef "rsync-receivedir" (src, dst, rem.remoteUser, rem.remoteHost))
            .&&. rootHelp (fetch o rem src dst) === rootHelp (fetch o' rem src dst)

prop_refSeparatesCoordinates :: Property
prop_refSeparatesCoordinates = forAll genCoordinates $ \(rem, src, dst, o) ->
    forAll genCoordinates $ \(rem', src', dst', _) ->
        let base = rootRef (fetch o rem src dst)
         in (src /= src' ==> base =/= rootRef (fetch o rem src' dst))
                .&&. (dst /= dst' ==> base =/= rootRef (fetch o rem src dst'))
                .&&. (rem.remoteUser /= rem'.remoteUser ==> base =/= rootRef (fetch o rem{remoteUser = rem'.remoteUser} src dst))
                .&&. (rem.remoteHost /= rem'.remoteHost ==> base =/= rootRef (fetch o rem{remoteHost = rem'.remoteHost} src dst))

prop_refDistinctFromSiblings :: Property
prop_refDistinctFromSiblings = forAll genCoordinates $ \(rem, src, dst, o) ->
    let received = rootRef (fetch o rem src dst)
     in received =/= rootRef (sendDirWith o silent ignoreTrack mkdirTrack (FS.Directory dst) rem src)
            .&&. received =/= rootRef (receiveFileWith o silent ignoreTrack mkdirTrack rem src dst)

{- | 'sendDir' and 'receiveFile' against their keys and help spelled out
again here, whatever the options.
-}
prop_olderNodesUnchanged :: Property
prop_olderNodesUnchanged = forAll genCoordinates $ \(rem, a, b, o) ->
    let sent = sendDirWith o silent ignoreTrack mkdirTrack (FS.Directory a) rem b
        got = receiveFileWith o silent ignoreTrack mkdirTrack rem a b
        login = rem.remoteUser <> "@" <> rem.remoteHost
     in rootRef sent === Just (mkRef "rsync-senddir" (a, rem.remoteUser, rem.remoteHost))
            .&&. rootHelp sent === Just ("copies " <> Text.pack a <> " to " <> Text.pack b <> " over rsync")
            .&&. rootRef got === Just (mkRef "rsync-receivefile" (a, b, rem.remoteUser, rem.remoteHost))
            .&&. rootHelp got === Just ("copies " <> Text.pack a <> " from " <> login <> " over rsync")

-------------------------------------------------------------------------------
-- A real rsync between two local directories: the module's flags and parser
-- against what rsync really does. No ssh, so nothing about the remote half.

realRsyncTests :: TestTree
realRsyncTests =
    testGroup
        "a real rsync, local paths"
        [ testCase "probe, fetch, probe again, change, probe" $
            requireExecutable "rsync" $
                withTempDir $ \tmp -> do
                    let src = tmp </> "results"
                        dst = tmp </> "in"
                    createDirectoryIfMissing True (src </> "sub")
                    createDirectoryIfMissing True dst
                    writeFile (src </> "a") "a\n"
                    writeFile (src </> "sub" </> "b") "b\n"
                    let probe = readProcessWithExitCode "rsync" (receiveDirFlags <> probeFlags <> [src, dst]) ""
                        verdictOf (code, out, _) = interpretProbe code (C8.pack out)
                    before <- probe
                    assertEqual (show before) (Failure "4 item(s) differ from the remote directory") (verdictOf before)
                    (code, _, err) <- readProcessWithExitCode "rsync" (receiveDirFlags <> [src, dst]) ""
                    assertEqual err ExitSuccess code
                    landed <- readFile (dst </> "results" </> "sub" </> "b")
                    assertEqual "a path without a trailing slash lands under the local directory" "b\n" landed
                    after <- probe
                    assertEqual ("--times is what makes this empty: " <> show after) Success (verdictOf after)
                    writeFile (src </> "a") "a longer a\n"
                    changed <- probe
                    assertEqual (show changed) (Failure "1 item(s) differ from the remote directory") (verdictOf changed)
                    writeFile (dst </> "results" </> "sub" </> "b") "edited locally\n"
                    both <- probe
                    assertEqual (show both) (Failure "2 item(s) differ from the remote directory") (verdictOf both)
        ]

-------------------------------------------------------------------------------
-- The node itself, through an ssh that runs the command locally. Opt-in; see
-- the module header for why and how.

endToEndTests :: TestTree
endToEndTests =
    testGroup
        "the node, end to end"
        [ testCase "check, up, check, change, check, up, check" $ do
            shim <- lookupEnv "SALMON_TEST_RSYNC_SSH_SHIM"
            case shim of
                Nothing ->
                    hPutStrLn stderr "SKIPPED: Rsync.receiveDir end to end needs a local-exec `ssh` shim on PATH and SALMON_TEST_RSYNC_SSH_SHIM=1 (see Test.RsyncSpec)"
                Just _ -> withTempDir $ \tmp -> do
                    let src = tmp </> "results"
                        dst = tmp </> "deep" </> "in"
                        node = receiveDir silent ignoreTrack mkdirTrack (Remote "nobody" "shim.invalid") src (FS.Directory dst)
                    createDirectoryIfMissing True (src </> "sub")
                    writeFile (src </> "a") "a\n"
                    writeFile (src </> "sub" </> "b") "b\n"
                    first <- rootCheck node
                    assertEqual "the local directory does not exist yet" (Failure "5 item(s) differ from the remote directory") first
                    ok <- runUp node
                    assertBool "up" ok
                    made <- doesDirectoryExist dst
                    assertBool "the mkdir track ran" made
                    landed <- readFile (dst </> "results" </> "sub" </> "b")
                    assertEqual "" "b\n" landed
                    second <- rootCheck node
                    assertEqual "nothing changed: nothing to fetch" Success second
                    again <- runUp node
                    assertBool "a second up is fine" again
                    writeFile (src </> "a") "a longer a\n"
                    third <- rootCheck node
                    assertEqual "" (Failure "1 item(s) differ from the remote directory") third
                    ok' <- runUp node
                    assertBool "up again" ok'
                    refreshed <- readFile (dst </> "results" </> "a")
                    assertEqual "" "a longer a\n" refreshed
                    fourth <- rootCheck node
                    assertEqual "" Success fourth
                    gone <- rootCheck (receiveDir silent ignoreTrack mkdirTrack (Remote "nobody" "shim.invalid") (tmp </> "absent") (FS.Directory dst))
                    unless (gone == Unknown) $
                        assertFailure ("a remote directory that is not there: " <> show gone)
        ]
