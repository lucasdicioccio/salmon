{-# LANGUAGE OverloadedStrings #-}

{- | Layer 1 coverage for "Salmon.Builtin.Nodes.Filesystem"'s @check@.

'FS.checkFileContents' is the second real check in the tree (after
'Salmon.Builtin.Nodes.Systemd.checkService') and the one that reaches the
most graphs, since nearly every recipe writes a config file. These run
against a throwaway temp directory rather than a fake filesystem, because
what the check actually asserts — "the bytes on disk are these bytes" — is
not a thing a fake can be wrong about in the interesting way.

Two of the cases below are about consequences rather than the check itself.
'skippedFileKeepsItsMtime' is the one 'Salmon.Builtin.Nodes.Systemd' depends
on: systemd decides a unit needs reloading from its file's mtime, so a node
that rewrote identical bytes on every pass made every pass look like a
changed unit. And 'redeclaredContentsAreRewritten' is the shape of (I6) —
the same path declared with different contents — which is the case a check
is the only thing that can notice.
-}
module Test.FilesystemSpec (tests) where

import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Bits ((.&.))
import System.Directory (
    copyFile,
    createDirectory,
    createDirectoryLink,
    getHomeDirectory,
    renameDirectory,
    createDirectoryIfMissing,
    createFileLink,
    doesDirectoryExist,
    doesFileExist,
    getModificationTime,
    pathIsSymbolicLink,
    removeFile,
 )
import System.FilePath (takeDirectory, (</>))
import qualified System.Posix.Files as Posix
import qualified System.Posix.User as PosixUser
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (CheckResult (..), Report (..))
import Salmon.Builtin.Extension (Extension, Op, opAct)
import Salmon.Builtin.Extension (help, notes, ref)
import Salmon.Op.Actions (extension, shorthand)
import Salmon.Op.Ref (mkRef)
import qualified Salmon.Builtin.Nodes.Filesystem as FS

import Test.Harness (runDown, runUp, runUpCapturing, withTempDir)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Filesystem"
        [ testCase "a file that is not there yet needs writing" missingIsFailure
        , testCase "a file with the right bytes is satisfied" matchingIsSuccess
        , testCase "a file of the right length but the wrong bytes is not" sameSizeIsNotEnough
        , testCase "the reason never quotes the contents" reasonKeepsSecrets
        , testCase "a second pass skips the file instead of rewriting it" secondPassSkips
        , testCase "and so leaves its mtime alone, which systemd reads" skippedFileKeepsItsMtime
        , testCase "a clobbered file is written again" clobberedIsRewritten
        , testCase "a deleted file is written again" deletedIsRewritten
        , testCase "the same path redeclared with new contents is rewritten" redeclaredContentsAreRewritten
        , testGroup
            "down tolerates an effect that was never created"
            [ testCase "a file already gone is not a teardown failure" downOfAbsentFileSucceeds
            , testCase "a directory already gone is not a teardown failure" downOfAbsentDirSucceeds
            , testCase "a file's down still takes the enclosing directory with it" downRemovesBoth
            , testCase "but a directory holding something undeclared still fails" downOfNonEmptyDirFails
            ]
        , testGroup
            "ownedFile: recursive tree ownership"
            [ testCase "a missing directory is a Failure, same as a missing file" ownedMissingDirIsFailure
            , testCase "a directory is checkable at all (not doesFileExist's permanent 'missing')" ownedDirectoryIsCheckable
            , testCase "up recurses into nested files and subdirectories" ownedUpChownsDescendants
            , testCase "ownedMode is applied to the top entry only, never descendants" ownedModeNotAppliedToDescendants
            , testCase "a dangling symlink inside the tree is chowned itself, never followed" ownedTreeDoesNotFollowSymlinks
            ]
        , testGroup
            "scratchDir: down removes the tree, guarded by identity"
            [ testCase "up, a file inside, down: gone, and a second down also succeeds" scratchAcceptance
            , testCase "check: missing before up, Success after, and a second up is a no-op" scratchCheckAndIdempotentUp
            , testCase "a directory salmon did not create is never taken over nor removed" scratchRefusesPreExisting
            , testCase "a claim copied into a recreated directory does not match" scratchRefusesCopiedClaim
            , testCase "a symlink at the path is refused, its target untouched" scratchRefusesSymlinkPath
            , testCase "a symlink inside the tree is removed, never followed" scratchDoesNotFollowInnerSymlinks
            , testCase "a deleted claim makes down refuse" scratchRefusesMissingClaim
            , testCase "root, top-level, relative and dotted paths are refused" scratchPathRefusals
            , testCase "the home directory and its ancestors are refused" scratchRefusesHome
            , testCase "dir's rendering is unchanged by the new variant" dirRenderingUnchanged
            ]
        ]

-- | The node under test, and the check it carries, for one path/contents pair.
contentsAt :: FilePath -> Text -> FS.FileContents Text
contentsAt path body = FS.FileContents path body

checkOf :: FS.FileContents Text -> IO CheckResult
checkOf = FS.checkFileContents

nodeOf :: FS.FileContents Text -> IO Bool
nodeOf = runUp . FS.filecontents

-------------------------------------------------------------------------------

missingIsFailure :: IO ()
missingIsFailure = withTempDir $ \d -> do
    verdict <- checkOf (contentsAt (d </> "absent.conf") "hello\n")
    assertBool "missing is a Failure" (isFailure verdict)

matchingIsSuccess :: IO ()
matchingIsSuccess = withTempDir $ \d -> do
    let want = contentsAt (d </> "a.conf") "hello\n"
    assertBool "up succeeded" =<< nodeOf want
    assertEqual "the bytes match" Success =<< checkOf want

{- | The whole reason this is a byte comparison and not
'Salmon.Actions.UpDown.skipIfFileExists': a file of exactly the right length
holding the wrong thing is the failure mode a config node most needs to
catch, and it is the one an existence check calls satisfied. It also pins
that the size comparison is a fast path in front of the answer rather than
the answer.
-}
sameSizeIsNotEnough :: IO ()
sameSizeIsNotEnough = withTempDir $ \d -> do
    let path = d </> "same-size.conf"
    let want = contentsAt path "aaaaa\n"
    assertBool "up succeeded" =<< nodeOf want
    writeFile path "bbbbb\n"
    verdict <- checkOf want
    assertBool "same length, different bytes, still a Failure" (isFailure verdict)

{- | Failure text goes into reports. The files this node writes include
pgbouncer userlists and postgrest configurations with signing keys in them,
so the reason says which file and never what is in it.
-}
reasonKeepsSecrets :: IO ()
reasonKeepsSecrets = withTempDir $ \d -> do
    let path = d </> "secret.conf"
    let want = contentsAt path "password = hunter2\n"
    assertBool "up succeeded" =<< nodeOf want
    writeFile path "password = swordfish\n"
    verdict <- checkOf want
    case verdict of
        Failure why -> do
            assertBool "names the file" (C8.pack path `C8.isInfixOf` C8.pack (show why))
            assertBool "not what we would write" (not ("hunter2" `substr` why))
            assertBool "nor what is there" (not ("swordfish" `substr` why))
        other -> fail ("expected a Failure, got " <> show other)

{- | The behaviour change this check lands on every existing caller: a
'filecontents' node whose bytes already match stops being re-applied.
-}
secondPassSkips :: IO ()
secondPassSkips = withTempDir $ \d -> do
    let want = contentsAt (d </> "twice.conf") "hello\n"
    first <- runUpCapturing (FS.filecontents want)
    assertEqual "written the first time" ["file-contents"] (evals first)
    second <- runUpCapturing (FS.filecontents want)
    assertEqual "skipped the second time" [] (evals second)
    assertEqual "and reported as a skip" ["file-contents"] (skips second)

{- | The consequence 'Salmon.Builtin.Nodes.Systemd.checkService' was waiting
for. systemd answers @NeedDaemonReload@ from the unit file's mtime, so a node
that rewrote byte-identical contents on every pass reported a changed unit on
every pass — and the unit check would then reload and restart a service with
nothing wrong with it. Verified against a real @systemctl --user@ unit while
writing this: rewriting identical bytes flips @NeedDaemonReload@ to @yes@.
-}
skippedFileKeepsItsMtime :: IO ()
skippedFileKeepsItsMtime = withTempDir $ \d -> do
    let path = d </> "unit.service"
    let want = contentsAt path "[Service]\nExecStart=/bin/true\n"
    assertBool "up succeeded" =<< nodeOf want
    before <- getModificationTime path
    assertBool "second up succeeded" =<< nodeOf want
    after <- getModificationTime path
    assertEqual "the file was not touched at all" before after

clobberedIsRewritten :: IO ()
clobberedIsRewritten = withTempDir $ \d -> do
    let path = d </> "clobbered.conf"
    let want = contentsAt path "hello\n"
    assertBool "up succeeded" =<< nodeOf want
    writeFile path "something else entirely\n"
    reports <- runUpCapturing (FS.filecontents want)
    assertEqual "written again" ["file-contents"] (evals reports)
    assertEqual "and back to what it should say" "hello\n" =<< readFile path

deletedIsRewritten :: IO ()
deletedIsRewritten = withTempDir $ \d -> do
    let path = d </> "deleted.conf"
    let want = contentsAt path "hello\n"
    assertBool "up succeeded" =<< nodeOf want
    removeFile path
    reports <- runUpCapturing (FS.filecontents want)
    assertEqual "written again" ["file-contents"] (evals reports)

{- | (I6)'s shape, from the node's end. Two declarations of one path with
different contents are one 'Salmon.Op.Ref.Ref' — @filecontents@ keys on the
path alone — so nothing above the node can tell they differ. Only the check
can, and now it does.
-}
redeclaredContentsAreRewritten :: IO ()
redeclaredContentsAreRewritten = withTempDir $ \d -> do
    let path = d </> "redeclared.conf"
    assertBool "the first declaration went up" =<< nodeOf (contentsAt path "first\n")
    reports <- runUpCapturing (FS.filecontents (contentsAt path "second\n"))
    assertEqual "the second was evaluated, not skipped" ["file-contents"] (evals reports)
    assertEqual "and the file says the new thing" "second\n" =<< readFile path

{- | A @down@ that throws is contained by marking every /predecessor/
'Salmon.Actions.UpDown.Blocked', so a node whose effect was never created
blocks the teardown of everything it was declared on top of. Absence is this
node's effect being absent, and teardown never consults a node's check (that
answers "does my effect need creating"), so tolerating it is the node's own
business. Found by a GCP sandbox that could not remove its working directory
because an earlier teardown had already removed the file inside it.
-}
downOfAbsentFileSucceeds :: IO ()
downOfAbsentFileSucceeds = withTempDir $ \d -> do
    let path = d </> "never-written.conf"
    assertBool "tearing down a file that was never written succeeds" =<< runDown (FS.filecontents (contentsAt path "unused\n"))

downOfAbsentDirSucceeds :: IO ()
downOfAbsentDirSucceeds = withTempDir $ \d -> do
    let path = d </> "never-created"
    assertBool "tearing down a directory that was never created succeeds" =<< runDown (FS.dir (FS.Directory path))

-- | Tolerating absence must not turn `down` into a no-op.
downRemovesBoth :: IO ()
downRemovesBoth = withTempDir $ \d -> do
    let dir = d </> "workdir"
    let path = dir </> "written.conf"
    assertBool "the node went up" =<< nodeOf (contentsAt path "body\n")
    assertBool "the file is there" =<< doesFileExist path
    assertBool "teardown succeeded" =<< runDown (FS.filecontents (contentsAt path "body\n"))
    assertEqual "the file is gone" False =<< doesFileExist path
    assertEqual "and so is the directory it brought with it" False =<< doesDirectoryExist dir

{- | The signal worth keeping: something in the directory was never declared
(or did not go down), which is exactly the case that caught 'Podman.login'
leaving its authfile behind.
-}
downOfNonEmptyDirFails :: IO ()
downOfNonEmptyDirFails = withTempDir $ \d -> do
    let dir = d </> "occupied"
    assertBool "the directory went up" =<< runUp (FS.dir (FS.Directory dir))
    writeFile (dir </> "undeclared.txt") "left behind\n"
    assertEqual "teardown of a non-empty directory fails" False =<< runDown (FS.dir (FS.Directory dir))
    assertBool "and leaves it standing" =<< doesDirectoryExist dir

-------------------------------------------------------------------------------

{- | The current process's own username\/group, so tests can declare "this
tree is owned by me" -- a chown any unprivileged test process is always
allowed to perform on files it already owns -- without needing root.
-}
selfOwnership :: IO (Text, Text)
selfOwnership = do
    user <- Text.pack <$> PosixUser.getEffectiveUserName
    gid <- PosixUser.getEffectiveGroupID
    group <- Text.pack . PosixUser.groupName <$> PosixUser.getGroupEntryForID gid
    pure (user, group)

ownedMissingDirIsFailure :: IO ()
ownedMissingDirIsFailure = withTempDir $ \d -> do
    (user, group) <- selfOwnership
    let owner = FS.FileOwnership (d </> "never-made") (Just user) (Just group) 0o755
    verdict <- FS.checkOwnership owner
    assertBool "missing directory is a Failure" (isFailure verdict)

{- | Before (I5)'s fix this used 'doesFileExist', which is 'False' for a
directory -- so a directory-shaped 'ownedFile' reported permanently
"missing", however many times 'up' ran.
-}
ownedDirectoryIsCheckable :: IO ()
ownedDirectoryIsCheckable = withTempDir $ \d -> do
    (user, group) <- selfOwnership
    let path = d </> "handed-over"
    createDirectory path
    let owner = FS.FileOwnership path (Just user) (Just group) 0o755
    assertBool "up succeeded" =<< runUp (FS.ownedFile owner)
    assertEqual "the directory now checks as Success" Success =<< FS.checkOwnership owner

{- | A rootfs handed to an unprivileged user has package-installed
subdirectories (etc\/ssh\/sshd_config.d, say) that only the top-level chown
used to reach. 'up' must walk the whole tree.
-}
ownedUpChownsDescendants :: IO ()
ownedUpChownsDescendants = withTempDir $ \d -> do
    (user, group) <- selfOwnership
    let top = d </> "rootfs-etc-ssh"
    let sub = top </> "sshd_config.d"
    createDirectoryIfMissing True sub
    writeFile (sub </> "99-salmon-test.conf") "# nothing\n"
    let owner = FS.FileOwnership top (Just user) (Just group) 0o755
    assertBool "up succeeded" =<< runUp (FS.ownedFile owner)
    assertEqual "the whole tree now checks as Success" Success =<< FS.checkOwnership owner

-- | 'ownedMode' is a statement about the directory entry itself, not a
-- recursive chmod -- a config file wanting 0644 and a host key wanting
-- 0600 underneath the same handed-over directory must not both end up at
-- whatever single mode the caller gave the top of the tree.
ownedModeNotAppliedToDescendants :: IO ()
ownedModeNotAppliedToDescendants = withTempDir $ \d -> do
    (user, group) <- selfOwnership
    let top = d </> "mode-scoped"
    let nested = top </> "keep-this-mode.conf"
    createDirectory top
    writeFile nested "unchanged\n"
    Posix.setFileMode nested 0o600
    let owner = FS.FileOwnership top (Just user) (Just group) 0o755
    assertBool "up succeeded" =<< runUp (FS.ownedFile owner)
    nestedMode <- (.&. 0o7777) . Posix.fileMode <$> Posix.getFileStatus nested
    assertEqual "the nested file's own mode survived untouched" 0o600 nestedMode

{- | A symlink under a handed-over tree is chowned itself
('setSymbolicLinkOwnerAndGroup'); its target is somebody else's business,
and a dangling one must not make the walk throw trying to stat through it.
-}
ownedTreeDoesNotFollowSymlinks :: IO ()
ownedTreeDoesNotFollowSymlinks = withTempDir $ \d -> do
    (user, group) <- selfOwnership
    let top = d </> "with-a-symlink"
    createDirectory top
    let link = top </> "dangling"
    createFileLink "/nonexistent-salmon-test-target" link
    let owner = FS.FileOwnership top (Just user) (Just group) 0o755
    assertBool "up succeeded despite the dangling symlink" =<< runUp (FS.ownedFile owner)
    assertBool "the symlink is still a symlink, not resolved/replaced" =<< pathIsSymbolicLink link
    assertEqual "and the tree checks as Success" Success =<< FS.checkOwnership owner

-------------------------------------------------------------------------------

isFailure :: CheckResult -> Bool
isFailure (Failure _) = True
isFailure _ = False

substr :: Text -> Text -> Bool
substr needle hay = C8.pack (show needle) `C8.isInfixOf` C8.pack (show hay)

-- | Only the file node: 'FS.filecontents' brings its enclosing directory
-- with it, and that one has no check of its own.
evals :: [Report Extension] -> [Text]
evals rs = [shorthand act | Eval act <- rs, shorthand act == "file-contents"]

skips :: [Report Extension] -> [Text]
skips rs = [shorthand act | Skip act <- rs, shorthand act == "file-contents"]

-------------------------------------------------------------------------------

scratchAt :: FilePath -> Op
scratchAt = FS.scratchDir . FS.Directory

-- | The customer's acceptance criterion, verbatim.
scratchAcceptance :: IO ()
scratchAcceptance = withTempDir $ \d -> do
    let dir = d </> "rig" </> "logs"
    assertBool "up succeeded" =<< runUp (scratchAt dir)
    createDirectoryIfMissing True (dir </> "nested")
    writeFile (dir </> "child.log") "written by a child process\n"
    writeFile (dir </> "nested" </> "app.pid") "4242\n"
    assertBool "down succeeded" =<< runDown (scratchAt dir)
    assertEqual "the directory is gone" False =<< doesDirectoryExist dir
    assertBool "its parent stays" =<< doesDirectoryExist (d </> "rig")
    assertBool "a second down also succeeds" =<< runDown (scratchAt dir)

scratchCheckAndIdempotentUp :: IO ()
scratchCheckAndIdempotentUp = withTempDir $ \d -> do
    let dir = d </> "scratch"
    before <- FS.checkScratchDir dir
    assertBool ("missing before up: " <> show before) (isFailure before)
    assertBool "up" =<< runUp (scratchAt dir)
    writeFile (dir </> "kept.log") "x\n"
    assertEqual "Success after up" Success =<< FS.checkScratchDir dir
    assertBool "a second up succeeds" =<< runUp (scratchAt dir)
    assertBool "and leaves the contents alone" =<< doesFileExist (dir </> "kept.log")

scratchRefusesPreExisting :: IO ()
scratchRefusesPreExisting = withTempDir $ \d -> do
    let dir = d </> "theirs"
    createDirectory dir
    writeFile (dir </> "precious") "not salmon's\n"
    assertEqual "up refuses to adopt it" False =<< runUp (scratchAt dir)
    assertEqual "down refuses to remove it" False =<< runDown (scratchAt dir)
    assertBool "the contents survive" =<< doesFileExist (dir </> "precious")
    assertEqual "and no claim was written" False =<< doesFileExist (dir </> FS.scratchClaimFile)

scratchRefusesCopiedClaim :: IO ()
scratchRefusesCopiedClaim = withTempDir $ \d -> do
    let dir = d </> "scratch"
    assertBool "up" =<< runUp (scratchAt dir)
    renameDirectory dir (d </> "moved-away")
    createDirectory dir
    copyFile (d </> "moved-away" </> FS.scratchClaimFile) (dir </> FS.scratchClaimFile)
    writeFile (dir </> "precious") "someone else's\n"
    assertEqual "down refuses: same path, same claim text, another inode" False =<< runDown (scratchAt dir)
    assertBool "the contents survive" =<< doesFileExist (dir </> "precious")

scratchRefusesSymlinkPath :: IO ()
scratchRefusesSymlinkPath = withTempDir $ \d -> do
    let target = d </> "target"
    let link = d </> "link"
    createDirectory target
    writeFile (target </> "precious") "x\n"
    createDirectoryLink target link
    assertEqual "up refuses a symlink" False =<< runUp (scratchAt link)
    assertEqual "down refuses a symlink" False =<< runDown (scratchAt link)
    assertBool "the target survives" =<< doesFileExist (target </> "precious")
    -- a dangling symlink is not absence either
    let dangling = d </> "dangling"
    createDirectoryLink (d </> "nowhere") dangling
    assertEqual "up refuses a dangling symlink" False =<< runUp (scratchAt dangling)
    assertBool "which stays a symlink" =<< pathIsSymbolicLink dangling

scratchDoesNotFollowInnerSymlinks :: IO ()
scratchDoesNotFollowInnerSymlinks = withTempDir $ \d -> do
    let outside = d </> "outside"
    let dir = d </> "scratch"
    createDirectory outside
    writeFile (outside </> "precious") "x\n"
    assertBool "up" =<< runUp (scratchAt dir)
    createDirectoryLink outside (dir </> "points-out")
    assertBool "down" =<< runDown (scratchAt dir)
    assertEqual "the tree is gone" False =<< doesDirectoryExist dir
    assertBool "the symlink's target survives" =<< doesFileExist (outside </> "precious")

scratchRefusesMissingClaim :: IO ()
scratchRefusesMissingClaim = withTempDir $ \d -> do
    let dir = d </> "scratch"
    assertBool "up" =<< runUp (scratchAt dir)
    removeFile (dir </> FS.scratchClaimFile)
    writeFile (dir </> "something") "x\n"
    assertEqual "down refuses" False =<< runDown (scratchAt dir)
    assertBool "and deletes nothing" =<< doesFileExist (dir </> "something")

scratchPathRefusals :: IO ()
scratchPathRefusals = do
    let refused p = assertBool ("refused: " <> p) (FS.scratchPathRefusal p /= Nothing)
    mapM_ refused ["/", "/tmp", "/tmp/", "tmp/x", "", "/tmp/../etc/x", "/tmp/./x"]
    assertEqual "a nested absolute path is fine" Nothing (FS.scratchPathRefusal "/tmp/x")
    assertEqual "a deeper one too" Nothing (FS.scratchPathRefusal "/var/lib/rig/logs")

scratchRefusesHome :: IO ()
scratchRefusesHome = do
    home <- getHomeDirectory
    r <- FS.scratchPathRefusalIO home
    assertBool ("home refused: " <> home) (r /= Nothing)
    case FS.scratchPathRefusal home of
        Just _ -> pure () -- e.g. /root: already refused as top-level
        Nothing -> assertBool "home's parent refused" . (/= Nothing) =<< FS.scratchPathRefusalIO (takeDirectory home)

-- | Pins what 'FS.dir' renders, so adding a variant never changes it (a
-- changed representative restarts what stands on it).
dirRenderingUnchanged :: IO ()
dirRenderingUnchanged =
    case opAct (FS.dir (FS.Directory "/srv/x")) of
        Nothing -> assertBool "dir has an action" False
        Just act -> do
            assertEqual "shorthand" "directory" (shorthand act)
            assertEqual "help" "ensures /srv/x exists, including subdirs" (help (extension act))
            assertEqual "ref" (mkRef "directory" ("/srv/x" :: FilePath)) (ref (extension act))
            assertEqual
                "notes"
                [ "create dir recursively"
                , "does not delete contents of the directory"
                , "reapplies rather than parking under supervision; see supReapply"
                ]
                (notes (extension act))
