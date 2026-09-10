{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Salmon.Builtin.Nodes.Filesystem where

import Salmon.Builtin.Extension
import Salmon.Op.Ref

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBytestring
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.TypeLits (Symbol)
import Salmon.Actions.UpDown (CheckResult (..), skipIfDirectoryIsMissing)
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Supervision (defaultSupervision, supReapply, supervised)
import Salmon.Op.Track
import System.Directory
import System.FilePath

newtype Directory = Directory {directoryPath :: FilePath}
    deriving (Eq, Ord, Show)

{- | (R9). No 'check', by design rather than by omission: there is nothing
about a directory's existence worth a separate question, since
'createDirectoryIfMissing' already costs about what 'doesDirectoryExist'
would. So this declares 'Salmon.Op.Supervision.supReapply' instead — under
@run serve@ a tending machine for this node re-runs @up@ on the adaptive
delay rather than parking, which is what makes a directory removed behind
salmon's back come back on its own. Under a one-shot @run up@\/@run down@
this changes nothing at all: the field is read only by
"Salmon.Actions.Upkeep", and the check still answers
'Salmon.Actions.UpDown.Immaterial' either way.

This is the node 'Salmon.Op.Supervision.supReapply' was written for — see
its haddock for why almost nothing else in this tree should set it.
-}
dir :: Directory -> Op
dir directory =
    op "directory" nodeps $ \actions ->
        actions
            { help = Text.pack $ "ensures " <> path <> " exists, including subdirs"
            , notes =
                [ "create dir recursively"
                , "does not delete contents of the directory"
                , "reapplies rather than parking under supervision; see supReapply"
                ]
            , ref = mkRef "directory" path
            , up = createDirectoryIfMissing True path
            , down = removeDirectory path
            , dynamics = [supervised defaultSupervision{supReapply = True}]
            }
  where
    path :: FilePath
    path = directory.directoryPath

-------------------------------------------------------------------------------

{- | Some file contents that get set once.

Default behaviour is to delete the file on down action
-}
data FileContents a = FileContents {filePath :: FilePath, contents :: a}
    deriving (Eq, Ord, Show, Functor)

filecontents :: (EncodeFileContents a) => FileContents a -> Op
filecontents fcontents =
    op "file-contents" (deps [enclosingdir]) $ \actions ->
        actions
            { help = Text.pack $ "writes " <> path <> " with some contents"
            , notes =
                [ "depends on the enclosing directory"
                ]
            , ref = mkRef "file-contents" path
            , check = checkFileContents fcontents
            , up = ByteString.writeFile path =<< encodeFileContents fcontents.contents
            , down = removeFile path
            }
  where
    enclosingdir :: Op
    enclosingdir = dir (Directory $ takeDirectory path)

    path :: FilePath
    path = fcontents.filePath

{- | Are the bytes on disk already the bytes this node would write?

The second builtin to get a real @check@, after
'Salmon.Builtin.Nodes.Systemd.checkService', and the one that reaches the
most graphs: nearly every recipe here writes a config file. Two things it
buys that are worth separating.

Under a one-shot @run up@ it is an /optimisation with a visible consequence/:
a file whose contents already match is 'Salmon.Actions.UpDown.Skipped', so
its mtime stops moving. That is not cosmetic downstream —
'Salmon.Builtin.Nodes.Systemd.systemdService' writes its unit file through
this node and then asks systemd whether the unit needs reloading, and
systemd answers that from the file's mtime. Rewriting identical bytes every
pass therefore made @NeedDaemonReload@ true every pass, which made
@checkService@ say 'Salmon.Actions.UpDown.Failure' every pass, which
reloaded and restarted a perfectly healthy service. The unit check could not
deliver what it promised until this one existed.

Under @run serve@ it is what makes a config file /supervised/: a
'Salmon.Actions.UpDown.Immaterial' node is parked and never looks again,
where this one notices the file being edited, truncated or deleted behind
salmon's back and puts it back. It is also the answer to a re-declaration
that changes a node's contents without changing its 'Salmon.Op.Ref.Ref' —
the convergence pass still records that node as converged and skips it (see
(I6) in @specs/per-node-state-machines-remaining.md@), and the tending
machine's check is the only thing that then notices the new content.

Comparing bytes rather than mere existence is deliberate:
'Salmon.Actions.UpDown.skipIfFileExists' would call a file with the wrong
contents satisfied, which is the failure mode this node most needs to avoid.
The comparison is cheap in the sense that matters — the node's contents are
already in hand, since 'up' is about to encode them anyway.

Three details:

* __The size is compared first__, and a mismatch answers without reading the
  file. It is one @stat@, and it bounds what a node holding a few hundred
  bytes will read if something else has clobbered its path with something
  enormous.
* __The reason never quotes the contents.__ Failure text goes into reports,
  and the files this node writes include @pgbouncer@ userlists and
  @postgrest@ configurations with signing keys in them.
* __Contents are all it answers about__, because contents are all 'up' sets.
  A file whose mode somebody changed still matches; nothing here ever set
  the mode, so there is nothing to restore.

One hazard, for the @'EncodeFileContents' (IO a)@ instance only: the check
runs the encoder, so a generator with side effects runs once more per look,
and one that is not deterministic (a timestamp) makes this always answer
'Salmon.Actions.UpDown.Failure' and rewrite the file on every pass. That is
the safe direction rather than a correctness problem, but a node built that
way should either be given a stable encoder or set its own 'check'.
-}
checkFileContents :: (EncodeFileContents a) => FileContents a -> IO CheckResult
checkFileContents fcontents = do
    exists <- doesFileExist path
    if not exists
        then pure (Failure ("missing: " <> Text.pack path))
        else do
            wanted <- encodeFileContents fcontents.contents
            size <- getFileSize path
            if size /= fromIntegral (ByteString.length wanted)
                then pure (Failure ("wrong size: " <> Text.pack path))
                else do
                    there <- ByteString.readFile path
                    pure $
                        if there == wanted
                            then Success
                            else Failure ("contents differ: " <> Text.pack path)
  where
    path :: FilePath
    path = fcontents.filePath

{- | Utility class to write various file contents.
The Text instance encodes contents in UTF8.
-}
class EncodeFileContents a where
    encodeFileContents :: a -> IO ByteString.ByteString

instance EncodeFileContents Text.Text where
    encodeFileContents = pure . Text.encodeUtf8

instance EncodeFileContents ByteString.ByteString where
    encodeFileContents = pure . id

instance EncodeFileContents String where
    encodeFileContents = pure . C8.pack

instance EncodeFileContents Aeson.Value where
    encodeFileContents = pure . LBytestring.toStrict . Aeson.encode

instance (EncodeFileContents a) => EncodeFileContents (IO a) where
    encodeFileContents ioX = ioX >>= encodeFileContents

-------------------------------------------------------------------------------

fileCopy :: FilePath -> FilePath -> Op
fileCopy src tgt =
    op "file-copy" (deps [enclosingdir]) $ \actions ->
        actions
            { help = Text.pack $ "copies " <> src <> " " <> tgt
            , ref = mkRef "file-copy" (src, tgt)
            , up = copyFile src tgt
            , down = removeFile tgt
            }
  where
    enclosingdir :: Op
    enclosingdir = dir (Directory $ takeDirectory tgt)

-------------------------------------------------------------------------------
moveDirectory :: FilePath -> FilePath -> (Extension -> Extension) -> Op
moveDirectory src tgt modActions =
    op "move-dir" (deps [enclosingdir]) $ \actions ->
        modActions $
            actions
                { help = Text.pack $ "moves " <> src <> " " <> tgt
                , ref = mkRef "move-dir" (src, tgt)
                , up = renameDirectory src tgt
                }
  where
    enclosingdir :: Op
    enclosingdir = dir (Directory $ takeDirectory tgt)

-------------------------------------------------------------------------------
replaceDirectory :: FilePath -> FilePath -> FilePath -> Op
replaceDirectory src tgt trash =
    op "replace-dir" (deps [delete3 `inject` move2 `inject` move1]) $ \actions ->
        actions
            { help = Text.pack $ "replace " <> src <> " " <> tgt
            , ref = mkRef "replace-dir" (src, tgt)
            }
  where
    move1 :: Op
    move1 = moveDirectory tgt trash $ \actions ->
        actions{check = skipIfDirectoryIsMissing tgt}
    move2 :: Op
    move2 = moveDirectory src tgt id
    delete3 :: Op
    delete3 = destroyDirectory trash

-------------------------------------------------------------------------------
destroyDirectory :: FilePath -> Op
destroyDirectory trash =
    op "delete-dir" nodeps $ \actions ->
        actions
            { help = Text.pack $ "recursively trashes " <> trash
            , ref = mkRef "delete-dir" trash
            , up = removeDirectoryRecursive trash
            , check = skipIfDirectoryIsMissing trash
            }

-------------------------------------------------------------------------------

data File (sym :: Symbol)
    = PreExisting FilePath
    | Generated (Track' FilePath) FilePath

getFilePath :: File a -> FilePath
getFilePath (PreExisting path) = path
getFilePath (Generated _ path) = path

fileOp :: File a -> Op
fileOp (PreExisting path) = placeholder "pre-existing-file" (Text.pack path)
fileOp (Generated t path) = run t path

withFile :: File a -> (FilePath -> Op) -> Op
withFile file@(PreExisting path) f = f path `inject` fileOp file
withFile (Generated mkp path) f = tracking mkp (\x -> (x, x)) path f

generateFileContents :: (EncodeFileContents a) => a -> FilePath -> File b
generateFileContents c path =
    Generated (Track $ \_ -> filecontents $ FileContents path c) path

-------------------------------------------------------------------------------

-- | A line to ensure is present in a file, appending it if missing.
data AppendLineIfMissing = AppendLineIfMissing {appendLineFilePath :: FilePath, appendLineText :: Text.Text}

{- | Idempotent append: ensures a line is present in a file, appending it if
not already there verbatim (@grep -qxF ... || echo ... >>@, done in-process
rather than via a shell) — the same "append-if-missing" shape used for
@pg_hba.conf@ lines (see @Salmon.Builtin.Nodes.Postgres.ensureHbaLineScript@),
generalized to any file. Does not truncate or otherwise touch the file if the
line is already present. Creates the enclosing directory but not the file
itself (an absent file is treated as empty, and the append creates it).
-}
appendLineIfMissing :: AppendLineIfMissing -> Op
appendLineIfMissing item =
    op "append-line-if-missing" (deps [enclosingdir]) $ \actions ->
        actions
            { help = Text.pack $ "ensures a line is present in " <> path
            , notes = ["append-if-missing", "does not truncate or delete existing lines"]
            , ref = mkRef "append-line-if-missing" (path, item.appendLineText)
            , up = ensureLine
            }
  where
    path :: FilePath
    path = item.appendLineFilePath

    enclosingdir :: Op
    enclosingdir = dir (Directory $ takeDirectory path)

    ensureLine :: IO ()
    ensureLine = do
        exists <- doesFileExist path
        contents <- if exists then Text.decodeUtf8 <$> ByteString.readFile path else pure ""
        if item.appendLineText `elem` Text.lines contents
            then pure ()
            else ByteString.appendFile path (Text.encodeUtf8 $ item.appendLineText <> "\n")
