{-# LANGUAGE OverloadedStrings #-}

{- | The git registry for "Salmon.Actions.Follow": the desired state is a
repository, and the document for a label is a file in it.

@--follow git+\<url\>[#\<branch\>[:\<subdir\>]]@ names it. The repository is
cloned once into a working directory the caller chooses (see
"Salmon.Actions.Follow.Registry" for the default), and every round is a
@git fetch@ followed by a @git reset --hard@ onto what the remote branch
now points at — never a merge, since the checkout is nobody's to edit. The
document for label @L@ is @\<subdir\>/\<L\>.json@ in that checkout, and the
stamp is the commit the branch resolved to: a round that finds the same
commit reads nothing, which is the directory registry's "ask before
reading" with a hash instead of an mtime. A file missing from the commit is
'Absent'; the fetch failing — no network, no such branch, a credential
prompt refused (@GIT_TERMINAL_PROMPT@ is off, so a private repository fails
rather than hangs) — throws and is a failed round.

Everything goes through the @git@ binary as "Salmon.Builtin.Nodes.Git"
does, with 'Binary.untrackedExec' so that a non-zero exit is a throw
carrying git's own stderr, and not a Haskell git library.

The subdirectory has to come after the branch (@#main:hosts@, or @#:hosts@
for the remote's default branch), because a URL has colons of its own —
@ssh://host:22/repo@, @git\@host:repo.git@ — and the spec's
@[#\<branch\>][:\<subdir\>]@ leaves which one is the subdirectory's to
guess.
-}
module Salmon.Actions.Follow.Registry.Git (
    Source (..),
    parseSource,
    renderSource,
    gitRegistry,
    documentPathIn,
) where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getEnvironment)
import System.FilePath ((<.>), (</>))
import System.Process.ListLike (CreateProcess (..), proc)

import Salmon.Actions.Follow (Fetch (..), Label, Registry (..), Stamp (..), digestOf, labelText)
import Salmon.Builtin.Nodes.Binary (Command (..))
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Reporter (silent)

-- | A repository, a branch (the remote's default when 'Nothing') and the
-- subdirectory the documents are under (the root when 'Nothing').
data Source = Source
    { sourceUrl :: Text
    , sourceBranch :: Maybe Text
    , sourceSubdir :: Maybe FilePath
    }
    deriving (Show, Eq)

-- | What follows @git+@: @\<url\>[#\<branch\>[:\<subdir\>]]@.
parseSource :: Text -> Either Text Source
parseSource spec
    | Text.null url = Left ("a git registry needs a URL: git+" <> spec)
    | otherwise = case Text.stripPrefix "#" fragment of
        Nothing -> Right (Source url Nothing Nothing)
        Just rest ->
            let (branch, subdir) = Text.breakOn ":" rest
             in Right
                    ( Source
                        url
                        (if Text.null branch then Nothing else Just branch)
                        (case Text.unpack (Text.drop 1 subdir) of "" -> Nothing; d -> Just d)
                    )
  where
    (url, fragment) = Text.breakOn "#" spec

-- | The address back, as @git+...@: what @history@ and reports name.
renderSource :: Source -> Text
renderSource s =
    "git+"
        <> s.sourceUrl
        <> case (s.sourceBranch, s.sourceSubdir) of
            (Nothing, Nothing) -> ""
            (b, d) -> "#" <> maybe "" id b <> maybe "" (\d' -> ":" <> Text.pack d') d

-- | Where a label's document is in a checkout: @\<subdir\>/\<label\>.json@.
documentPathIn :: FilePath -> Source -> Label -> FilePath
documentPathIn workdir s lbl = maybe workdir (workdir </>) s.sourceSubdir </> Text.unpack (labelText lbl) <.> "json"

{- | A registry over a checkout at @workdir@, made once per process (the
environment is read here, once, to turn off git's credential prompts). -}
gitRegistry :: FilePath -> Source -> IO Registry
gitRegistry workdir source = do
    env <- getEnvironment
    let git = Command (\args -> (proc "git" args){env = Just (("GIT_TERMINAL_PROMPT", "0") : filter ((/= "GIT_TERMINAL_PROMPT") . fst) env)})
        run :: [String] -> IO ()
        run args = Binary.untrackedExec git args "" silent
        -- what the branch resolves to after a fetch, as a full hash
        resolve :: IO Text
        resolve = do
            out <- Binary.untrackedExecOutput git ["-C", workdir, "rev-parse", "--verify", remoteRef] "" silent
            let hash = Text.strip (Text.pack (C8.unpack out))
            if Text.null hash then ioError (userError ("git rev-parse " <> remoteRef <> " answered nothing")) else pure hash
    pure
        Registry
            { registryName = renderSource source
            , registryFetch = \lbl previous -> do
                cloned <- doesDirectoryExist (workdir </> ".git")
                if cloned
                    then run ["-C", workdir, "fetch", "--quiet", "origin"]
                    else do
                        createDirectoryIfMissing True workdir
                        run (["clone", "--quiet"] ++ maybe [] (\b -> ["--branch", Text.unpack b, "--single-branch"]) source.sourceBranch ++ [Text.unpack source.sourceUrl, workdir])
                commit <- resolve
                let stamp = Stamp commit
                if Just stamp == previous
                    then pure Unchanged
                    else do
                        run ["-C", workdir, "reset", "--hard", "--quiet", Text.unpack commit]
                        let path = documentPathIn workdir source lbl
                        present <- doesFileExist path
                        if not present
                            then pure Absent
                            else do
                                bytes <- LByteString.readFile path
                                LByteString.length bytes `seq` pure (Found stamp (digestOf bytes) bytes)
            }
  where
    remoteRef = maybe "origin/HEAD" (\b -> "origin/" <> Text.unpack b) source.sourceBranch
