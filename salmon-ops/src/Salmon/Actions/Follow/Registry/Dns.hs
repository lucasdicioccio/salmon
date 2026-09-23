{-# LANGUAGE OverloadedStrings #-}

{- | The DNS-index registry for "Salmon.Actions.Follow": DNS is the
registry's /index/, HTTP its /storage/ (the spec's recommended shape).

@--follow dns:\<zone\>@ names it. The document for label @L@ is announced by
one @TXT@ record at @\<L\>.\<zone\>@ reading

> v=salmon1 url=<https url> sha256=<hex digest of the document's bytes>

and that record's digest is the stamp: a round is one DNS lookup, and the
URL is fetched only when the digest the record carries is not the one last
seen — one UDP round-trip, cached by the record's TTL, and no connection to
the store at all while nothing changes. The body that comes back is hashed
and compared with the record; a mismatch throws 'IndexMismatch' and is a
failed round with that reason, never applied, because a store serving
something other than what the index announces is either mid-publish or
somebody else's, and neither is a document. No record is 'Absent'; the
lookup failing (no resolver reachable, a @SERVFAIL@) throws.

The resolver is a 'Resolver' — a name and one function — so that a test can
answer lookups itself. The one shipped, 'digResolver', shells out to
@dig +short@ through "Salmon.Builtin.Nodes.Binary" like every other binary
this tree drives: nothing in the tree resolves DNS today, and one @TXT@
lookup was not worth a resolver library's dependency footprint.
-}
module Salmon.Actions.Follow.Registry.Dns (
    Resolver (..),
    digResolver,
    parseDigTxt,
    IndexRecord (..),
    parseIndexRecord,
    recordName,
    dnsRegistry,
    IndexMismatch (..),
) where

import Control.Exception (Exception, throwIO)
import Data.Either (partitionEithers)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client (Manager)
import System.Process.ListLike (proc)

import Salmon.Actions.Follow (Digest (..), Fetch (..), Label, Registry (..), Stamp (..), labelText)
import Salmon.Actions.Follow.Registry.Http (fetchUrl)
import Salmon.Builtin.Nodes.Binary (Command (..))
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Reporter (silent)

-- | Who answers a @TXT@ lookup: every record's strings already joined,
-- one 'Text' per record; @[]@ for a name with none. May throw.
data Resolver = Resolver
    { resolverName :: Text
    , resolveTxt :: Text -> IO [Text]
    }

-- | @dig +short TXT \<name\>@; a non-zero exit (no server reachable) throws.
digResolver :: Resolver
digResolver =
    Resolver
        { resolverName = "dig"
        , resolveTxt = \name -> do
            out <- Binary.untrackedExecOutput dig ["+short", "TXT", Text.unpack name] "" silent
            pure (parseDigTxt (Text.decodeUtf8Lenient out))
        }
  where
    dig = Command (\args -> proc "dig" args)

{- | @dig +short@ prints one record per line as its quoted strings —
@"v=salmon1 url=..." "sha256=..."@ for a record longer than one string —
and the odd @;;@ comment on the way to a non-zero exit. Each line's strings
are unescaped and joined, as the @TXT@ RFC says a reader should.
-}
parseDigTxt :: Text -> [Text]
parseDigTxt = map (Text.concat . strings . Text.unpack) . filter (not . Text.isPrefixOf ";") . filter (not . Text.null) . Text.lines
  where
    strings :: String -> [Text]
    strings s = case dropWhile (/= '"') s of
        [] -> []
        (_ : rest) -> let (str, more) = quoted rest in Text.pack str : strings more
    quoted :: String -> (String, String)
    quoted ('\\' : c : rest) = let (s, more) = quoted rest in (c : s, more)
    quoted ('"' : rest) = ([], rest)
    quoted (c : rest) = let (s, more) = quoted rest in (c : s, more)
    quoted [] = ([], [])

-- | What a @v=salmon1@ record announces.
data IndexRecord = IndexRecord
    { indexUrl :: Text
    , indexDigest :: Digest
    }
    deriving (Show, Eq)

-- | @v=salmon1 url=... sha256=...@, whitespace-separated, in any order after
-- the version; a record for some other version, or one missing either
-- field, is a 'Left' naming what is missing.
parseIndexRecord :: Text -> Either Text IndexRecord
parseIndexRecord txt =
    case Text.words txt of
        ("v=salmon1" : fields) ->
            let pairs = [(k, Text.drop 1 v) | f <- fields, let (k, v) = Text.breakOn "=" f]
             in case (lookup "url" pairs, lookup "sha256" pairs) of
                    (Just url, Just hex)
                        | Text.length hex == 64 && Text.all isHex hex -> Right (IndexRecord url (Digest (Text.toLower hex)))
                        | otherwise -> Left ("sha256= is not a hex sha256 digest: " <> hex)
                    (Nothing, _) -> Left "no url= in the record"
                    (_, Nothing) -> Left "no sha256= in the record"
        _ -> Left ("not a v=salmon1 record: " <> txt)
  where
    isHex c = c `elem` ("0123456789abcdefABCDEF" :: String)

-- | The name looked up for a label: @\<label\>.\<zone\>@.
recordName :: Text -> Label -> Text
recordName zone lbl = labelText lbl <> "." <> Text.dropWhileEnd (== '.') zone

-- | The index said one thing and the store served another.
data IndexMismatch = IndexMismatch
    { mismatchName :: Text
    , mismatchUrl :: Text
    , mismatchAnnounced :: Digest
    , mismatchServed :: Digest
    }

instance Show IndexMismatch where
    show m =
        Text.unpack $
            "the document at "
                <> m.mismatchUrl
                <> " does not hash to what the index record "
                <> m.mismatchName
                <> " announces (record: sha256="
                <> Text.take 12 m.mismatchAnnounced.unDigest
                <> ", served: sha256="
                <> Text.take 12 m.mismatchServed.unDigest
                <> ")"

instance Exception IndexMismatch

-- | A registry over a zone, named @dns:\<zone\>@.
dnsRegistry :: Resolver -> Manager -> Text -> Registry
dnsRegistry resolver mgr zone =
    Registry
        { registryName = "dns:" <> zone
        , registryFetch = \lbl previous -> do
            let name = recordName zone lbl
            txts <- resolveTxt resolver name
            if null txts
                then pure Absent
                else case partitionEithers (map parseIndexRecord txts) of
                    (errs, []) -> ioError (userError (Text.unpack (name <> " has " <> Text.pack (show (length txts)) <> " TXT record(s) and none is a salmon index: " <> Text.intercalate "; " errs)))
                    (_, record : _) -> do
                        let stamp = Stamp ("sha256:" <> record.indexDigest.unDigest)
                        if Just stamp == previous
                            then pure Unchanged
                            else do
                                -- unconditional: the record already said it moved
                                fetched <- fetchUrl mgr record.indexUrl Nothing
                                case fetched of
                                    Found _ digest bytes
                                        | digest == record.indexDigest -> pure (Found stamp digest bytes)
                                        | otherwise -> throwIO (IndexMismatch name record.indexUrl record.indexDigest digest)
                                    Absent -> ioError (userError (Text.unpack (name <> " points at " <> record.indexUrl <> ", which has no document")))
                                    Unchanged -> ioError (userError (Text.unpack (record.indexUrl <> " answered 304 to an unconditional request")))
        }
