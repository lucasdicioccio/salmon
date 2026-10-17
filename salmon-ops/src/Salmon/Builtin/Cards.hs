{- | Web UI card templates for a few builtins (feature 91138a85), built on
"Salmon.Op.CardTemplate": a node carrying one has its card in the page drawn
from it instead of the default sections (contract in
@salmon-ops\/ui\/card-template.js@).

__Opt-in.__ No builtin attaches its card by itself, so no existing
declaration serves anything new on @\/dag@. A recipe opts in per node:

> withCard (Cards.fileContents path) (filecontents (FileContents path body))

and since a template is left out of the representative, adding one (or
dropping it) restarts nothing.

What a card shows is public by construction: the values baked into it are
the ones the builtin already makes public in its @ref@ and @help@ (a path, a
unit name, a port, a database name), and the rest is read from the node's
own public @\/dag@ fields at drawing time. A card never takes, and so never
shows, a file's contents, a password or any digest of either.

Every card links to its builtin's page on the salmon docs site ('cardDocs',
'docsLink'; the pages are @resources\/builtins\/<slug>.md@). The link is in the
template only, so like the rest of the card it restarts nothing.
-}
module Salmon.Builtin.Cards (
    -- * Attaching
    withCard,

    -- * Cards for builtins
    fileContents,
    directory,
    systemdService,
    pgDatabase,
    fileContentsCard,
    directoryCard,
    systemdServiceCard,
    pgDatabaseCard,

    -- * The skeleton
    Card (..),
    Verdicts (..),
    Link (..),
    renderCard,
    allLinks,

    -- * The builtin's page on the docs site
    docsBaseUrl,
    docsLink,
    tramajString,
    percentEncode,
) where

import qualified Data.ByteString as ByteString
import Data.Char (isAsciiLower, isAsciiUpper, isControl, isDigit, ord)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Numeric (showHex)
import Text.Printf (printf)
import System.FilePath (takeDirectory)

import Salmon.Builtin.Extension (Op, amendNode, withCardTemplate)
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import qualified Salmon.Builtin.Nodes.Systemd as Systemd

-- | Give this node (the root only, not its dependencies) this card.
withCard :: Text -> Op -> Op
withCard src = amendNode (withCardTemplate src)

{- | A card's content, everything but the node's own fields. All the text is
author-written and public: it ends up in the template source, which @\/dag@
serves.
-}
data Card = Card
    { cardFacts :: [(Text, Text)]
    -- ^ identity rows, drawn as a small table (label, value)
    , cardVerdicts :: Verdicts
    , cardUp :: Text
    -- ^ what @up@ does
    , cardDown :: Text
    -- ^ what @down@ does
    , cardLinks :: [Link]
    -- ^ external links (a cloud console page, say), drawn under the facts;
    -- none draws nothing
    , cardDocs :: Maybe Text
    -- ^ the builtin's page on the salmon docs site, by slug ('docsLink'),
    -- drawn after 'cardLinks'; 'Nothing' draws nothing
    }
    deriving (Eq, Show)

{- | A link out of the page, to the managed system. Built only from public
node data (a project, a zone, a name: what the node's @ref@ and @help@
already publish), never from a secret. The page draws only an @https@ URL
without credentials, opens it in a new tab with no referrer, and draws
anything else as plain text (@ui\/card-template.js@).
-}
data Link = Link
    { linkLabel :: Text
    , linkUrl :: Text
    }
    deriving (Eq, Show)

-- | The last check, in the node's own words.
data Verdicts = Verdicts
    { verdictInPlace :: Text
    -- ^ @success@, @completed@, @skipped@
    , verdictToApply :: Text
    -- ^ @failure@ (its reason is drawn under it)
    , verdictUnsure :: Text
    -- ^ @unknown@
    , verdictNoCheck :: Text
    -- ^ @immaterial@: the node has no check
    , verdictUnchecked :: Text
    -- ^ no check result in what the page has seen (the page's check comes from @\/dag@, so a page loaded before the
    -- first check says this until it is reloaded)
    }
    deriving (Eq, Show)

{- | A Tramaj string literal for this text: quotes, backslashes and
backticks (which open an interpolation) are escaped, and so is every
control character, so no text can end the literal or reach the program.
-}
tramajString :: Text -> Text
tramajString t = "\"" <> Text.concatMap esc t <> "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '`' = "\\`"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc '\r' = "\\r"
    esc c
        | isControl c = Text.pack ("\\u{" <> showHex (ord c) "}")
        | otherwise = Text.singleton c

{- | The template for a card. The layout is the same for every builtin:
the help line, the identity facts, the last check in the node's words (and
its reason), a conflict warning, what @up@ and @down@ do, then the notes,
dynamics and tending output folded away.
-}
renderCard :: Card -> Text
renderCard c =
    Text.intercalate "\n" $
        [ "@v = $ctx.check.verdict"
        , "@ok = or(eq($v, \"success\"), or(eq($v, \"completed\"), eq($v, \"skipped\")))"
        , ".div("
        , "  .p(class: \"t-muted\", $ctx.help),"
        , "  .table(.tbody(" <> Text.intercalate ", " (fmap fact c.cardFacts) <> ")),"
        ]
            <> links
            <> [ "  .p(class: branch(\"t-muted\", $ok, \"t-ok\", eq($v, \"failure\"), \"t-warn\", eq($v, \"unknown\"), \"t-warn\"),"
        , "     branch(" <> s v.verdictUnchecked <> ", $ok, " <> s v.verdictInPlace <> ", eq($v, \"failure\"), " <> s v.verdictToApply <> ", eq($v, \"unknown\"), " <> s v.verdictUnsure <> ", eq($v, \"immaterial\"), " <> s v.verdictNoCheck <> ")),"
        , "  branch(.p(class: \"t-muted\", $ctx.check.reason), eq($ctx.check.reason, null), \"\"),"
        , "  branch(\"\", $ctx.conflict, .p(class: \"t-err\", \"two live declarations disagree about this node\")),"
        , "  .h5(\"up\"), .p(" <> s c.cardUp <> "),"
        , "  .h5(\"down\"), .p(" <> s c.cardDown <> "),"
        , "  .details(.summary(\"notes (`str(count($ctx.notes))`)\"), .ul(map($ctx.notes, (n) => .li($n)))),"
        , "  .details(.summary(\"dynamics (`str(count($ctx.dynamics))`)\"), .ul(map($ctx.dynamics, (d) => .li(.code($d))))),"
        , "  .details(.summary(\"output (`str(count($ctx.output))`)\"), .pre(map($ctx.output, (l) => \"`$l`\\n\")))"
        , ")"
        ]
  where
    s = tramajString
    v = c.cardVerdicts
    fact (k, x) = ".tr(.th(" <> s k <> "), .td(.code(" <> s x <> ")))"
    -- nothing at all without links, so a card without any renders as before
    links = case allLinks c of
        [] -> []
        ls -> ["  .p(class: \"card-links\", " <> Text.intercalate ", \" · \", " (fmap link ls) <> "),"]
    link :: Link -> Text
    link l = ".a(href: " <> s l.linkUrl <> ", " <> s l.linkLabel <> ")"

-- | What a card links to, in drawing order: its 'cardLinks', then its docs page.
allLinks :: Card -> [Link]
allLinks c = c.cardLinks <> maybe [] (\slug -> [docsLink slug]) c.cardDocs

{- | Where the salmon documentation site is published: the kitchen-sink site
of @website\/@, so this must equal the @publishURL@ and @basePath@ of
@website\/src\/kitchen-sink.json@ (@BuiltinCardsSpec@ checks it). GitHub
Pages serves it from @docs\/@ on @master@ (@website\/README.md@). A
constant, not a setting:
a card's template is fixed when the graph is built, and it is not part of the
representative, so changing this restarts nothing.
-}
docsBaseUrl :: Text
docsBaseUrl = "https://lucasdicioccio.github.io/salmon"

{- | The link to a builtin's page on the docs site: @builtin-<slug>.html@,
the kitchen-sink page made from @resources\/builtins\/<slug>.md@
(@website\/scripts\/sync-repo-docs.sh@ mirrors it to
@website\/src\/builtin-<slug>.cmark@, so the page is in the site's search
index and its "Builtin nodes" navigation group). The slug is author-chosen and
fixed per builtin (never a declaration's value), so the URL needs no
encoding beyond what the slug already is.
-}
docsLink :: Text -> Link
docsLink slug = Link "docs" (docsBaseUrl <> "/builtin-" <> slug <> ".html")

{- | Percent-encodes everything but RFC 3986's unreserved characters, as
UTF-8: for a name put in a URL's path segment or query value.
-}
percentEncode :: Text -> Text
percentEncode = Text.concatMap enc
  where
    enc c
        | isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-._~" :: String) = Text.singleton c
        | otherwise = Text.pack (concatMap (printf "%%%02X") (ByteString.unpack (Text.encodeUtf8 (Text.singleton c))))

{- | For 'Salmon.Builtin.Nodes.Filesystem.filecontents' at this path. The
contents are not on the card, nor any digest of them (the builtin's own
@content-hash@ note, when it has one, still shows among the notes).
-}
fileContents :: FilePath -> Text
fileContents path = renderCard (fileContentsCard path)

-- | 'fileContents' before rendering.
fileContentsCard :: FilePath -> Card
fileContentsCard path =
        Card
            { cardFacts = [("path", Text.pack path), ("directory", Text.pack (takeDirectory path))]
            , cardVerdicts =
                Verdicts
                    { verdictInPlace = "in place, as declared"
                    , verdictToApply = "missing or different from the declaration: up writes it"
                    , verdictUnsure = "could not tell whether the file is as declared"
                    , verdictNoCheck = "the node declares no check"
                    , verdictUnchecked = "no check result on this page yet"
                    }
            , cardUp = "writes the whole file (no merge with what is there)"
            , cardDown = "removes the file (already absent is fine); the directory stays"
            , cardLinks = []
            , cardDocs = Just "filesystem-file-contents"
            }

-- | For 'Salmon.Builtin.Nodes.Filesystem.dir' at this path.
directory :: FilePath -> Text
directory path = renderCard (directoryCard path)

-- | 'directory' before rendering.
directoryCard :: FilePath -> Card
directoryCard path =
        Card
            { cardFacts = [("path", Text.pack path)]
            , cardVerdicts =
                Verdicts
                    { verdictInPlace = "in place"
                    , verdictToApply = "missing: up creates it"
                    , verdictUnsure = "could not tell whether the directory is there"
                    , verdictNoCheck = "no check: asking costs what creating costs, so up re-applies (under serve, on the supervision delay)"
                    , verdictUnchecked = "no check result on this page yet"
                    }
            , cardUp = "creates the directory and its parents; never touches what is inside"
            , cardDown = "removes the directory only when it is empty (a non-empty one fails, on purpose)"
            , cardLinks = []
            , cardDocs = Just "filesystem-directory"
            }

{- | For 'Salmon.Builtin.Nodes.Systemd.systemdService' (and its watching
variants) on this config: the unit, its scope and its unit directory, never
the unit file's body.
-}
systemdService :: Systemd.Config -> Text
systemdService cfg = renderCard (systemdServiceCard cfg)

-- | 'systemdService' before rendering.
systemdServiceCard :: Systemd.Config -> Card
systemdServiceCard cfg =
        Card
            { cardFacts =
                [ ("unit", cfg.config_target)
                , ("scope", scope cfg.config_scope)
                , ("unit dir", Text.pack cfg.config_unit_dir)
                ]
            , cardVerdicts =
                Verdicts
                    { verdictInPlace = "active and enabled, as loaded"
                    , verdictToApply = "not as declared: up reloads, enables and restarts it"
                    , verdictUnsure = "in transition, or systemctl did not answer: nothing restarts on this"
                    , verdictNoCheck = "the node declares no check"
                    , verdictUnchecked = "no check result on this page yet"
                    }
            , cardUp = "systemctl daemon-reload, enable, then restart"
            , cardDown = "systemctl stop; the unit file is its own node"
            , cardLinks = []
            , cardDocs = Just "systemd-service"
            }
  where
    scope Systemd.System = "system"
    scope Systemd.User = "user"

{- | For 'Salmon.Builtin.Nodes.Postgres.database' on this port: the
database and the port. That builtin has no check, which the card says.
-}
pgDatabase :: Postgres.Port -> Postgres.Database -> Text
pgDatabase port db = renderCard (pgDatabaseCard port db)

-- | 'pgDatabase' before rendering.
pgDatabaseCard :: Postgres.Port -> Postgres.Database -> Card
pgDatabaseCard port db =
        Card
            { cardFacts = [("database", db.getDatabase), ("port", Text.pack (show port))]
            , cardVerdicts =
                Verdicts
                    { verdictInPlace = "the database is there"
                    , verdictToApply = "missing: up creates it"
                    , verdictUnsure = "could not ask the server"
                    , verdictNoCheck = "no check: run up applies it every time (a no-op when it exists); under serve it parks and nothing watches it"
                    , verdictUnchecked = "no check result on this page yet"
                    }
            , cardUp = "CREATE DATABASE as the postgres user, on the local server at this port, unless pg_database already has it"
            , cardDown = "nothing: a database is never dropped by this node"
            , cardLinks = []
            , cardDocs = Just "postgres-database"
            }
