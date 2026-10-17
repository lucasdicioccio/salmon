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
    renderCard,
    tramajString,
) where

import Data.Char (isControl, ord)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (showHex)
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
    Text.intercalate
        "\n"
        [ "@v = $ctx.check.verdict"
        , "@ok = or(eq($v, \"success\"), or(eq($v, \"completed\"), eq($v, \"skipped\")))"
        , ".div("
        , "  .p(class: \"t-muted\", $ctx.help),"
        , "  .table(.tbody(" <> Text.intercalate ", " (fmap fact c.cardFacts) <> ")),"
        , "  .p(class: branch(\"t-muted\", $ok, \"t-ok\", eq($v, \"failure\"), \"t-warn\", eq($v, \"unknown\"), \"t-warn\"),"
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
            }
