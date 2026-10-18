{- | A readable text reporter for @run up@\/@run down@: one line per
'UpDown.Report', where 'Salmon.Reporter.reportPrint' prints the derived
'Show' of the report and of its 'Act' (a long record per report).

A line is the report's kind (the same word as its @kind@ in the JSON
encoding of "Salmon.Reporter.Tagged"), the node's 'shortRef' prefixed with
@#@ (what a @--select \#...@ selector matches), and the node's shorthand and
help:

> eval    #MTA4NDYx file: write /etc/motd
> done    #MTA4NDYx file: write /etc/motd
> failed  #MTA4NDYx file: write /etc/motd -- user error (exit 1)

Everything printed (shorthand, help, the failure text) is already public
report text (see CLAUDE.md, "Secrets"). Newlines and carriage returns inside
those (any control character but a tab) are escaped as @\\n@\/@\\r@\/... so that each report is exactly one line.

This is an opt-in: 'Salmon.Reporter.reportPrint' is unchanged, so a binary
that passes it prints what it always printed. @--json@ remains the
machine-readable form; this one is for people and is not a format to parse.
-}
module Salmon.Reporter.UpDownText (
    reportText,
    reportHText,
    renderReport,
    renderKind,
) where

import Control.Exception (SomeException)
import Data.Char (isControl, showLitChar)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (Handle, hFlush, stdout)

import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension (Extension (..))
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Mailbox as Mailbox
import Salmon.Op.Ref (Ref, shortRef)
import Salmon.Reporter (Reporter, ReporterM (..))

-- | 'reportHText' on standard output.
reportText :: Reporter (UpDown.Report Extension)
reportText = reportHText stdout

{- | One 'renderReport' line per report, written in one call and flushed, so
that a report is never split by another thread writing to the same handle.
-}
reportHText :: Handle -> Reporter (UpDown.Report Extension)
reportHText h = ReporterM $ \rep -> do
    Text.hPutStr h (renderReport rep <> "\n")
    hFlush h

-- | The report as one line, without its trailing newline.
renderReport :: UpDown.Report Extension -> Text
renderReport rep =
    case rep of
        UpDown.Skip act -> about act []
        UpDown.Eval act -> about act []
        UpDown.Done act -> about act []
        UpDown.Failed act e -> about act [exceptionText e]
        UpDown.Blocked act -> about act []
        UpDown.Conflicting r kept replaced ->
            line r ["kept", node kept <> ";", "replaced", node replaced]
        UpDown.Instructed act instr -> about act [instructionText instr]
        UpDown.DroppedInstructions act n -> about act [tshow n <> " dropped"]
  where
    about :: Act Extension -> [Text] -> Text
    about act extra =
        line act.extension.ref (node act : concatMap (\x -> ["--", x]) extra)

    line :: Ref -> [Text] -> Text
    line r rest = Text.unwords (Text.justifyLeft 7 ' ' (renderKind rep) : ("#" <> shortRef r) : rest)

    node :: Act Extension -> Text
    node act = oneLine act.shorthand <> ": " <> oneLine act.extension.help

{- | The report's kind: the word "Salmon.Reporter.Tagged" puts in the @kind@
field of the same report's JSON object.
-}
renderKind :: UpDown.Report ext -> Text
renderKind rep =
    case rep of
        UpDown.Skip _ -> "skip"
        UpDown.Eval _ -> "eval"
        UpDown.Done _ -> "done"
        UpDown.Failed _ _ -> "failed"
        UpDown.Blocked _ -> "blocked"
        UpDown.Conflicting{} -> "conflicting"
        UpDown.Instructed _ _ -> "instructed"
        UpDown.DroppedInstructions _ _ -> "dropped-instructions"

-- | As "Salmon.Reporter.Tagged" renders a failure: the exception's 'show'.
exceptionText :: SomeException -> Text
exceptionText = oneLine . Text.pack . show

instructionText :: Mailbox.Instruction -> Text
instructionText instr =
    case instr of
        Mailbox.Force -> "force"
        Mailbox.Satisfy -> "satisfy"
        Mailbox.Recheck -> "recheck"
        Mailbox.Pause -> "pause"
        Mailbox.Resume -> "resume"

{- | Escape what would break the line, so one report stays one line: every
control character but a tab, as Haskell writes it in a string literal
(@\\n@, @\\r@, @\\ESC@, ...).
-}
oneLine :: Text -> Text
oneLine = Text.concatMap escape
  where
    escape c
        | isControl c && c /= '\t' = Text.pack (showLitChar c "")
        | otherwise = Text.singleton c

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
