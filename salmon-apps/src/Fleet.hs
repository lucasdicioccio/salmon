{-# LANGUAGE OverloadedStrings #-}

{- | @salmon-fleet@: the controller's side of pull mode. @salmon-fleet
status DIR@ (milestone 5 of @specs/pull-mode.md@) folds every status
document in @DIR@ — one per host, written by each host's @run serve
--status-sink@ — into one line per host; the fold itself is
"Salmon.Actions.Fleet", and this is the command line over it. It reads
only, and writes nothing a host reads.

@status@'s default output is one tab-separated line per host
('Salmon.Actions.Fleet.renderRow'), unchanged, for scripts and pipes.
Beside it, @--pretty@ draws the same rows as a bordered table (using
@layoutz-hs@, <https://github.com/mattlianje/layoutz>) with the stale
marker as its own column, so it is visible on a screen reader or a pipe
and not only as color. Which one comes out by default follows the same
convention @ls@\/@git@ use: pretty on an interactive terminal
('System.IO.hIsTerminalDevice'), TSV otherwise; @--pretty@\/@--no-pretty@ force
either one regardless, and @--json@ still wins over both, unchanged. Color
is applied only when standard output actually is a terminal — never with
@--json@, never with plain TSV, and never just because @--pretty@ was
passed while piped — as a post-render, whole-line ANSI wrap (no new
dependency for it): 'Salmon.Actions.Fleet.renderRow'\/'Fleet.Row' are
already plain, so this never touches the fold or its TSV rendering, only
how @salmon-apps@ prints on top of it. The decision of whether to go
pretty at all is the pure, testable 'decidePretty': terminal detection
happens once in 'main' and is passed in as a plain 'Bool', so a test never
needs a real pty.

@salmon-fleet keygen --out FILE@ and @salmon-fleet sign --key FILE@ are the
signing half of "Salmon.Actions.Follow.Signature": a key pair as two JWK
files (@FILE@, mode 0600, and @FILE.pub@ for the hosts' @--follow-key@), and
a document on standard input wrapped in a signed envelope on standard
output — so the round trip from a document to a host that verifies it needs
no tool but this one. @sign@ writes to the filesystem only where @--out@
says; @keygen@ refuses to overwrite a key that exists.

@salmon-fleet describe@ and @salmon-fleet run ARGS@ are the
@agents-exe@ bash-toolbox protocol (see @documentation/binary-tool.md@ in
the @agents-exe@ repository) wrapped around @status@ alone — the only
subcommand here that is flat-arg, one-shot and read-only to begin with.
@describe@ prints the tool's JSON description ('fleetDescribe'\/'describeValue');
@run DIR [--label L] [--stale S]@ runs the equivalent of
@status DIR [--label L] [--stale S] --json@ (JSON forced; @--pretty@\/@--no-pretty@
are a terminal's business and are not exposed to a toolbox caller, which is a
program, not a terminal). @status@ itself is unchanged. One known gap: the
current @describe@\/@run@ spec's @arity@ is @single@ or @optional@ only — there is
no repeatable arity — so @--label@, repeatable on the real CLI, is exposed to the
toolbox as a single optional string; a toolbox caller cannot filter on more than
one label at once. @keygen@\/@sign@ are not exposed this way (out of scope for
this pass).
-}
module Fleet (
    main,

    -- * The pretty table (exposed for tests)
    decidePretty,
    prettyTable,

    -- * The agents-exe bash-toolbox protocol (exposed for tests)
    describeValue,
    foldStatusDir,
) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LByteString
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime)
import qualified Layoutz as L
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdout)

import Salmon.Actions.Serve (AppliedDocument (..))
import Salmon.Actions.Serve.StatusSink (Document)
import qualified Salmon.Actions.Fleet as Fleet
import qualified Salmon.Actions.Follow as Follow
import qualified Salmon.Actions.Follow.Signature as Signature

data Command
    = Status FilePath (Maybe String) Double Bool (Maybe Bool)
    | Keygen FilePath
    | Sign FilePath (Maybe FilePath) (Maybe String)
    | Describe
    | Run FilePath (Maybe String) Double

main :: IO ()
main = do
    cmd <- execParser (info (commandP <**> helper) (fullDesc <> progDesc "The controller's side of pull mode: fold status sink documents, make signing keys, sign documents" <> header "salmon-fleet"))
    case cmd of
        Keygen path -> do
            taken <- doesFileExist path
            when taken $ do
                hPutStrLn stderr ("salmon-fleet: " <> path <> " exists; not overwriting a key")
                exitFailure
            key <- Signature.generateKeyPair
            Signature.writeKeyPair path key
            hPutStrLn stderr ("salmon-fleet: wrote " <> path <> " (private, 0600) and " <> path <> ".pub (public); key id " <> Text.unpack (Signature.keyId (Signature.publicKey key)))
        Sign keyPath out mlabel -> do
            loaded <- Signature.readPrivateKeyFile keyPath
            key <- case loaded of
                Left err -> hPutStrLn stderr ("salmon-fleet: --key " <> Text.unpack err) >> exitFailure
                Right k -> pure k
            document <- LByteString.getContents
            lbl <- case traverse (Follow.mkLabel . Text.pack) mlabel of
                Left err -> hPutStrLn stderr ("salmon-fleet: --label: " <> Text.unpack err) >> exitFailure
                Right l -> pure l
            signed <- Signature.signDocumentFor key lbl document
            case signed of
                Left err -> hPutStrLn stderr ("salmon-fleet: cannot sign: " <> Text.unpack err) >> exitFailure
                Right envelope -> maybe LByteString.putStr LByteString.writeFile out envelope
        Status dir label stale asJson prettyOverride -> do
            (docs, rows, rejected) <- foldStatusDir dir label stale
            if asJson
                then LByteString.putStr (encode rows <> "\n")
                else do
                    isTty <- hIsTerminalDevice stdout
                    if decidePretty prettyOverride isTty
                        then Text.putStrLn (prettyTable isTty rows)
                        else do
                            Text.putStrLn Fleet.renderHeader
                            forM_ rows (Text.putStrLn . Fleet.renderRow)
            reportStatusOutcome docs rows rejected label
        Describe ->
            LByteString.putStr (encode describeValue <> "\n")
        Run dir label stale -> do
            (docs, rows, rejected) <- foldStatusDir dir label stale
            LByteString.putStr (encode rows <> "\n")
            reportStatusOutcome docs rows rejected label

-- | @status@ and @run@'s shared work: read the directory, fold it. Kept in
-- one place so the toolbox @run@ path (which always emits JSON) can't drift
-- from what @status --json@ computes.
foldStatusDir :: FilePath -> Maybe String -> Double -> IO ([(FilePath, Document)], [Fleet.Row], [(FilePath, String)])
foldStatusDir dir label stale = do
    (docs, rejected) <- Fleet.readStatusDir dir
    forM_ rejected $ \(path, why) ->
        hPutStrLn stderr ("salmon-fleet: skipping " <> path <> ": " <> why)
    now <- getCurrentTime
    let opts = Fleet.Options (Text.pack <$> label) (realToFrac stale)
        rows = Fleet.fold opts now docs
    pure (docs, rows, rejected)

-- | @status@ and @run@'s shared stderr/exit-code behaviour: warn if
-- @--label@ matched nobody, and exit non-zero if the directory had nothing
-- readable in it at all.
reportStatusOutcome :: [(FilePath, Document)] -> [Fleet.Row] -> [(FilePath, String)] -> Maybe String -> IO ()
reportStatusOutcome docs rows rejected label = do
    unless (null docs || not (null rows) || label == Nothing) $
        hPutStrLn stderr ("salmon-fleet: no host carries label " <> maybe "" id label)
    -- a directory with nothing readable in it is an error worth an
    -- exit code: the fold has nothing to say and probably was not
    -- pointed at the right place
    unless (not (null docs) || null rejected) exitFailure

{- | Pretty on an interactive terminal, TSV otherwise — the same convention
@ls@\/@git@ follow — unless @--pretty@\/@--no-pretty@ (a 'Just') forces one
or the other regardless of whether standard output is actually a terminal.
Pure so a test can drive both sides of the terminal-detection line without
a pty. -}
decidePretty :: Maybe Bool -> Bool -> Bool
decidePretty override isTerminal = maybe isTerminal id override

{- | The rows as a bordered table (@layoutz-hs@), one row per host, with an
explicit @STALE@ column beside the other fields — so the stale marker
survives a pipe or a screen reader exactly like every other column, rather
than living only in color. The @Bool@ says whether standard output is
/actually/ a terminal: only then are stale and errored rows painted, and
only as a whole-line ANSI wrap applied /after/ layoutz has rendered and
padded every cell — layoutz sizes columns by the plain length of each
cell's text, so coloring a cell's substring in place would misalign every
column after it; wrapping the finished line does not, since the escape
codes carry no visible width. -}
prettyTable :: Bool -> [Fleet.Row] -> Text
prettyTable colorOn rows =
    let headers = ["HOST", "MODE", "LABELS", "CONVERGED", "ERRORED", "AGE", "STALE"]
        cellsOf r =
            [ L.text (Text.unpack r.rowHost)
            , L.text (Text.unpack r.rowMode)
            , L.text (labelsCell r)
            , L.text (show r.rowConverged <> "/" <> show r.rowNodes)
            , L.text (show r.rowErrored)
            , L.text (show (round r.rowAge :: Integer) <> "s")
            , L.text (if r.rowStale then "STALE" else "")
            ]
        rendered = Text.pack (L.render (L.table headers (fmap cellsOf rows)))
     in if colorOn then colorizeRows rows rendered else rendered

labelsCell :: Fleet.Row -> String
labelsCell r
    | null r.rowLabels = "-"
    | otherwise = intercalate "," (fmap labelText r.rowLabels)
  where
    labelText :: AppliedDocument -> String
    labelText a = Text.unpack (a.appliedDocLabel <> "=" <> a.appliedDocId <> "@" <> Text.take 12 a.appliedDocDigest)

{- | Paint each data row of the rendered table: red for a stale host, yellow
for one with errored nodes, untouched otherwise (borders and header
included). The table's own shape is what makes this safe without parsing
it back: 'L.table' puts exactly one top border, one header, one separator,
then one line per row (none of our cells contain a newline), then one
bottom border — so the @i@-th row is line @3 + i@. -}
colorizeRows :: [Fleet.Row] -> Text -> Text
colorizeRows rows rendered =
    Text.intercalate "\n" (zipWith paint [0 ..] (Text.lines rendered))
  where
    n = length rows
    paint :: Int -> Text -> Text
    paint i line
        | i >= 3 && i < 3 + n =
            let r = rows !! (i - 3)
             in if r.rowStale
                    then ansiWrap ansiRed line
                    else
                        if r.rowErrored > 0
                            then ansiWrap ansiYellow line
                            else line
        | otherwise = line

ansiWrap :: Text -> Text -> Text
ansiWrap code line = code <> line <> ansiReset

ansiRed, ansiYellow, ansiReset :: Text
ansiRed = "\ESC[31m"
ansiYellow = "\ESC[33m"
ansiReset = "\ESC[0m"

-------------------------------------------------------------------------------
-- agents-exe bash-toolbox describe/run (documentation/binary-tool.md in the
-- agents-exe repository): a slug, a description, and one arg object per
-- 'runP' argument below, kept in exact correspondence with it by hand (see
-- Test.FleetSpec's schema-shape test).

-- | The JSON @salmon-fleet describe@ prints: this tool's toolbox interface,
-- wrapping @status@ alone. See the module haddock for the known gap
-- (@--label@ is repeatable on the real CLI; the current toolbox spec's
-- @arity@ has no repeatable case, so it is exposed here as a single
-- optional string).
describeValue :: Value
describeValue =
    object
        [ "slug" .= ("salmon-fleet-status" :: Text)
        , "description"
            .= ( "One line per host, folded from a directory of salmon `run serve --status-sink` "
                    <> "status documents (one JSON file per host): host name, mode, applied labels, "
                    <> "converged/errored node counts out of the total, and how long ago the host "
                    <> "last wrote its status. Read-only; writes nothing." ::
                    Text
               )
        , "args"
            .= [ object
                    [ "name" .= ("dir" :: Text)
                    , "description" .= ("A directory of *.json status sink documents, one per host." :: Text)
                    , "type" .= ("string" :: Text)
                    , "backing_type" .= ("string" :: Text)
                    , "arity" .= ("single" :: Text)
                    , "mode" .= ("positional" :: Text)
                    ]
               , object
                    [ "name" .= ("label" :: Text)
                    , "description"
                        .= ( "Only include hosts whose applied documents carry this label. The underlying "
                                <> "CLI allows repeating --label; this toolbox arg is single-valued only, since "
                                <> "the current describe/run spec has no repeatable arity." ::
                                Text
                           )
                    , "type" .= ("string" :: Text)
                    , "backing_type" .= ("string" :: Text)
                    , "arity" .= ("optional" :: Text)
                    , "mode" .= ("dashdashspace" :: Text)
                    ]
               , object
                    [ "name" .= ("stale" :: Text)
                    , "description" .= ("Flag a host whose status document is older than this many seconds (default 60)." :: Text)
                    , "type" .= ("number" :: Text)
                    , "backing_type" .= ("string" :: Text)
                    , "arity" .= ("optional" :: Text)
                    , "mode" .= ("dashdashspace" :: Text)
                    ]
               ]
        , "empty-result"
            .= object
                [ "tag" .= ("AddMessage" :: Text)
                , "contents" .= ("No status documents found in DIR (or none carry --label)." :: Text)
                ]
        ]

commandP :: Parser Command
commandP =
    hsubparser $
        command "status" (info statusP (progDesc "One line per host from the status documents in DIR."))
            <> command "keygen" (info keygenP (progDesc "Write a fresh Ed25519 signing key pair: FILE (private, 0600) and FILE.pub (public, for --follow-key)."))
            <> command "sign" (info signP (progDesc "Wrap the document on standard input in a signed envelope, on standard output (or --out FILE)."))
            <> command "describe" (info (pure Describe) (progDesc "Print the agents-exe bash-toolbox JSON description of this tool (wraps `status` only)."))
            <> command "run" (info runP (progDesc "The agents-exe bash-toolbox entry point: the equivalent of `status DIR [--label L] [--stale S] --json`."))
  where
    keygenP =
        Keygen
            <$> strOption (long "out" <> metavar "FILE" <> help "Where to write the private key; the public key goes to FILE.pub.")
    signP =
        Sign
            <$> strOption (long "key" <> metavar "FILE" <> help "The private key (JWK) to sign with, as `keygen --out FILE` wrote it.")
            <*> optional (strOption (long "out" <> metavar "FILE" <> help "Write the signed envelope here instead of standard output."))
            <*> optional (strOption (long "label" <> metavar "LABEL" <> help "The label this document is for; put into the signed document so a host following another label refuses it. Hosts run with --follow-key refuse a signed document that names none, unless --follow-accept-unlabelled."))
    statusP =
        Status
            <$> strArgument (metavar "DIR" <> help "A directory of *.json status sink documents (one per host).")
            <*> optional (strOption (long "label" <> metavar "LABEL" <> help "Only hosts whose applied documents include this label."))
            <*> option auto (long "stale" <> metavar "SECONDS" <> value 60 <> showDefault <> help "Flag a host whose document was written longer ago than this.")
            <*> switch (long "json" <> help "Emit the fold as one JSON array instead of lines.")
            <*> prettyOverrideP
    -- agents-exe's flattening: DIR positional, --label/--stale
    -- dashdashspace, exactly `describeValue`'s `args` — JSON is not a flag
    -- here, it's what `run` always emits.
    runP =
        Run
            <$> strArgument (metavar "DIR" <> help "A directory of *.json status sink documents (one per host).")
            <*> optional (strOption (long "label" <> metavar "LABEL" <> help "Only hosts whose applied documents include this label."))
            <*> option auto (long "stale" <> metavar "SECONDS" <> value 60 <> showDefault <> help "Flag a host whose document was written longer ago than this.")
    prettyOverrideP :: Parser (Maybe Bool)
    prettyOverrideP =
        flag' (Just True) (long "pretty" <> help "Force the table output even when standard output is not a terminal.")
            <|> flag' (Just False) (long "no-pretty" <> help "Force the plain tab-separated output even on a terminal.")
            <|> pure Nothing
