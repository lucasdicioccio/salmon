{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | A long-running convergence loop, fed a stream of seeds.

Where @run up@ is a one-shot "expand this one directive and walk it once",
'serve' keeps a 'World' around: a set of seeds that have been declared, the
graphs those seeds evaluated to, and — unified across all of them by 'Ref' —
a per-node 'NodeState' saying which 'Direction' that node is wanted in and
whether it has 'Converged' there yet.

The unit of input is a /declaration/: a seed, plus what to do with it (see
'ServeCommand'). Declaring a seed appends an 'Epoch' to an append-only
history (keeping the seed, the directive it configured to, and the graph it
evaluated to /at that moment/ — a later declaration of the same seed
re-evaluates and appends a fresh epoch rather than mutating the old one) and
updates the set of /active/ seeds. From the active set everything else is
derived:

  * a node in some active seed's graph is wanted 'TurnUp';
  * a node this world has ever seen and no active seed still asks for is
    wanted 'TurnDown' — this is why retired epochs' graphs are kept, they
    are the only remaining description of how to tear those nodes down;
  * flipping a node's direction resets it to 'Pending', so it gets applied
    again in the new direction.

Convergence then runs — after every declaration, and on demand via
@converge@ — as one teardown pass followed by one bring-up pass, each of
which is just "Salmon.Actions.UpDown".'UpDown.downTreeWith' /
'UpDown.upTreeWith' over the relevant graphs with a 'UpDown.Gate' that
filters down to the nodes wanted in that pass and not yet converged. The
dependency ordering, the dedup-by-'Ref', and the "a failed node blocks
whatever depended on it" containment therefore behave exactly as they do for
@run up@ / @run down@; the only thing this module adds on top is the memory
of what has already been done. Nodes that end a pass 'Errored' or 'Blocked'
stay non-converged and are retried by the next pass.
-}
module Salmon.Actions.Serve (
    -- * Running
    serve,

    -- * Input language
    ServeCommand (..),
    Declaration (..),
    Selection (..),
    noSelection,
    Topic,
    parseServeCommand,
    parseSelection,
    tokenize,

    -- * World state
    World (..),
    emptyWorld,
    Epoch (..),
    EpochId (..),
    NodeState (..),
    Direction (..),
    Convergence (..),

    -- * Reporting
    Report (..),
    reportText,
    renderReport,
) where

import Control.Comonad.Cofree (Cofree)
import Control.Exception (IOException, try)
import Control.Monad (when)
import Control.Monad.Identity (runIdentity)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (isSpace)
import Data.Foldable (toList, traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (Handle, hFlush, hGetLine, hIsEOF, stdout)

import qualified Salmon.Actions.Query as Query
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Actions.UpDown (Requirement (..))
-- imported with their field selectors: OverloadedRecordDot only solves
-- HasField for fields that are in scope.
import Salmon.Builtin.Extension (Extension (..), Op, Track', deps, evalDeps, opAct)
import Salmon.Op.Actions (Act (..), Actions (..), ShortHand)
import Salmon.Op.Configure (Configure, gen)
import Salmon.Op.Graph (Graph)
import Salmon.Op.OpGraph (OpGraph (OpGraph))
import Salmon.Op.Ref (Ref, unRef)
import Salmon.Op.Track (run)
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | Which way a node is currently wanted.
data Direction
    = TurnUp
    | TurnDown
    deriving (Show, Eq, Ord)

-- | How far a node is from its wanted 'Direction'.
data Convergence
    = -- | never applied in the current direction (new node, or the
      -- direction just flipped under it)
      Pending
    | -- | applied in the current direction, or found to already be there
      Converged
    | -- | the last attempt threw; will be retried
      Errored
    | -- | the last attempt never ran because a neighbour failed; will be retried
      Blocked
    deriving (Show, Eq, Ord)

{- | What 'serve' remembers about one node of the unified graph. Keyed by
'Ref', so the same node reached through several seeds' graphs is one entry.
-}
data NodeState = NodeState
    { nodeShorthand :: !ShortHand
    , nodeHelp :: !Text
    , nodeDirection :: !Direction
    , nodeConvergence :: !Convergence
    , -- | most recent epoch whose graph contained this node
      nodeEpoch :: !EpochId
    }
    deriving (Show)

newtype EpochId = EpochId {unEpochId :: Int}
    deriving (Show, Eq, Ord)

{- | One declaration, and everything derived from it at the time it was made.
Kept forever in 'worldHistory' (the graph of a retired seed is what a
teardown is walked over), so this is deliberately a snapshot: re-declaring
the same seed appends a new epoch instead of updating this one.
-}
data Epoch seed directive = Epoch
    { epochId :: !EpochId
    , epochDeclaration :: !Declaration
    , epochDirection :: !Direction
    , -- | the argv this seed was declared with, kept for @history@; a
      -- directive-file declaration (@up-directive@ and friends) gets a
      -- synthetic @["<directive-file>", path]@ here instead.
      epochTokens :: [String]
    , -- | 'Nothing' when this epoch was declared straight from a directive
      -- file, which has no seed to keep.
      epochSeed :: Maybe seed
    , epochDirective :: directive
    , -- | identity of the seed for the active set: its encoded directive, so
      -- that two spellings of the same desired state are one active seed
      epochKey :: !ByteString
    , epochOp :: Op
    , -- | the graph as evaluated when the seed was declared
      epochGraph :: Cofree Graph Op
    , epochRefs :: Map Ref (ShortHand, Text)
    }

data World seed directive = World
    { worldNextId :: !Int
    , -- | append-only, newest first
      worldHistory :: [Epoch seed directive]
    , -- | seeds currently declared up, by 'epochKey'
      worldActive :: Map ByteString EpochId
    , -- | every node ever seen, unified by 'Ref'
      worldNodes :: Map Ref NodeState
    }

emptyWorld :: World seed directive
emptyWorld = World 0 [] Map.empty Map.empty

-------------------------------------------------------------------------------

-- | What declaring a seed does to the active set.
data Declaration
    = -- | @up@: add this seed to the active set
      Add
    | -- | @only@: make this seed the whole active set, retiring the others
      Replace
    | -- | @down@: retire this seed
      Remove
    deriving (Show, Eq, Ord)

{- | A pair of select\/exclude path-patterns, understood the same way as
"Salmon.Actions.Query" ('Query.parsePattern'\/'Query.matchPattern'). An empty
'selSelect' means "everything" (mirrors 'Query.resolveSelectors'); an empty
'selExclude' subtracts nothing. 'noSelection' is both empty, and is what a
bare @status@\/@history@\/@converge@\/@query@ (no @--select@\/@--exclude@ at
all) parses to.
-}
data Selection = Selection
    { selSelect :: ![Text]
    , selExclude :: ![Text]
    }
    deriving (Show, Eq)

noSelection :: Selection
noSelection = Selection [] []

data ServeCommand
    = Declare !Declaration ![String]
    | -- | @up-directive@\/@only-directive@\/@down-directive@: declare a seed
      -- straight from a directive JSON file, skipping seed-arg parsing.
      DeclareDirective !Declaration !FilePath
    | -- | @load@: run a file of serve-command lines, in order, as if typed.
      Load !FilePath
    | -- | @clear@: retire every seed (everything known goes down)
      Clear
    | -- | @converge@: re-attempt whatever has not converged; a non-empty
      -- 'Selection' restricts this one pass to matching nodes only.
      Converge !Selection
    | Status !Selection
    | History !Selection
    | -- | @query@: annotate the world's nodes with a 'Selection', without
      -- acting on anything.
      QueryCmd !Selection
    | -- | @help@\/@help TOPIC@: print the command reference, or (when
      -- 'Just' a recognised 'Topic') a lengthier explanation of just that
      -- one command. 'Nothing', or a topic 'lookupTopic' doesn't recognise,
      -- both fall back to the same full reference.
      Help !(Maybe Topic)
    | Quit
    | -- | blank line or comment
      Noop
    deriving (Show, Eq)

-- | A @help@ argument, matched case-insensitively against 'helpTopics'.
type Topic = Text

declarationDirection :: Declaration -> Direction
declarationDirection Add = TurnUp
declarationDirection Replace = TurnUp
declarationDirection Remove = TurnDown

{- | Parse one line of 'serve' input: a command word followed, for the
declaring commands, by the seed's own command-line arguments.
-}
parseServeCommand :: String -> Either Text ServeCommand
parseServeCommand line =
    case dropWhile isSpace line of
        [] -> Right Noop
        ('#' : _) -> Right Noop
        _ -> dispatch =<< tokenize line
  where
    dispatch toks =
        case toks of
            [] -> Right Noop
            (w : args) ->
                case w of
                    "up" -> Right (Declare Add args)
                    "only" -> Right (Declare Replace args)
                    "down" -> Right (Declare Remove args)
                    "up-directive" -> onlyFile w args (DeclareDirective Add)
                    "only-directive" -> onlyFile w args (DeclareDirective Replace)
                    "down-directive" -> onlyFile w args (DeclareDirective Remove)
                    "load" -> onlyFile w args Load
                    "clear" -> nullary w args Clear
                    "converge" -> Converge <$> parseSelection args
                    "status" -> Status <$> parseSelection args
                    "history" -> History <$> parseSelection args
                    "query" -> QueryCmd <$> parseSelection args
                    "help" -> Help <$> helpTopic w args
                    "?" -> Help <$> helpTopic w args
                    "quit" -> nullary w args Quit
                    "exit" -> nullary w args Quit
                    _ -> Left ("unknown command: " <> Text.pack w)

    nullary w args cmd
        | null args = Right cmd
        | otherwise = Left (Text.pack w <> " takes no argument")

    helpTopic w args =
        case args of
            [] -> Right Nothing
            [t] -> Right (Just (Text.pack t))
            _ -> Left (Text.pack w <> " takes at most one topic argument")

    onlyFile w args mk =
        case args of
            [path] -> Right (mk path)
            _ -> Left (Text.pack w <> " takes exactly one file argument")

{- | Scans a token list for repeated @--select PATTERN@\/@--exclude
PATTERN@ pairs, shared by @query@\/@status@\/@history@\/@converge@.
-}
parseSelection :: [String] -> Either Text Selection
parseSelection = go [] []
  where
    go sel exc [] = Right (Selection (reverse sel) (reverse exc))
    go sel exc ("--select" : p : rest) = go (Text.pack p : sel) exc rest
    go sel exc ("--exclude" : p : rest) = go sel (Text.pack p : exc) rest
    go _ _ ["--select"] = Left "--select needs a PATTERN argument"
    go _ _ ["--exclude"] = Left "--exclude needs a PATTERN argument"
    go _ _ (w : _) = Left ("unrecognized argument: " <> Text.pack w)

{- | Split a line into argv-style tokens, honouring single quotes, double
quotes and backslash escapes, so a seed can carry values with spaces in them.
-}
tokenize :: String -> Either Text [String]
tokenize = outside []
  where
    outside toks s =
        case s of
            [] -> Right (reverse toks)
            (c : cs)
                | isSpace c -> outside toks cs
                | otherwise -> word toks "" (c : cs)

    word toks cur s =
        case s of
            [] -> Right (reverse (reverse cur : toks))
            (c : cs)
                | isSpace c -> outside (reverse cur : toks) cs
                | c == '\\' -> escape (word toks) cur cs
                | c == '\'' -> quoted '\'' toks cur cs
                | c == '"' -> quoted '"' toks cur cs
                | otherwise -> word toks (c : cur) cs

    quoted q toks cur s =
        case s of
            [] -> Left "unterminated quote"
            (c : cs)
                | c == q -> word toks cur cs
                | c == '\\' && q == '"' -> escape (quoted q toks) cur cs
                | otherwise -> quoted q toks (c : cur) cs

    escape k cur s =
        case s of
            (d : ds) -> k (d : cur) ds
            [] -> Left "trailing backslash"

-------------------------------------------------------------------------------

data Report
    = Started
    | -- | input closed
      Stopped
    | BadCommand !Text
    | BadSeed !Text
    | BadDirective !Text
    | -- | reading a @load@ file failed, or its nesting was too deep
      BadLoad !Text
    | Loading !FilePath
    | -- | lines run from a @load@ file
      LoadDone !FilePath !Int
    | -- | epoch, direction, nodes in its graph, active seeds afterwards
      Declared !EpochId !Direction !Int !Int
    | -- | number of seeds retired
      Cleared !Int
    | -- | nodes to turn down, nodes to turn up
      ConvergeStart !Int !Int
    | -- | everything applied cleanly, nodes still not converged
      ConvergeStop !Bool !Int
    | StatusReport ![(Ref, NodeState)]
    | -- | epoch, declaration, still active, argv
      HistoryReport ![(EpochId, Declaration, Bool, [String])]
    | -- | world nodes annotated against a resolved selection: selected, excluded
      QueryReport ![(Ref, NodeState)] !(Set Ref) !(Set Ref)
    | -- | @help@: the full reference ('Nothing', or a 'Topic' 'lookupTopic'
      -- didn't recognise), or a lengthier explanation of just that one
      -- recognised 'Topic'.
      HelpText !(Maybe Topic)
    deriving (Show)

-- | Prints 'Report's in a human-readable, one-event-per-block form.
reportText :: Reporter Report
reportText = ReporterM $ \rep -> do
    traverse_ Text.putStrLn (renderReport rep)
    hFlush stdout

renderReport :: Report -> [Text]
renderReport rep =
    case rep of
        Started ->
            [ "serve: ready"
            , "serve: type `help` for the command reference"
            ]
        Stopped -> ["serve: input closed"]
        BadCommand err -> ["serve: " <> err]
        BadSeed err -> ("serve: cannot configure seed:") : Text.lines err
        BadDirective err -> ("serve: cannot decode directive:") : Text.lines err
        BadLoad err -> ["serve: " <> err]
        Loading path -> ["serve: loading " <> Text.pack path]
        LoadDone path n -> ["serve: loaded " <> Text.pack path <> " (" <> tshow n <> " line(s))"]
        Declared eid dir nnodes nactive ->
            [ Text.unwords
                [ "serve: epoch"
                , renderEpochId eid
                , renderDirection dir
                , "(" <> tshow nnodes <> " nodes,"
                , tshow nactive <> " active seed(s))"
                ]
            ]
        Cleared n -> ["serve: retired " <> tshow n <> " seed(s)"]
        ConvergeStart ndown nup ->
            ["serve: converging (" <> tshow ndown <> " down, " <> tshow nup <> " up)"]
        ConvergeStop ok remaining ->
            [ Text.unwords
                [ "serve:"
                , -- 'remaining' rather than 'ok' decides the headline: a
                  -- restricted pass can leave nodes pending (skipped, not
                  -- attempted) while still reporting 'ok' — nothing it
                  -- actually attempted failed.
                  if remaining == 0 then "converged" else "converge incomplete"
                , "(" <> tshow remaining <> " node(s) left"
                , if ok then ")" else ", including a failure)"
                ]
            ]
        StatusReport [] -> ["serve: no nodes"]
        StatusReport xs -> "serve: nodes:" : fmap renderNode (sortOn statusOrder xs)
        HistoryReport [] -> ["serve: no seed declared yet"]
        HistoryReport xs -> "serve: seeds:" : fmap renderEpochLine xs
        QueryReport [] _ _ -> ["serve: no nodes"]
        QueryReport xs sel exc -> "serve: nodes:" : fmap (renderQueryNode sel exc) (sortOn statusOrder xs)
        HelpText mtopic ->
            case mtopic >>= lookupTopic of
                Just detailed -> detailed
                Nothing -> commandReference
  where
    statusOrder :: (Ref, NodeState) -> (Direction, Convergence, ShortHand, Text)
    statusOrder (r, st) = (st.nodeDirection, st.nodeConvergence, st.nodeShorthand, unRef r)

    renderNode :: (Ref, NodeState) -> Text
    renderNode (r, st) =
        Text.unwords
            [ " "
            , renderDirection st.nodeDirection
            , Text.justifyLeft 9 ' ' (tshow st.nodeConvergence)
            , Text.justifyLeft 10 ' ' (unRef r)
            , st.nodeShorthand
            ]

    renderQueryNode :: Set Ref -> Set Ref -> (Ref, NodeState) -> Text
    renderQueryNode sel exc entry@(r, _) = renderNode entry <> annotation
      where
        annotation
            | r `Set.member` exc = " [excluded]"
            | r `Set.member` sel = " [selected]"
            | otherwise = ""

    renderEpochLine :: (EpochId, Declaration, Bool, [String]) -> Text
    renderEpochLine (eid, decl, active, toks) =
        Text.unwords
            [ " "
            , renderEpochId eid
            , Text.justifyLeft 8 ' ' (renderDeclaration decl)
            , if active then "[active]" else "[retired]"
            , Text.pack (unwords toks)
            ]

renderDirection :: Direction -> Text
renderDirection TurnUp = "up"
renderDirection TurnDown = "down"

renderDeclaration :: Declaration -> Text
renderDeclaration Add = "up"
renderDeclaration Replace = "only"
renderDeclaration Remove = "down"

renderEpochId :: EpochId -> Text
renderEpochId eid = "#" <> tshow eid.unEpochId

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-- | The full command reference, printed by @help@\/@?@ with no topic, or
-- with a topic 'lookupTopic' doesn't recognise.
commandReference :: [Text]
commandReference =
    [ "serve: commands:"
    , "  up <seed args...>              add this seed to the active set"
    , "  only <seed args...>            make this seed the whole active set, retiring the others"
    , "  down <seed args...>            retire this seed"
    , "  up-directive <file>            like `up`, but from a directive JSON file (no seed parsing)"
    , "  only-directive <file>          like `only`, but from a directive JSON file"
    , "  down-directive <file>          like `down`, but from a directive JSON file"
    , "  load <file>                    run a file of these command lines, in order, as if typed"
    , "  clear                          retire every seed (everything known goes down)"
    , "  converge [--select P]... [--exclude P]..."
    , "                                 re-attempt whatever has not converged;"
    , "                                 with --select/--exclude, restrict this one pass to matching nodes"
    , "  status [--select P]... [--exclude P]..."
    , "                                 list nodes and their direction/convergence"
    , "  history [--select P]... [--exclude P]..."
    , "                                 list past declarations"
    , "  query [--select P]... [--exclude P]..."
    , "                                 annotate nodes [selected]/[excluded], without acting on anything"
    , "  help, ? [TOPIC]                print this reference, or (given a topic) more about just it"
    , "  quit, exit                     leave the loop, changing nothing on the way out"
    , "serve: --select/--exclude patterns are /-separated node-path globs (* one segment, ** any depth);"
    , "       may repeat; omitting --select entirely means everything."
    , "serve: `help TOPIC` for more, where TOPIC is one of:"
    , "       up, directive, load, clear, converge, status, history, query, select"
    ]

{- | @help TOPIC@'s lookup table, matched case-insensitively (several names
may share one block of text, e.g. @up@\/@only@\/@down@ all point at
'declareHelp'). A topic not listed here falls back to 'commandReference'
(see 'lookupTopic').
-}
helpTopics :: [(Topic, [Text])]
helpTopics =
    [ ("up", declareHelp)
    , ("only", declareHelp)
    , ("down", declareHelp)
    , ("directive", directiveHelp)
    , ("up-directive", directiveHelp)
    , ("only-directive", directiveHelp)
    , ("down-directive", directiveHelp)
    , ("load", loadHelp)
    , ("clear", clearHelp)
    , ("converge", convergeHelp)
    , ("status", statusHelp)
    , ("history", historyHelp)
    , ("query", queryHelp)
    , ("select", selectHelp)
    , ("exclude", selectHelp)
    , ("pattern", selectHelp)
    ]

lookupTopic :: Topic -> Maybe [Text]
lookupTopic t = lookup (Text.toLower t) helpTopics

declareHelp :: [Text]
declareHelp =
    [ "serve: up / only / down <seed args...>"
    , ""
    , "  up <seed args...>    parses <seed args...> with the seed's own command-line parser (the"
    , "                       same words that would follow `config` on the command line),"
    , "                       configures it into a directive, and adds the resulting epoch to the"
    , "                       active set."
    , "  only <seed args...>  like `up`, but also retires every other currently active seed. A"
    , "                       node shared with a retiring seed (e.g. an enclosing directory) is"
    , "                       left alone if the new seed still wants it too."
    , "  down <seed args...>  retires this seed. Its nodes go down unless another active seed"
    , "                       still wants them."
    , ""
    , "  A seed is identified by its encoded directive, not by its argv spelling: re-declaring an"
    , "  already-active, unchanged seed is a no-op (nothing pending, nothing re-run)."
    , ""
    , "  Every declaration converges automatically right after being recorded (as if `converge`"
    , "  had been typed next); it is never itself scoped by --select/--exclude."
    , ""
    , "  See also: `help directive` (declaring from a pre-generated directive file instead of"
    , "  seed args), `help load` (batch-declaring several seeds from a script file)."
    ]

directiveHelp :: [Text]
directiveHelp =
    [ "serve: up-directive / only-directive / down-directive <file>"
    , ""
    , "  Exactly like `up`/`only`/`down`, but the seed's own command-line parser is skipped"
    , "  entirely: <file> is read and JSON-decoded straight into the directive, e.g. the output"
    , "  of `my-salmon config <seed-args> > configs/foo.json` saved ahead of time."
    , ""
    , "  Useful when a directive was already generated once (or came from somewhere other than"
    , "  this binary's own seed parser) and there is no seed value to reconstruct here — `status`"
    , "  still shows these nodes normally, and `history` records the file path in place of argv."
    , ""
    , "  A malformed or unreadable file reports an error and declares nothing."
    ]

loadHelp :: [Text]
loadHelp =
    [ "serve: load <file>"
    , ""
    , "  Reads <file> and runs each of its lines through this exact same command language, in"
    , "  order, as if they had been typed (or piped) at the prompt one at a time — including"
    , "  further `load` lines, blank lines, and `#`-comments."
    , ""
    , "  A `quit`/`exit` inside a loaded file ends the whole serve session, not just the load."
    , ""
    , "  Nested loads are capped at a small depth to catch a file that (directly or indirectly)"
    , "  loads itself; exceeding it reports an error rather than looping forever."
    , ""
    , "  This is the way to turn a directory of saved scripts (each a sequence of `up`/`only`/"
    , "  `down`/`up-directive`/... lines) into one `load configs/whatever.txt` declaration."
    ]

clearHelp :: [Text]
clearHelp =
    [ "serve: clear"
    , ""
    , "  Retires every currently active seed in one step (equivalent to a `down` for each). Every"
    , "  node no seed wants any more goes down on the convergence pass that follows automatically."
    , "  Takes no arguments."
    ]

convergeHelp :: [Text]
convergeHelp =
    [ "serve: converge [--select PATTERN]... [--exclude PATTERN]..."
    , ""
    , "  Re-attempts whatever has not yet converged: one teardown pass over nodes wanted down,"
    , "  then one bring-up pass over nodes wanted up. This runs automatically after every"
    , "  declaration; a bare `converge` is for retrying after fixing whatever made a node error"
    , "  out, or after a wait for some external condition."
    , ""
    , "  With --select/--exclude, this one pass is additionally restricted to nodes matching the"
    , "  resolved selection (see `help select`) — anything outside it is left exactly as it was,"
    , "  neither attempted nor marked converged, so a later unrestricted `converge` still picks it"
    , "  up. Omitting both flags converges everything pending, as before."
    , ""
    , "  The report's headline (`converged` vs. `converge incomplete`) reflects how many nodes are"
    , "  still left afterwards, not just whether anything attempted this pass failed — a"
    , "  restricted pass can report no failure while still leaving excluded nodes pending."
    ]

statusHelp :: [Text]
statusHelp =
    [ "serve: status [--select PATTERN]... [--exclude PATTERN]..."
    , ""
    , "  Lists every node this world has ever seen, unified by Ref across every seed that shares"
    , "  it, with its wanted direction (up/down) and convergence (Pending/Converged/Errored/"
    , "  Blocked)."
    , ""
    , "  With no flags at all, lists everything, exactly as before --select/--exclude existed"
    , "  (including nodes still on their way down). With --select/--exclude given, narrows the"
    , "  listing to the resolved selection (see `help select`) — this can, unlike the unfiltered"
    , "  form, only show nodes belonging to a currently active seed."
    ]

historyHelp :: [Text]
historyHelp =
    [ "serve: history [--select PATTERN]... [--exclude PATTERN]..."
    , ""
    , "  Lists every declaration ever made (an append-only log — a retired seed's epoch is kept,"
    , "  since its graph is the only remaining description of how to tear it down), newest last,"
    , "  each tagged [active]/[retired] and showing the original argv (or, for a directive-file"
    , "  declaration, the file path)."
    , ""
    , "  With --select/--exclude, only epochs that declared at least one node in the resolved"
    , "  selection (see `help select`) are shown."
    ]

queryHelp :: [Text]
queryHelp =
    [ "serve: query [--select PATTERN]... [--exclude PATTERN]..."
    , ""
    , "  Lists every node (like a plain `status`), annotating each one [selected] or [excluded]"
    , "  against the resolved selection (see `help select`), without acting on anything — no"
    , "  convergence pass runs. Useful for checking what a `converge --select/--exclude` would"
    , "  touch before actually running it."
    ]

selectHelp :: [Text]
selectHelp =
    [ "serve: --select PATTERN / --exclude PATTERN"
    , ""
    , "  Shared by `converge`/`status`/`history`/`query`. A PATTERN is a /-separated glob over a"
    , "  node's tree position (the same path `run tree`/`run dag` print): a plain segment must"
    , "  match literally, `*` matches exactly one segment, `**` matches any number of segments"
    , "  (including zero), so `**` alone matches everything."
    , ""
    , "  Both flags may repeat; each is unioned with itself first. The selected set is every node"
    , "  matching some --select pattern (or, if --select is omitted entirely, every node), minus"
    , "  every node matching some --exclude pattern."
    , ""
    , "  The same node (a shared predecessor, e.g. a directory two files sit in) can occur at"
    , "  several paths; matching any one of them is enough to select or exclude it."
    ]

-------------------------------------------------------------------------------

{- | Read declarations from a handle until EOF (or @quit@), converging after
each one, and hand back the 'World' as it stands when the loop ends. Never
tears anything down on its way out: exiting the loop leaves the machine as
the last convergence left it.
-}
serve ::
    forall seed directive.
    (ToJSON directive, FromJSON directive) =>
    -- | loop-level events
    Reporter Report ->
    -- | per-node events, same reporter @run up@ uses
    Reporter (UpDown.Report Extension) ->
    -- | parses a seed out of one declaration's arguments
    ([String] -> Either Text seed) ->
    Configure IO seed directive ->
    Track' directive ->
    Handle ->
    IO (World seed directive)
serve r nodeReporter parseSeed configure program h = do
    world <- newIORef emptyWorld
    runReporter r Started
    loop world
    readIORef world
  where
    nat = pure . runIdentity

    -- | Deepest chain of nested @load@s allowed, to bound a self-referential
    -- (or mutually-referential) load file rather than looping forever.
    maxLoadDepth :: Int
    maxLoadDepth = 8

    loop :: IORef (World seed directive) -> IO ()
    loop world = do
        eof <- hIsEOF h
        if eof
            then runReporter r Stopped
            else do
                line <- hGetLine h
                keepGoing <- step 0 world line
                when keepGoing (loop world)

    step :: Int -> IORef (World seed directive) -> String -> IO Bool
    step depth world line =
        case parseServeCommand line of
            Left err -> do
                runReporter r (BadCommand err)
                pure True
            Right cmd ->
                case cmd of
                    Noop -> pure True
                    Quit -> pure False
                    Help mtopic -> do
                        runReporter r (HelpText mtopic)
                        pure True
                    Status sel -> do
                        w <- readIORef world
                        runReporter r (StatusReport (filterNodes w sel))
                        pure True
                    History sel -> do
                        w <- readIORef world
                        let (selr, excr) = resolveWorldSelectors w sel
                        let allowed = selr `Set.difference` excr
                        let matches :: Epoch seed directive -> Bool
                            matches ep = sel == noSelection || not (Set.null (Set.intersection (Map.keysSet ep.epochRefs) allowed))
                        runReporter r (HistoryReport (historyLinesMatching matches w))
                        pure True
                    QueryCmd sel -> do
                        w <- readIORef world
                        let (selr, excr) = resolveWorldSelectors w sel
                        runReporter r (QueryReport (Map.toList w.worldNodes) selr excr)
                        pure True
                    Converge sel -> do
                        restriction <-
                            if sel == noSelection
                                then pure Nothing
                                else do
                                    w <- readIORef world
                                    let (selr, excr) = resolveWorldSelectors w sel
                                    pure (Just (selr `Set.difference` excr))
                        converge world restriction
                        pure True
                    Clear -> do
                        w <- readIORef world
                        writeIORef world (retune w{worldActive = Map.empty})
                        runReporter r (Cleared (Map.size w.worldActive))
                        converge world Nothing
                        pure True
                    Declare decl args -> do
                        declare world decl args
                        pure True
                    DeclareDirective decl path -> do
                        declareDirective world decl path
                        pure True
                    Load path -> loadFile world (depth + 1) path

    -- | Filters 'worldNodes' by a 'Selection', preserving today's exact
    -- unfiltered listing (including nodes wanted 'TurnDown') when no
    -- @--select@\/@--exclude@ was given at all.
    filterNodes :: World seed directive -> Selection -> [(Ref, NodeState)]
    filterNodes w sel
        | sel == noSelection = Map.toList w.worldNodes
        | otherwise =
            let (selr, excr) = resolveWorldSelectors w sel
                allowed = selr `Set.difference` excr
             in [(rf, st) | (rf, st) <- Map.toList w.worldNodes, rf `Set.member` allowed]

    loadFile :: IORef (World seed directive) -> Int -> FilePath -> IO Bool
    loadFile world depth path
        | depth > maxLoadDepth = do
            runReporter r (BadLoad ("refusing to load " <> Text.pack path <> ": nesting too deep (possible cycle)"))
            pure True
        | otherwise = do
            runReporter r (Loading path)
            result <- try (readFile path) :: IO (Either IOException String)
            case result of
                Left ex -> do
                    runReporter r (BadLoad ("cannot read " <> Text.pack path <> ": " <> Text.pack (show ex)))
                    pure True
                Right contents -> go 0 (lines contents)
      where
        go n [] = do
            runReporter r (LoadDone path n)
            pure True
        go n (ln : rest) = do
            keepGoing <- step depth world ln
            if keepGoing then go (n + 1) rest else pure False

    declare :: IORef (World seed directive) -> Declaration -> [String] -> IO ()
    declare world decl args =
        case parseSeed args of
            Left err -> runReporter r (BadSeed err)
            Right seed -> do
                directive <- gen configure seed
                w0 <- readIORef world
                let o = run program directive
                let gr = evalDeps o
                let ep =
                        Epoch
                            { epochId = EpochId w0.worldNextId
                            , epochDeclaration = decl
                            , epochDirection = declarationDirection decl
                            , epochTokens = args
                            , epochSeed = Just seed
                            , epochDirective = directive
                            , epochKey = encode directive
                            , epochOp = o
                            , epochGraph = gr
                            , epochRefs = graphRefs gr
                            }
                commitEpoch world w0 decl ep

    declareDirective :: IORef (World seed directive) -> Declaration -> FilePath -> IO ()
    declareDirective world decl path = do
        result <- try (LByteString.readFile path) :: IO (Either IOException ByteString)
        case result of
            Left ex -> runReporter r (BadDirective ("cannot read " <> Text.pack path <> ": " <> Text.pack (show ex)))
            Right bytes ->
                case eitherDecode bytes of
                    Left err -> runReporter r (BadDirective (Text.pack err))
                    Right directive -> do
                        w0 <- readIORef world
                        let o = run program directive
                        let gr = evalDeps o
                        let ep =
                                Epoch
                                    { epochId = EpochId w0.worldNextId
                                    , epochDeclaration = decl
                                    , epochDirection = declarationDirection decl
                                    , epochTokens = ["<directive-file>", path]
                                    , epochSeed = Nothing
                                    , epochDirective = directive
                                    , epochKey = encode directive
                                    , epochOp = o
                                    , epochGraph = gr
                                    , epochRefs = graphRefs gr
                                    }
                        commitEpoch world w0 decl ep

    -- | Appends and records a freshly-built epoch, then converges (fully:
    -- a declaration is never itself scoped by a 'Selection').
    commitEpoch :: IORef (World seed directive) -> World seed directive -> Declaration -> Epoch seed directive -> IO ()
    commitEpoch world w0 decl ep = do
        let w1 = retune (record decl ep w0)
        writeIORef world w1
        runReporter r $
            Declared
                ep.epochId
                ep.epochDirection
                (Map.size ep.epochRefs)
                (Map.size w1.worldActive)
        converge world Nothing

    -- | Runs one down-then-up convergence pass. @restriction@, when
    -- present, additionally 'Skippable'-gates any node whose 'Ref' isn't in
    -- it — used only by an explicit @converge --select\/--exclude@; the
    -- auto-converge that follows every declaration always passes 'Nothing'.
    converge :: IORef (World seed directive) -> Maybe (Set Ref) -> IO ()
    converge world restriction = do
        w <- readIORef world
        let (nup, ndown) = pendingCounts w
        runReporter r (ConvergeStart ndown nup)
        -- teardown first: a node being replaced by an incompatible one
        -- (different content, hence a different 'Ref') has to go before its
        -- successor is brought up.
        okDown <-
            if ndown == 0
                then pure True
                else
                    UpDown.downTreeWith
                        (gateFor world TurnDown restriction)
                        (recorder world TurnDown restriction)
                        nat
                        (forest (downOps w))
        okUp <-
            if nup == 0
                then pure True
                else
                    UpDown.upTreeWith
                        (gateFor world TurnUp restriction)
                        (recorder world TurnUp restriction)
                        nat
                        (forest (upOps w))
        w' <- readIORef world
        let (rup, rdown) = pendingCounts w'
        runReporter r (ConvergeStop (okDown && okUp) (rup + rdown))

    {- | Only touch what this pass is for: a node wanted the other way (it
    belongs to some other seed), already converged, or excluded by this
    pass's own 'restriction' (an explicit @converge --select\/--exclude@) is
    left alone. -}
    gateFor :: IORef (World seed directive) -> Direction -> Maybe (Set Ref) -> UpDown.Gate Extension
    gateFor world dir restriction = \act -> do
        w <- readIORef world
        pure $ case Map.lookup act.extension.ref w.worldNodes of
            Nothing -> Skippable
            Just st
                | st.nodeDirection /= dir -> Skippable
                | st.nodeConvergence == Converged -> Skippable
                | maybe False (Set.notMember act.extension.ref) restriction -> Skippable
                | otherwise -> Required

    recorder :: IORef (World seed directive) -> Direction -> Maybe (Set Ref) -> Reporter (UpDown.Report Extension)
    recorder world dir restriction = reportBoth (stateWriter world dir restriction) nodeReporter

    {- 'upTree'/'downTree' report an 'Eval' before running a node and, once
    it returns, exactly one of 'Done' (succeeded) or 'Failed' (threw) — so
    recording convergence off 'Done'/'Failed' rather than 'Eval' is exact. A
    'Skip' is either this pass's own gate (already converged, not ours — both
    fine to record as converged, the direction check below drops the latter
    — or restricted out by an explicit @converge --select\/--exclude@, which
    must leave the node's actual convergence untouched so a later
    unrestricted @converge@ still retries it) or, on the way up, the node's
    own 'prelim' saying its effect is already in place, which is convergence
    too. -}
    stateWriter :: IORef (World seed directive) -> Direction -> Maybe (Set Ref) -> Reporter (UpDown.Report Extension)
    stateWriter world dir restriction = ReporterM $ \rep ->
        case rep of
            UpDown.Eval _ -> pure ()
            UpDown.Done act -> mark act Converged
            UpDown.Skip act
                | maybe False (Set.notMember act.extension.ref) restriction -> pure ()
                | otherwise -> mark act Converged
            UpDown.Failed act _ -> mark act Errored
            UpDown.Blocked act -> mark act Blocked
            UpDown.Redundant _ -> pure ()
      where
        mark :: Act Extension -> Convergence -> IO ()
        mark act c = modifyIORef' world (setConvergence dir act.extension.ref c)

-------------------------------------------------------------------------------

-- | Appends the epoch to the history and applies it to the active set.
record :: Declaration -> Epoch seed directive -> World seed directive -> World seed directive
record decl ep w =
    w
        { worldNextId = w.worldNextId + 1
        , worldHistory = ep : w.worldHistory
        , worldActive = case decl of
            Add -> Map.insert ep.epochKey ep.epochId w.worldActive
            Replace -> Map.singleton ep.epochKey ep.epochId
            Remove -> Map.delete ep.epochKey w.worldActive
        }

{- | Re-derives every node's wanted 'Direction' from the active seeds. A node
whose direction is unchanged keeps its 'Convergence'; one that just flipped
goes back to 'Pending', because whatever was done to it was done the other way.
-}
retune :: World seed directive -> World seed directive
retune w =
    w{worldNodes = Map.mapWithKey adjust known}
  where
    activeIds :: Set EpochId
    activeIds = Set.fromList (Map.elems w.worldActive)

    desired :: Set Ref
    desired =
        Set.unions
            [ Map.keysSet ep.epochRefs
            | ep <- w.worldHistory
            , Set.member ep.epochId activeIds
            ]

    -- every node ever seen; history is newest-first and 'Map.unions' is
    -- left-biased, so a node's metadata comes from the latest graph it was in.
    known :: Map Ref (EpochId, ShortHand, Text)
    known =
        Map.unions
            [ fmap (\(sh, hlp) -> (ep.epochId, sh, hlp)) ep.epochRefs
            | ep <- w.worldHistory
            ]

    adjust r (eid, sh, hlp) =
        let dir = if Set.member r desired then TurnUp else TurnDown
         in case Map.lookup r w.worldNodes of
                Just st
                    | st.nodeDirection == dir ->
                        st{nodeShorthand = sh, nodeHelp = hlp, nodeEpoch = eid}
                _ -> NodeState sh hlp dir Pending eid

setConvergence :: Direction -> Ref -> Convergence -> World seed directive -> World seed directive
setConvergence dir r c w =
    w{worldNodes = Map.adjust upd r w.worldNodes}
  where
    upd st
        | st.nodeDirection == dir = st{nodeConvergence = c}
        | otherwise = st

-- | (nodes wanted up, nodes wanted down) that have not converged yet.
pendingCounts :: World seed directive -> (Int, Int)
pendingCounts w =
    (count TurnUp, count TurnDown)
  where
    count dir = length [() | st <- Map.elems w.worldNodes, st.nodeDirection == dir, st.nodeConvergence /= Converged]

-- | Graphs of the active seeds: what the up pass walks.
upOps :: World seed directive -> [Op]
upOps w =
    [ ep.epochOp
    | ep <- w.worldHistory
    , Set.member ep.epochId activeIds
    ]
  where
    activeIds = Set.fromList (Map.elems w.worldActive)

{- | Graphs holding at least one node still to be torn down: what the down
pass walks. Epochs whose nodes are all converged (or all still wanted up)
are left out, so a long history does not make every pass more expensive.
-}
downOps :: World seed directive -> [Op]
downOps w =
    [ ep.epochOp
    | ep <- w.worldHistory
    , any needsDown (Map.keys ep.epochRefs)
    ]
  where
    needsDown r =
        case Map.lookup r w.worldNodes of
            Just st -> st.nodeDirection == TurnDown && st.nodeConvergence /= Converged
            Nothing -> False

{- | Bundles several graphs under one 'Actionless' root, which 'upTree' and
'downTree' walk through without treating it as a node of its own.
-}
forest :: [Op] -> Op
forest ops = OpGraph (deps ops) Actionless

historyLines :: World seed directive -> [(EpochId, Declaration, Bool, [String])]
historyLines = historyLinesMatching (const True)

-- | Like 'historyLines', but only for epochs satisfying a predicate.
historyLinesMatching ::
    (Epoch seed directive -> Bool) ->
    World seed directive ->
    [(EpochId, Declaration, Bool, [String])]
historyLinesMatching p w =
    [ (ep.epochId, ep.epochDeclaration, Set.member ep.epochId activeIds, ep.epochTokens)
    | ep <- reverse w.worldHistory
    , p ep
    ]
  where
    activeIds = Set.fromList (Map.elems w.worldActive)

{- | Resolves a 'Selection' against every currently-/active/ epoch's graph,
unioning the per-epoch @(selected, excluded)@ pairs 'Query.resolveSelectors'
returns — there is no single unified cograph for the whole 'World', only the
unified 'worldNodes' map. An empty 'selSelect' still resolves to "everything"
per epoch, so the union over active epochs is exactly every active node,
mirroring 'retune''s own @desired@ computation.
-}
resolveWorldSelectors :: World seed directive -> Selection -> (Set Ref, Set Ref)
resolveWorldSelectors w sel =
    (Set.unions (map fst perEpoch), Set.unions (map snd perEpoch))
  where
    activeIds = Set.fromList (Map.elems w.worldActive)
    perEpoch =
        [ Query.resolveSelectors ep.epochGraph sel.selSelect sel.selExclude
        | ep <- w.worldHistory
        , Set.member ep.epochId activeIds
        ]

graphRefs :: Cofree Graph Op -> Map Ref (ShortHand, Text)
graphRefs gr =
    Map.fromList
        [ (a.extension.ref, (a.shorthand, a.extension.help))
        | o <- toList gr
        , Just a <- [opAct o]
        ]
