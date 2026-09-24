{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | The four report streams as one, and as JSON.

A salmon binary reports through four vocabularies: 'UpDown.Report' (what a
node did, from the one-shot drivers and, wrapped in 'Upkeep.Acted', from the
tending loop), 'Upkeep.Report' (what a node's own machine is doing between
commands), 'Serve.Report' (what the @run serve@ loop is doing) and
'Follow.Report' (what the fetcher of @run serve --follow@ is doing on its own
thread). Each has its own text rendering, and each is emitted through its own
'Reporter'. 'Tagged' is the sum of the four, tagged by stream, so that one
'Reporter' 'Tagged' can be split contravariantly into the four the drivers
expect ('serveStream'/'updownStream'/'upkeepStream'/'followStream' are the
'contramap's) and so that a second consumer — a JSON line writer, a status
sink ("Salmon.Actions.Serve.StatusSink"), a server (see
@specs\/generic-server.md@) — sees every event in one place, composed beside
the text one with 'reportBoth' rather than as a second reporting mechanism.

The 'ToJSON' instances live here rather than beside the types for one
reason: the two parametric streams are only encodable at 'Extension', which
"Salmon.Actions.UpDown" cannot import (it is what "Salmon.Builtin.Extension"
imports). Keeping all four together, orphans included, also makes this the
one module a client reads to know the wire format.

The format: every report is an object with a @kind@, a @ref@ (an object with
the 'shortRef' and the full text) wherever there is one node the report is
about, and named fields. A report that nests another stream's report
('Upkeep.Acted', 'Serve.Tended') nests the inner object as-is under
@report@. 'Tagged' adds @stream@ to the object — @serve@, @updown@,
@upkeep@ or @follow@; the key is not @origin@ because that word names who typed a
command (see 'Serve.Origin'), which the event stream will carry too. Report
text — @help@,
@notes@, failure text — is public and encoded verbatim; see the spec's
decisions. Sequence numbers are added on the event stream alone, by
"Salmon.Actions.Serve.Events".
-}
module Salmon.Reporter.Tagged (
    -- * The sum
    Tagged (..),
    serveStream,
    updownStream,
    upkeepStream,
    followStream,

    -- * Reporters
    reportJSONLines,
    reportTexts,

    -- * Encoding pieces
    refValue,
    actPairs,
    checkResultValue,
    nodeStatePairs,
    nodeStateValue,
    epochValue,
    originValue,
) where

import Control.Exception (SomeException)
import Data.Aeson (Key, ToJSON (..), Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (Handle, hFlush)

import qualified Salmon.Actions.Follow as Follow
import qualified Salmon.Actions.Follow.Scheduler as Scheduler
import qualified Salmon.Actions.Serve as Serve
import qualified Salmon.Actions.UpDown as UpDown
import qualified Salmon.Actions.Upkeep as Upkeep
import Salmon.Builtin.Extension (Extension (..))
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Mailbox as Mailbox
import Salmon.Op.Ref (Ref, shortRef, unRef)
import qualified Salmon.Op.Status as Status
import Salmon.Op.Supervision (Micros (..), Restart (..), Strategy (..), Supervision (..))
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | One of the four streams, tagged by where it came from.
data Tagged
    = FromServe !Serve.Report
    | FromUpDown !(UpDown.Report Extension)
    | FromUpkeep !(Upkeep.Report Extension)
    | FromFollow !Follow.Report
    deriving (Show)

serveStream :: Reporter Tagged -> Reporter Serve.Report
serveStream = contramap FromServe

updownStream :: Reporter Tagged -> Reporter (UpDown.Report Extension)
updownStream = contramap FromUpDown

upkeepStream :: Reporter Tagged -> Reporter (Upkeep.Report Extension)
upkeepStream = contramap FromUpkeep

followStream :: Reporter Tagged -> Reporter Follow.Report
followStream = contramap FromFollow

{- | The four text reporters, behind one 'Tagged' one. Dispatches and does
nothing else, so whatever each of the four prints, it prints unchanged —
this is what a binary's own reporters go through when @--json@ is absent.
-}
reportTexts ::
    Reporter Serve.Report ->
    Reporter (UpDown.Report Extension) ->
    Reporter (Upkeep.Report Extension) ->
    Reporter Follow.Report ->
    Reporter Tagged
reportTexts serveR updownR upkeepR followR = ReporterM $ \tagged ->
    case tagged of
        FromServe rep -> runReporter serveR rep
        FromUpDown rep -> runReporter updownR rep
        FromUpkeep rep -> runReporter upkeepR rep
        FromFollow rep -> runReporter followR rep

{- | One JSON object per line, flushed as it is written so a consumer on the
other end of a pipe (@| jq@) sees each report when it happens rather than
when the buffer fills. Each line is handed to the handle as one strict
chunk: the concurrent drivers already serialise 'runReporter' through an
'Control.Concurrent.MVar.MVar', and a single write on top of that is what
keeps two reports from interleaving inside a line.
-}
reportJSONLines :: Handle -> Reporter Tagged
reportJSONLines h = ReporterM $ \tagged -> do
    LByteString.hPut h (LByteString.fromStrict (LByteString.toStrict (Aeson.encode tagged <> "\n")))
    hFlush h

-------------------------------------------------------------------------------

instance ToJSON Tagged where
    toJSON tagged =
        case tagged of
            FromServe rep -> withOrigin "serve" (toJSON rep)
            FromUpDown rep -> withOrigin "updown" (toJSON rep)
            FromUpkeep rep -> withOrigin "upkeep" (toJSON rep)
            FromFollow rep -> withOrigin "follow" (toJSON rep)
      where
        withOrigin :: Text -> Value -> Value
        withOrigin origin (Object o) = Object (KeyMap.insert "stream" (String origin) o)
        -- every instance below produces an object; kept total rather than
        -- partial so a future non-object encoding degrades to a wrapper
        -- instead of a crash in a reporter.
        withOrigin origin v = object ["stream" .= origin, "report" .= v]

-------------------------------------------------------------------------------

-- | A 'Ref' as both its short tag and its full text.
refValue :: Ref -> Value
refValue r = object ["short" .= shortRef r, "full" .= unRef r]

-- | The fields every report about one node carries: its @ref@ at the top
-- level, and what the node says it is under @node@.
actPairs :: Act Extension -> [(Key, Value)]
actPairs act =
    [ "ref" .= refValue act.extension.ref
    , "node" .= nodeValue act
    ]

nodeValue :: Act Extension -> Value
nodeValue act =
    object
        [ "shorthand" .= act.shorthand
        , "help" .= act.extension.help
        , "notes" .= act.extension.notes
        ]

kind :: Text -> (Key, Value)
kind k = "kind" .= k

exceptionValue :: SomeException -> Value
exceptionValue = String . Text.pack . show

checkResultValue :: UpDown.CheckResult -> Value
checkResultValue cr =
    case cr of
        UpDown.Success -> verdict "success" []
        UpDown.Skipped -> verdict "skipped" []
        UpDown.Completed -> verdict "completed" []
        UpDown.Failure reason -> verdict "failure" ["reason" .= reason]
        UpDown.Unknown -> verdict "unknown" []
        UpDown.Immaterial -> verdict "immaterial" []
  where
    verdict :: Text -> [(Key, Value)] -> Value
    verdict v rest = object (("verdict" .= v) : rest)

instructionValue :: Mailbox.Instruction -> Value
instructionValue instr =
    String $ case instr of
        Mailbox.Force -> "force"
        Mailbox.Satisfy -> "satisfy"
        Mailbox.Recheck -> "recheck"
        Mailbox.Pause -> "pause"
        Mailbox.Resume -> "resume"

microsValue :: Micros -> Value
microsValue (Micros us) = toJSON us

directionValue :: Status.Direction -> Value
directionValue Status.TurnUp = "up"
directionValue Status.TurnDown = "down"

stabilityValue :: Status.Stability -> Value
stabilityValue Status.Stable = "stable"
stabilityValue Status.Transient = "transient"

supervisionValue :: Supervision -> Value
supervisionValue sup =
    object
        [ "restart" .= restart sup.supRestart
        , "strategy" .= strategy sup.supStrategy
        , "reapply" .= sup.supReapply
        , "watchdog_us" .= fmap microsValue sup.supWatchdog
        , "stable_after_us" .= microsValue sup.supStableAfter
        , "demote_every_us" .= microsValue sup.supDemoteEvery
        , "give_up_after" .= sup.supGiveUpAfter
        ]
  where
    restart :: Restart -> Text
    restart Always = "always"
    restart OnFailure = "on-failure"
    restart Never = "never"
    strategy :: Strategy -> Text
    strategy OneForOne = "one-for-one"
    strategy RestForOne = "rest-for-one"

{- | The loop's mode, as @status@ renders it: @interactive@, @following@
or @replay@. On the wire in two places (@status@\'s object and @\/dag@\'s
envelope), so it is encoded once, here, beside the other orphans.
-}
instance ToJSON Serve.Mode where
    toJSON = String . Serve.renderMode

-------------------------------------------------------------------------------

instance ToJSON (UpDown.Report Extension) where
    toJSON rep =
        object $ case rep of
            UpDown.Skip act -> kind "skip" : actPairs act
            UpDown.Eval act -> kind "eval" : actPairs act
            UpDown.Done act -> kind "done" : actPairs act
            UpDown.Failed act e -> kind "failed" : actPairs act ++ ["error" .= exceptionValue e]
            UpDown.Blocked act -> kind "blocked" : actPairs act
            UpDown.Conflicting r kept replaced ->
                [ kind "conflicting"
                , "ref" .= refValue r
                , "kept" .= nodeValue kept
                , "replaced" .= nodeValue replaced
                ]
            UpDown.Instructed act instr -> kind "instructed" : actPairs act ++ ["instruction" .= instructionValue instr]
            UpDown.DroppedInstructions act n -> kind "dropped-instructions" : actPairs act ++ ["dropped" .= n]

-------------------------------------------------------------------------------

instance ToJSON (Upkeep.Report Extension) where
    toJSON rep =
        object $ case rep of
            Upkeep.Acted inner -> [kind "acted", "report" .= inner]
            Upkeep.Upkeep act st -> kind "upkeep" : actPairs act ++ ["state" .= upkeepState st]
            Upkeep.Downkeep act st -> kind "downkeep" : actPairs act ++ ["state" .= downkeepState st]
            Upkeep.NextLook act cr delay ->
                kind "next-look" : actPairs act ++ ["check" .= checkResultValue cr, "delay_us" .= microsValue delay]
            Upkeep.Wedged act silent -> kind "wedged" : actPairs act ++ ["silent_us" .= microsValue silent]
            Upkeep.Unwedged act -> kind "unwedged" : actPairs act
            Upkeep.Demoted act dep -> kind "demoted" : actPairs act ++ ["dependency" .= refValue dep]
            Upkeep.Parked act -> kind "parked" : actPairs act
            Upkeep.Reapplying act delay -> kind "reapplying" : actPairs act ++ ["delay_us" .= microsValue delay]
            Upkeep.Paused act -> kind "paused" : actPairs act
            Upkeep.Resumed act -> kind "resumed" : actPairs act
            Upkeep.GaveUp act n -> kind "gave-up" : actPairs act ++ ["failures" .= n]
            Upkeep.Adopted act -> kind "adopted" : actPairs act
            Upkeep.Released act -> kind "released" : actPairs act
            Upkeep.Policy act sup ignored ->
                kind "policy" : actPairs act ++ ["supervision" .= supervisionValue sup, "ignored" .= fmap supervisionValue ignored]
            Upkeep.Untended act -> kind "untended" : actPairs act
            Upkeep.Escaped act e -> kind "escaped" : actPairs act ++ ["error" .= exceptionValue e]
            Upkeep.Supervising nup ndown -> [kind "supervising", "up" .= nup, "down" .= ndown]
            Upkeep.Retired n -> [kind "retired", "machines" .= n]
            Upkeep.Holding n -> [kind "holding", "machines" .= n]
      where
        upkeepState :: Upkeep.UpkeepState -> Text
        upkeepState Upkeep.WaitUp = "wait-up"
        upkeepState Upkeep.Upping = "upping"
        upkeepState Upkeep.Up = "up"
        downkeepState :: Upkeep.DownkeepState -> Text
        downkeepState Upkeep.WaitDown = "wait-down"
        downkeepState Upkeep.Downing = "downing"
        downkeepState Upkeep.Down = "down"

-------------------------------------------------------------------------------

instance ToJSON Serve.Report where
    toJSON rep =
        object $ case rep of
            Serve.Started -> [kind "started"]
            Serve.Stopped -> [kind "stopped"]
            Serve.HungUp origin -> [kind "hung-up", "from" .= Serve.originName origin]
            Serve.BadCommand err -> [kind "bad-command", "error" .= err]
            Serve.BadSeed err -> [kind "bad-seed", "error" .= err]
            Serve.BadDirective err -> [kind "bad-directive", "error" .= err]
            Serve.BadLoad err -> [kind "bad-load", "error" .= err]
            Serve.Loading path -> [kind "loading", "path" .= path]
            Serve.LoadDone path n -> [kind "load-done", "path" .= path, "lines" .= n]
            Serve.Declared eid dir nnodes nactive ->
                [ kind "declared"
                , "epoch" .= eid.unEpochId
                , "direction" .= directionValue dir
                , "nodes" .= nnodes
                , "active_seeds" .= nactive
                ]
            Serve.Cleared n -> [kind "cleared", "retired" .= n]
            Serve.Supervised on -> [kind "supervised", "on" .= on]
            Serve.AutoConverged on -> [kind "auto-converged", "on" .= on]
            Serve.Instructed instr n -> [kind "instructed", "instruction" .= instructionValue instr, "nodes" .= n]
            Serve.FetchRequested following -> [kind "fetch-requested", "following" .= following]
            Serve.Tended inner -> [kind "tended", "report" .= inner]
            Serve.ConvergeStart ndown nup -> [kind "converge-start", "down" .= ndown, "up" .= nup]
            Serve.ConvergeStop ok remaining -> [kind "converge-stop", "ok" .= ok, "remaining" .= remaining]
            Serve.StatusReport mode xs paths ->
                [kind "status", "mode" .= mode, "nodes" .= fmap (nodeStateValue paths Nothing) xs]
            Serve.HistoryReport xs -> [kind "history", "seeds" .= fmap epochValue xs]
            Serve.HistoryElided n -> [kind "history-elided", "elided" .= n]
            Serve.QueryReport xs sel exc paths ->
                [kind "query", "nodes" .= fmap (nodeStateValue paths (Just (sel, exc))) xs]
            -- the topic asked for, and the same lines the text reporter
            -- would print: the reference is prose, and there is nothing
            -- more structured to say about it.
            Serve.HelpText mtopic -> [kind "help", "topic" .= mtopic, "lines" .= Serve.renderReport rep]
            Serve.SinkFailed path err -> [kind "sink-failed", "path" .= path, "error" .= err]

-------------------------------------------------------------------------------

{- | The fetcher's stream. A label is its text, a digest its hex; the
schedule ('Scheduler.Config') is spelled out in microseconds, the unit the
scheduler itself keeps, under the same names the @--follow-*@ flags use.
-}
instance ToJSON Follow.Report where
    toJSON rep =
        object $ case rep of
            Follow.Following reg lbls cfg ->
                [kind "following", "registry" .= reg, "labels" .= fmap Follow.labelText lbls, "schedule" .= scheduleValue cfg]
            Follow.Injected lbl did dg nup ndown ->
                kind "injected" : labelled lbl ++ ["document" .= did, "sha256" .= dg.unDigest, "up" .= nup, "down" .= ndown]
            Follow.NoDiff lbl did dg -> kind "no-diff" : labelled lbl ++ ["document" .= did, "sha256" .= dg.unDigest]
            Follow.Deferred lbl did dg -> kind "deferred" : labelled lbl ++ ["document" .= did, "sha256" .= dg.unDigest]
            Follow.Backoff n us -> [kind "backoff", "failures" .= n, "next_us" .= us]
            Follow.Missing lbl -> kind "missing" : labelled lbl
            Follow.Vanished lbl -> kind "vanished" : labelled lbl
            Follow.Malformed lbl dg err -> kind "malformed" : labelled lbl ++ ["sha256" .= dg.unDigest, "error" .= err]
            Follow.FetchFailed lbl err -> kind "fetch-failed" : labelled lbl ++ ["error" .= err]
            Follow.Replayed lbl did dg -> kind "replayed" : labelled lbl ++ ["document" .= did, "sha256" .= dg.unDigest]
            Follow.Stale lbl did -> kind "stale" : labelled lbl ++ ["document" .= did]
            Follow.BadCache lbl err -> kind "bad-cache" : labelled lbl ++ ["error" .= err]
            Follow.Rejected lbl dg err -> kind "rejected" : labelled lbl ++ ["sha256" .= dg.unDigest, "reason" .= err]
            Follow.CacheFailed lbl err -> kind "cache-failed" : labelled lbl ++ ["error" .= err]
      where
        labelled :: Follow.Label -> [(Key, Value)]
        labelled lbl = ["label" .= Follow.labelText lbl]

scheduleValue :: Scheduler.Config -> Value
scheduleValue cfg =
    object
        [ "base_us" .= cfg.schedBase
        , "factor" .= cfg.schedFactor
        , "cap_us" .= cfg.schedCap
        , "jitter" .= cfg.schedJitter
        , "debounce_us" .= cfg.schedDebounce
        , "max_wait_us" .= cfg.schedMaxWait
        ]

-------------------------------------------------------------------------------

{- | A node as @status@\/@query@ list it: its 'Ref', what it is, which way it
is wanted, how far it has got, its machine's last snapshot, and the paths a
selector can name it by. The pairs rather than the object, so that a
consumer with more to say about the node — the @\/dag@ read in
"Salmon.Actions.Serve.Http", which adds its edges — extends the same
encoding rather than keeping a second one.
-}
nodeStatePairs :: Map Ref [Text] -> Maybe (Set Ref, Set Ref) -> (Ref, Serve.NodeState) -> [(Key, Value)]
nodeStatePairs paths selection (r, st) =
    [ "ref" .= refValue r
    , "shorthand" .= st.nodeShorthand
    , "help" .= st.nodeHelp
    , "direction" .= directionValue st.nodeDirection
    , "convergence" .= convergence st.nodeConvergence
    , "status" .= fmap statusValue st.nodeStatus
    , "paths" .= Map.findWithDefault [] r paths
    ]
        ++ case selection of
            Nothing -> []
            Just (sel, exc) ->
                [ "selected" .= (r `Set.member` sel)
                , "excluded" .= (r `Set.member` exc)
                ]

nodeStateValue :: Map Ref [Text] -> Maybe (Set Ref, Set Ref) -> (Ref, Serve.NodeState) -> Value
nodeStateValue paths selection = object . nodeStatePairs paths selection

convergence :: Serve.Convergence -> Text
convergence Serve.Pending = "pending"
convergence Serve.Stale = "stale"
convergence Serve.Converged = "converged"
convergence Serve.Errored = "errored"
convergence Serve.Blocked = "blocked"

-- the snapshot 'Serve.NodeState' keeps of a node's machine: the
-- clock reading is left out, since it is only meaningful against
-- a later reading of the same monotonic clock in the same process.
statusValue :: Status.Status -> Value
statusValue ms =
    object
        [ "check" .= checkResultValue ms.statusCheck
        , "direction" .= directionValue ms.statusDirection
        , "stability" .= stabilityValue ms.statusStability
        , "epoch" .= ms.statusEpoch
        , "output" .= Status.ringLines ms.statusOutput
        ]

-- | One line of @history@.
epochValue :: (Serve.EpochId, Serve.Declaration, Bool, Serve.Origin, [String]) -> Value
epochValue (eid, decl, active, origin, args) =
    object
        [ "epoch" .= eid.unEpochId
        , "declaration" .= declaration decl
        , "active" .= active
        , "origin" .= originValue origin
        , "args" .= args
        ]

-- the input-language word, the same one 'Serve.renderReport' prints
declaration :: Serve.Declaration -> Text
declaration Serve.Add = "up"
declaration Serve.Replace = "only"
declaration Serve.Remove = "down"

-- | Who made a declaration (or, on the event stream, typed a command):
-- the same distinction the text @history@ draws with its trailing
-- @[fetched ...]@\/@[loaded ...]@ annotation.
originValue :: Serve.Origin -> Value
originValue origin = case origin of
    Serve.Stdin -> object ["kind" .= ("stdin" :: Text)]
    Serve.Origin name -> object ["kind" .= ("other" :: Text), "name" .= name]
    Serve.Loaded path -> object ["kind" .= ("loaded" :: Text), "path" .= path]
    Serve.Fetched prov ->
        object
            [ "kind" .= ("fetched" :: Text)
            , "registry" .= prov.provRegistry
            , "label" .= prov.provLabel
            , "document" .= prov.provDocument
            , "sha256" .= prov.provDigest
            ]
