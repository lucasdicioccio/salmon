{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | The three report streams as one, and as JSON.

A salmon binary reports through three vocabularies: 'UpDown.Report' (what a
node did, from the one-shot drivers and, wrapped in 'Upkeep.Acted', from the
tending loop), 'Upkeep.Report' (what a node's own machine is doing between
commands) and 'Serve.Report' (what the @run serve@ loop is doing). Each has
its own text rendering, and each is emitted through its own
'Reporter'. 'Tagged' is the sum of the three, tagged by origin, so that one
'Reporter' 'Tagged' can be split contravariantly into the three the drivers
expect ('serveStream'/'updownStream'/'upkeepStream' are the 'contramap's) and
so that a second consumer — a JSON line writer today, a server later (see
@specs\/generic-server.md@) — sees every event in one place, composed beside
the text one with 'reportBoth' rather than as a second reporting mechanism.

The 'ToJSON' instances live here rather than beside the types for one
reason: the two parametric streams are only encodable at 'Extension', which
"Salmon.Actions.UpDown" cannot import (it is what "Salmon.Builtin.Extension"
imports). Keeping all three together, orphans included, also makes this the
one module a client reads to know the wire format.

The format: every report is an object with a @kind@, a @ref@ (an object with
the 'shortRef' and the full text) wherever there is one node the report is
about, and named fields. A report that nests another stream's report
('Upkeep.Acted', 'Serve.Tended') nests the inner object as-is under
@report@. 'Tagged' adds @origin@ to the object. Report text — @help@,
@notes@, failure text — is public and encoded verbatim; see the spec's
decisions. No sequence numbers yet.
-}
module Salmon.Reporter.Tagged (
    -- * The sum
    Tagged (..),
    serveStream,
    updownStream,
    upkeepStream,

    -- * Reporters
    reportJSONLines,
    reportTexts,

    -- * Encoding pieces
    refValue,
    actPairs,
    checkResultValue,
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

-- | One of the three streams, tagged by where it came from.
data Tagged
    = FromServe !Serve.Report
    | FromUpDown !(UpDown.Report Extension)
    | FromUpkeep !(Upkeep.Report Extension)
    deriving (Show)

serveStream :: Reporter Tagged -> Reporter Serve.Report
serveStream = contramap FromServe

updownStream :: Reporter Tagged -> Reporter (UpDown.Report Extension)
updownStream = contramap FromUpDown

upkeepStream :: Reporter Tagged -> Reporter (Upkeep.Report Extension)
upkeepStream = contramap FromUpkeep

{- | The three text reporters, behind one 'Tagged' one. Dispatches and does
nothing else, so whatever each of the three prints, it prints unchanged —
this is what a binary's own reporters go through when @--json@ is absent.
-}
reportTexts ::
    Reporter Serve.Report ->
    Reporter (UpDown.Report Extension) ->
    Reporter (Upkeep.Report Extension) ->
    Reporter Tagged
reportTexts serveR updownR upkeepR = ReporterM $ \tagged ->
    case tagged of
        FromServe rep -> runReporter serveR rep
        FromUpDown rep -> runReporter updownR rep
        FromUpkeep rep -> runReporter upkeepR rep

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
      where
        withOrigin :: Text -> Value -> Value
        withOrigin origin (Object o) = Object (KeyMap.insert "origin" (String origin) o)
        -- every instance below produces an object; kept total rather than
        -- partial so a future non-object encoding degrades to a wrapper
        -- instead of a crash in a reporter.
        withOrigin origin v = object ["origin" .= origin, "report" .= v]

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
            -- @from@ rather than @origin@: 'Tagged' already puts the stream's
            -- name under that key.
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
            Serve.Tended inner -> [kind "tended", "report" .= inner]
            Serve.ConvergeStart ndown nup -> [kind "converge-start", "down" .= ndown, "up" .= nup]
            Serve.ConvergeStop ok remaining -> [kind "converge-stop", "ok" .= ok, "remaining" .= remaining]
            Serve.StatusReport xs paths ->
                [kind "status", "nodes" .= fmap (nodeStateValue paths Nothing) xs]
            Serve.HistoryReport xs -> [kind "history", "seeds" .= fmap epochValue xs]
            Serve.HistoryElided n -> [kind "history-elided", "elided" .= n]
            Serve.QueryReport xs sel exc paths ->
                [kind "query", "nodes" .= fmap (nodeStateValue paths (Just (sel, exc))) xs]
            -- the topic asked for, and the same lines the text reporter
            -- would print: the reference is prose, and there is nothing
            -- more structured to say about it.
            Serve.HelpText mtopic -> [kind "help", "topic" .= mtopic, "lines" .= Serve.renderReport rep]
      where
        nodeStateValue :: Map Ref [Text] -> Maybe (Set Ref, Set Ref) -> (Ref, Serve.NodeState) -> Value
        nodeStateValue paths selection (r, st) =
            object $
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

        epochValue :: (Serve.EpochId, Serve.Declaration, Bool, Serve.Origin, [String]) -> Value
        epochValue (eid, decl, active, origin, args) =
            object
                [ "epoch" .= eid.unEpochId
                , "declaration" .= declaration decl
                , "active" .= active
                , "origin" .= originValue origin
                , "args" .= args
                ]

        -- who made the declaration: the same distinction the text `history`
        -- draws with its trailing `[fetched ...]`/`[loaded ...]` annotation.
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

        -- the input-language word, the same one 'Serve.renderReport' prints
        declaration :: Serve.Declaration -> Text
        declaration Serve.Add = "up"
        declaration Serve.Replace = "only"
        declaration Serve.Remove = "down"
