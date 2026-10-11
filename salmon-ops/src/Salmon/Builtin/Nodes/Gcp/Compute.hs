{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.Gcp.Compute (
    MachineType (..),
    renderMachineType,
    MachineTypeChange (..),
    MachineTypeVerdict (..),
    machineTypeName,
    parseLiveMachineType,
    compareMachineType,
    interpretInstanceMachineType,
    InstanceStep (..),
    instanceUpSteps,
    planExistingInstance,
    planAbsentInstance,
    RunLimit (..),
    LiveRunLimit (..),
    RunLimitVerdict (..),
    runLimitGrace,
    runLimitProblems,
    runLimitArgs,
    renderRunLimit,
    renderDeadline,
    parseLiveRunLimit,
    judgeRunLimit,
    interpretInstanceRun,
    BootDisk (..),
    Instance (..),
    InstancePower (..),
    ExternalAddress (..),
    InternalAddress (..),
    AccessScopes (..),
    scopeAliases,
    scopeUris,
    scopeProblems,
    declaredScopeUris,
    parseLiveScopes,
    interpretInstanceScopes,
    gceInstance,
    Address (..),
    AddressKind (..),
    address,
    readAddress,
    interpretAddressDescribe,
    interpretAddress,
    FirewallRule (..),
    firewallRule,
    interpretFirewallDescribe,
    SubnetPurpose (..),
    renderSubnetPurpose,
    Subnet (..),
    subnet,
    interpretSubnetDescribe,
    InstanceGroup (..),
    instanceGroup,
    interpretInstanceGroupDescribe,
    instanceGroupMember,
    interpretGroupMembership,
    interpretInstanceStatus,
    interpretInstancePresence,
    InstanceUpPlan (..),
    planInstanceUp,
    Report (..),
    ComputeCommand (..),
    computeCommand,
) where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime (zonedTimeToUTC)
import Text.Read (readMaybe)
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.IO.Error (userError)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..), Zone (..), gcloudProc, withProject, withZone)
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunComputeCommand !ComputeCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | GCE machine type.
data MachineType
    = E2Medium
    | E2Standard2
    | N2Standard4
    | Custom Text
    deriving (Eq, Show)

renderMachineType :: MachineType -> Text
renderMachineType E2Medium = "e2-medium"
renderMachineType E2Standard2 = "e2-standard-2"
renderMachineType N2Standard4 = "n2-standard-4"
renderMachineType (Custom t) = t

{- | What 'gceInstance' may do about an existing instance whose machine type
is not the declared one. GCP only changes a machine type on a stopped
instance, so reconciling one means an outage (about two minutes from stop to
serving, on the one resize this was written after), and that is the
declaration's to permit, never the node's to decide.

'RefuseMachineTypeChange' is what every declaration should start from: the
difference is a 'Failure' for the check and a throw for @up@, both naming
the running and the declared type, and nothing is stopped, started or
re-created. The remedy is then the operator's: stop it, @gcloud compute
instances set-machine-type NAME --machine-type T@, start it; or declare
'MayStopToResize'.

'MayStopToResize' lets @up@ do exactly that: stop the instance if it runs,
set the type, and start it again when the declaration is 'PoweredOn' (a
'PoweredOff' one stays stopped). It is a standing permission, not a one-off:
while it is declared, any later change of 'instanceMachineType' stops the
machine again, under @run serve@ as soon as the check notices.
-}
data MachineTypeChange
    = -- | fail the node, naming both types; never stop anything
      RefuseMachineTypeChange
    | -- | stop, @set-machine-type@, start
      MayStopToResize
    deriving (Eq, Show)

{- | The name a machine type goes by, whether it was written as a bare name
or as the URL (or partial path) GCP describes it with:
@.../zones/Z/machineTypes/e2-medium@ is @e2-medium@.
-}
machineTypeName :: Text -> Text
machineTypeName = Text.takeWhileEnd (/= '/') . Text.strip

{- | The machine type in the output of @describe
--format=value(machineType.basename())@: one word, read by its last path
segment so that the full URL (should the projection ever not apply) is the
same answer. Anything else -- no word, several, or one that does not look
like a machine type name -- is a 'Left' saying so, because under
'MayStopToResize' a misread output must never be taken for a difference
worth stopping a machine over.
-}
parseLiveMachineType :: Text -> Either Text Text
parseLiveMachineType out =
    case Text.words out of
        [word]
            | name <- machineTypeName word
            , not (Text.null name)
            , Text.all isNameChar name ->
                Right name
        _ -> Left ("no machine type could be read in " <> Text.pack (show out))
  where
    isNameChar c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-'

-- | An existing instance's machine type against the declared one.
data MachineTypeVerdict
    = MachineTypeAsDeclared
    | -- | the running one, then the declared one
      MachineTypeDiffers Text Text
    | -- | the describe failed or could not be read: nothing is known, so nothing is changed
      MachineTypeUndetermined Text
    deriving (Eq, Show)

-- | Compares names exactly: GCP has no two spellings of one machine type.
compareMachineType :: MachineType -> ExitCode -> Text -> MachineTypeVerdict
compareMachineType _ (ExitFailure n) _ =
    MachineTypeUndetermined ("could not describe the machine type (exit " <> Text.pack (show n) <> ")")
compareMachineType declared ExitSuccess out =
    case parseLiveMachineType out of
        Left why -> MachineTypeUndetermined why
        Right live
            | live == wanted -> MachineTypeAsDeclared
            | otherwise -> MachineTypeDiffers live wanted
  where
    wanted = machineTypeName (renderMachineType declared)

{- | Why an instance is not as declared, worded to follow @instance NAME@:
the check and @up@ say the same thing. Both types are named in every case.
-}
machineTypeReason :: MachineTypeChange -> Text -> Text -> Text
machineTypeReason change live wanted =
    "runs machine type " <> live <> ", declared " <> wanted <> "; " <> case change of
        RefuseMachineTypeChange ->
            "a machine type only changes on a stopped instance and this declaration does not permit stopping it:"
                <> " stop it and run gcloud compute instances set-machine-type, or declare MayStopToResize"
        MayStopToResize -> "the declaration permits stopping it to resize"

{- | The check's verdict on an existing instance's machine type: 'Success'
when it is the declared one, a 'Failure' naming both otherwise, and a
'Failure' too when it could not be read. Under 'RefuseMachineTypeChange' the
node never repairs that failure; under 'MayStopToResize' its @up@ does.
-}
interpretInstanceMachineType :: MachineTypeChange -> MachineType -> ExitCode -> Text -> CheckResult
interpretInstanceMachineType change declared code out =
    case compareMachineType declared code out of
        MachineTypeAsDeclared -> Success
        MachineTypeDiffers live wanted -> Failure ("instance " <> machineTypeReason change live wanted)
        MachineTypeUndetermined why -> Failure why

{- | How long an instance may run before GCP turns it off by itself (the
termination action is always @STOP@: the instance, its disks and its
addresses stay, and only what a stopped instance bills for is billed).

'NoRunLimit' passes nothing and asks nothing: it is what every instance was
before this type existed.

'MaxRunDuration' is a number of seconds counted by GCP from each start
(@--max-run-duration@; GCP accepts 30 seconds to 120 days, see
'runLimitProblems'). 'RunUntil' is one date for the life of the instance
(@--termination-time@), compared and rendered to the second.

With a limit declared, 'gceInstance' reads it back from an instance that
exists ('judgeRunLimit') and, once the deadline has passed, an instance that
is @TERMINATED@ /is/ the declared state of a 'PoweredOn' declaration: the
check is satisfied and @up@ does not start it. See 'RunLimitVerdict' for
every case, and 'planExistingInstance' for what @up@ then does.

__Starting an expired instance again is a new declaration__: another
duration or a later date, which @up@ sets on the stopped instance before
starting it. For a duration, starting it by hand does the same, since the
deadline is counted from the last start. A forced @up@ of an unchanged
declaration starts nothing.
-}
data RunLimit
    = -- | say nothing: the instance runs until something stops it
      NoRunLimit
    | -- | this many seconds after each start
      MaxRunDuration Integer
    | -- | until this date
      RunUntil UTCTime
    deriving (Eq, Show)

-- | Everything wrong with a run limit declaration; empty for 'NoRunLimit'.
runLimitProblems :: RunLimit -> [Text]
runLimitProblems limit = case limit of
    MaxRunDuration seconds
        | seconds < 30 || seconds > 120 * 86400 ->
            ["a maximum run duration of " <> Text.pack (show seconds) <> "s (GCP accepts 30s to 120 days)"]
    _ -> []

-- | A date the way it is given to gcloud and written in reports: UTC, to the second.
renderDeadline :: UTCTime -> Text
renderDeadline = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | A declared limit in words, for the notes and the reasons.
renderRunLimit :: RunLimit -> Text
renderRunLimit limit = case limit of
    NoRunLimit -> "none"
    MaxRunDuration seconds -> Text.pack (show seconds) <> "s after each start, then stopped by GCP"
    RunUntil date -> "until " <> renderDeadline date <> ", then stopped by GCP"

{- | The flags a declared limit is at @create@, and at @set-scheduling@ too
(with the other kind of limit cleared there, see 'computeCommand'). Nothing
for 'NoRunLimit'.
-}
runLimitArgs :: RunLimit -> [String]
runLimitArgs limit = case limit of
    NoRunLimit -> []
    MaxRunDuration seconds -> ["--max-run-duration", show seconds <> "s", "--instance-termination-action", "STOP"]
    RunUntil date -> ["--termination-time", Text.unpack (renderDeadline date), "--instance-termination-action", "STOP"]

{- | What an instance says of its own run limit, from @describe
--format=json(lastStartTimestamp,scheduling.maxRunDuration.seconds,scheduling.terminationTime,scheduling.instanceTerminationAction)@.
Every field may be absent.

The deadline GCP computes (@terminationTimestamp@) is deliberately not
read: gcloud's help says it is removed whenever the instance is stopped,
which is exactly when the check needs it, and the v1 API this command speaks
does not carry it. A duration's deadline is 'liveLastStart' plus the
duration instead.
-}
data LiveRunLimit = LiveRunLimit
    { liveMaxRunSeconds :: Maybe Integer
    , liveTerminationTime :: Maybe UTCTime
    , liveTerminationAction :: Maybe Text
    , liveLastStart :: Maybe UTCTime
    }
    deriving (Eq, Show)

{- | Reads that describe. An output that is not a JSON object, or a field
that is there and cannot be read (a duration that is not a number, a date
that is not RFC 3339), is a 'Left' saying so: a misread limit must never be
taken for an expired one, nor for one worth setting again.
-}
parseLiveRunLimit :: Text -> Either Text LiveRunLimit
parseLiveRunLimit out =
    case Aeson.decodeStrict (Text.encodeUtf8 out) of
        Just (Aeson.Object top) -> do
            let scheduling = case KeyMap.lookup "scheduling" top of
                    Just (Aeson.Object o) -> o
                    _ -> KeyMap.empty
            seconds <- case KeyMap.lookup "maxRunDuration" scheduling of
                Nothing -> Right Nothing
                Just Aeson.Null -> Right Nothing
                Just (Aeson.Object o) -> optional "scheduling.maxRunDuration.seconds" readSeconds (KeyMap.lookup "seconds" o)
                Just other -> unreadable "scheduling.maxRunDuration" other
            terminationTime <- optional "scheduling.terminationTime" readDate (KeyMap.lookup "terminationTime" scheduling)
            action <- optional "scheduling.instanceTerminationAction" readText (KeyMap.lookup "instanceTerminationAction" scheduling)
            lastStart <- optional "lastStartTimestamp" readDate (KeyMap.lookup "lastStartTimestamp" top)
            Right (LiveRunLimit seconds terminationTime action lastStart)
        _ -> Left ("no run limit could be read in " <> Text.pack (show out))
  where
    optional :: Text -> (Aeson.Value -> Maybe a) -> Maybe Aeson.Value -> Either Text (Maybe a)
    optional _ _ Nothing = Right Nothing
    optional _ _ (Just Aeson.Null) = Right Nothing
    optional field readValue (Just value) = maybe (unreadable field value) (Right . Just) (readValue value)
    unreadable :: Text -> Aeson.Value -> Either Text a
    unreadable field value = Left ("could not read " <> field <> " in " <> Text.pack (show (Aeson.encode value)))
    -- an int64 is a string in the API's JSON; a number is accepted as well
    readSeconds :: Aeson.Value -> Maybe Integer
    readSeconds value = case value of
        Aeson.String t -> readMaybe (Text.unpack t)
        Aeson.Number n | n == fromInteger (round n) -> Just (round n)
        _ -> Nothing
    readText value = case value of
        Aeson.String t -> Just t
        _ -> Nothing
    readDate value = case value of
        Aeson.String t ->
            let s = Text.unpack t
             in (iso8601ParseM s :: Maybe UTCTime) <|> (zonedTimeToUTC <$> iso8601ParseM s)
        _ -> Nothing

{- | An existing instance against its declared 'RunLimit', at a given time.

A deadline is, for 'RunUntil', the declared date; for 'MaxRunDuration', the
instance's last start plus the duration, and only when the instance holds
the declared duration (otherwise no run of it was under this limit).
-}
data RunLimitVerdict
    = -- | nothing is declared, so nothing was asked
      RunUnlimited
    | -- | the instance holds the declared limit and its deadline, this one, is ahead
      RunWithin UTCTime
    | -- | the deadline passed less than 'runLimitGrace' ago: GCP is stopping
      -- the instance, or just did
      RunEnding UTCTime
    | -- | the deadline passed
      RunExpired UTCTime
    | -- | the instance holds another limit (or none), and no declared date
      -- has passed: what it holds, then what is declared
      RunLimitDiffers Text Text
    | -- | the describe failed or could not be read: nothing is known, so
      -- nothing is started, stopped or set
      RunLimitUndetermined Text
    deriving (Eq, Show)

{- | For how long after a deadline a @RUNNING@ instance is still GCP's to
stop rather than a failure. GCP's documentation says the stop may come up to
30 seconds late, and a duration's deadline is computed here from the start
/request/, a little earlier than the moment GCP counts from.
-}
runLimitGrace :: NominalDiffTime
runLimitGrace = 300

{- | The verdict, from the declared limit, the current time and the describe
'parseLiveRunLimit' reads. Split out of 'gceInstance' for testability: no
clock is read here.

A declared date that has passed is 'RunEnding'/'RunExpired' whatever limit
the instance holds: nothing may start it any more, and GCP itself refuses to.
-}
judgeRunLimit :: RunLimit -> UTCTime -> ExitCode -> Text -> RunLimitVerdict
judgeRunLimit NoRunLimit _ _ _ = RunUnlimited
judgeRunLimit _ _ (ExitFailure n) _ =
    RunLimitUndetermined ("could not describe the run limit (exit " <> Text.pack (show n) <> ")")
judgeRunLimit limit now ExitSuccess out =
    case parseLiveRunLimit out of
        Left why -> RunLimitUndetermined why
        Right live -> case limit of
            NoRunLimit -> RunUnlimited
            RunUntil date
                | now >= date -> passed date
                | holds live && (seconds <$> live.liveTerminationTime) == Just (seconds date) && live.liveMaxRunSeconds == Nothing -> RunWithin date
                | otherwise -> differs live
            MaxRunDuration duration
                | holds live && live.liveMaxRunSeconds == Just duration && live.liveTerminationTime == Nothing ->
                    case live.liveLastStart of
                        Nothing -> RunLimitUndetermined "the instance holds the declared run limit and says nothing of its last start"
                        Just started
                            | now < deadline -> RunWithin deadline
                            | otherwise -> passed deadline
                          where
                            deadline = addUTCTime (fromInteger duration) started
                | otherwise -> differs live
  where
    seconds :: UTCTime -> Integer
    seconds = floor . utcTimeToPOSIXSeconds
    holds :: LiveRunLimit -> Bool
    holds live = live.liveTerminationAction == Just "STOP"
    passed deadline
        | now < addUTCTime runLimitGrace deadline = RunEnding deadline
        | otherwise = RunExpired deadline
    differs :: LiveRunLimit -> RunLimitVerdict
    differs live = RunLimitDiffers (renderLive live) (renderRunLimit limit)
    renderLive :: LiveRunLimit -> Text
    renderLive live =
        case (live.liveMaxRunSeconds, live.liveTerminationTime) of
            (Nothing, Nothing) -> "none"
            (duration, date) ->
                Text.intercalate
                    " and "
                    ( [Text.pack (show d) <> "s after each start" | Just d <- [duration]]
                        <> ["until " <> renderDeadline t | Just t <- [date]]
                    )
                    <> ", then "
                    <> maybe "no declared action" id live.liveTerminationAction

{- | Why an instance's run limit is not the declared one, worded to follow
@instance NAME@: the check and @up@ say the same thing.
-}
runLimitReason :: Text -> Text -> Text
runLimitReason live wanted =
    "holds run limit "
        <> live
        <> ", declared "
        <> wanted
        <> "; a run limit is only set on a stopped instance and this node does not stop one for it:"
        <> " stop it (the next up sets the limit and starts it), or run gcloud compute instances set-scheduling"

-- | Boot disk configuration.
{- | A boot disk, named either by a specific image or by an image family (in
whichever project publishes it). A family is the usual choice: it tracks the
publisher's current image, where a name pins one that is eventually deleted.
-}
data BootDisk = BootDisk
    { bootDiskSizeGb :: Int
    , bootDiskImage :: Maybe Text
    , bootDiskImageFamily :: Maybe Text
    , bootDiskImageProject :: Maybe Text
    }
    deriving (Eq, Show)

{- | Whether the declared instance is meant to be running or stopped
(@TERMINATED@). A stopped instance keeps its disks and its reserved address
and bills for those only, so 'PoweredOff' is how a recipe pauses a machine
without giving up what is on it; @up@ moves between the two, and @down@
deletes either.
-}
data InstancePower = PoweredOn | PoweredOff
    deriving (Eq, Show)

{- | The external (public) address of an instance's one network interface.

'NoExternalAddress' is a machine nothing outside the VPC can reach and that
cannot reach out either, unless the network has a Cloud NAT: no package
mirror, no container registry. It is the right choice for a machine only its
peers talk to (a database member), and it is fixed at create time like the
rest of the interface.
-}
data ExternalAddress
    = -- | one GCP picks at create time and takes back with the instance
      EphemeralExternal
    | -- | a reserved external 'Address', by name
      ReservedExternal Text
    | -- | none at all (@--no-address@)
      NoExternalAddress
    deriving (Eq, Show)

{- | The internal address of an instance's one network interface.

'PinnedInternal' is what lets a graph /name/ a machine before it exists: the
address is the caller's choice (it must lie in 'instanceSubnet''s range and
be free), so unlike an external one it is known when the graph is declared
and can go into a peer's @pg_hba.conf@ or a config file in the same pass.
GCP keeps such an address for the life of the instance, across stops and
starts, but hands it to anyone once the instance is deleted; reserving it as
well (an 'Address' of kind 'InternalAddress' carrying the same literal, which
the instance should then depend on) is what keeps it across a re-creation.
-}
data InternalAddress
    = -- | whatever GCP picks in the subnet
      EphemeralInternal
    | -- | this literal (@--private-network-ip@)
      PinnedInternal Text
    deriving (Eq, Show)

{- | The access scopes of an instance: the ceiling on what its service
account's tokens, as handed out by the metadata server, may be used for. IAM
decides what the account /may/ do; the scopes decide how much of that a
process on the machine gets, so an account allowed to write to a bucket
still cannot from an instance holding gcloud's default scopes (storage
read-only).

'DefaultScopes' passes nothing and is what every instance was before this
type existed. 'DeclaredScopes' lists them, each either one of gcloud's
aliases ('scopeAliases': @storage-rw@, @cloud-platform@, ...) or a full
@https://www.googleapis.com/auth/...@ URI; the empty list is no scope at all
(@--no-scopes@).

__Scopes are read at create time and never changed by this node.__ GCP only
lets them change on a stopped instance, so on an existing instance holding
other scopes than the declared ones the check is a 'Failure' naming both
sets and @up@ throws the same words without starting, stopping or
re-creating anything: stopping a machine is an operator's decision. The
remedy is theirs too -- stop it, @gcloud compute instances
set-service-account NAME --scopes ...@, start it; or take the node down and
up again, which deletes the instance.
-}
data AccessScopes
    = -- | say nothing: gcloud's defaults for the service account
      DefaultScopes
    | -- | exactly these, aliases or URIs (@[]@ is @--no-scopes@)
      DeclaredScopes [Text]
    deriving (Eq, Show)

{- | gcloud's scope aliases and the URIs they stand for, as documented for
@gcloud compute instances create --scopes@. The instance is described back
with URIs only, so the check needs this table to compare; the create command
is given the declared spelling, so if gcloud and this table ever disagree
about an alias the check says so (a 'Failure' naming both sets) rather than
an instance quietly holding something else.
-}
scopeAliases :: Map Text [Text]
scopeAliases =
    Map.fromList
        [ ("bigquery", [auth "bigquery"])
        , ("cloud-platform", [auth "cloud-platform"])
        , ("cloud-source-repos", [auth "source.full_control"])
        , ("cloud-source-repos-ro", [auth "source.read_only"])
        , ("compute-ro", [auth "compute.readonly"])
        , ("compute-rw", [auth "compute"])
        , ("datastore", [auth "datastore"])
        ,
            ( "default"
            , map
                auth
                [ "devstorage.read_only"
                , "logging.write"
                , "monitoring.write"
                , "pubsub"
                , "service.management.readonly"
                , "servicecontrol"
                , "trace.append"
                ]
            )
        , ("logging-write", [auth "logging.write"])
        , ("monitoring", [auth "monitoring"])
        , ("monitoring-read", [auth "monitoring.read"])
        , ("monitoring-write", [auth "monitoring.write"])
        , ("pubsub", [auth "pubsub"])
        , ("service-control", [auth "servicecontrol"])
        , ("service-management", [auth "service.management.readonly"])
        , ("sql-admin", [auth "sqlservice.admin"])
        , ("storage-full", [auth "devstorage.full_control"])
        , ("storage-ro", [auth "devstorage.read_only"])
        , ("storage-rw", [auth "devstorage.read_write"])
        , ("taskqueue", [auth "taskqueue"])
        , ("trace", [auth "trace.append"])
        , ("userinfo-email", [auth "userinfo.email"])
        ]
  where
    auth t = "https://www.googleapis.com/auth/" <> t

{- | The URIs one declared scope stands for, or why it cannot be declared: a
full @https://@ URI is itself, an alias is looked up, anything else is
refused here rather than by gcloud half-way through a pass.
-}
scopeUris :: Text -> Either Text [Text]
scopeUris scope
    | Text.null scope = Left "an empty scope"
    | Text.any (`elem` separators) scope = Left ("scope " <> Text.pack (show scope) <> " holds a separator")
    | "https://" `Text.isPrefixOf` scope = Right [scope]
    | otherwise =
        maybe
            (Left ("unknown scope alias " <> scope <> " (spell it as a full https://www.googleapis.com/auth/... URI)"))
            Right
            (Map.lookup scope scopeAliases)
  where
    separators = ", ;\t\n" :: String

-- | Everything wrong with a scope declaration; empty for 'DefaultScopes'.
scopeProblems :: AccessScopes -> [Text]
scopeProblems DefaultScopes = []
scopeProblems (DeclaredScopes scopes) = [problem | Left problem <- map scopeUris scopes]

{- | The set of URIs a declaration asks for; 'Nothing' when nothing is
declared (nothing to compare) or the declaration has 'scopeProblems'.
-}
declaredScopeUris :: AccessScopes -> Maybe (Set Text)
declaredScopeUris DefaultScopes = Nothing
declaredScopeUris (DeclaredScopes scopes) = Set.fromList . concat <$> traverse (either (const Nothing) Just . scopeUris) scopes

{- | The scope URIs in the output of @describe
--format=value(serviceAccounts[].scopes)@. Only the @https://@ words are
kept, whatever separates them, because how gcloud flattens a list of lists
has not been observed; an instance with no service account yields none.
-}
parseLiveScopes :: Text -> Set Text
parseLiveScopes =
    Set.fromList
        . filter ("https://" `Text.isPrefixOf`)
        . Text.split (`elem` (",; \t\r\n'\"[]" :: String))

{- | The verdict on an existing instance's scopes against the declared ones:
'Success' when nothing is declared or the two sets are equal, a 'Failure'
naming both otherwise. Never a reason to act: see 'AccessScopes'.
-}
interpretInstanceScopes :: AccessScopes -> ExitCode -> Text -> CheckResult
interpretInstanceScopes DefaultScopes _ _ = Success
interpretInstanceScopes scopes@(DeclaredScopes _) code out =
    case (declaredScopeUris scopes, code) of
        (Nothing, _) -> Failure ("scopes cannot be declared: " <> Text.intercalate "; " (scopeProblems scopes))
        (_, ExitFailure n) -> Failure ("could not describe instance scopes (exit " <> Text.pack (show n) <> ")")
        (Just wanted, ExitSuccess)
            | wanted == live -> Success
            | otherwise ->
                Failure
                    ( "instance holds scopes "
                        <> renderSet live
                        <> ", declared "
                        <> renderSet wanted
                        <> "; scopes only change on a stopped instance and this node does not stop one:"
                        <> " stop it and run gcloud compute instances set-service-account, or re-create it"
                    )
          where
            live = parseLiveScopes out
  where
    renderSet s
        | Set.null s = "(none)"
        | otherwise = Text.intercalate "," (Set.toList s)

-- | A GCE instance.
data Instance = Instance
    { instanceName :: Text
    , instancePower :: InstancePower
    -- ^ the state @up@ converges to; 'PoweredOn' is the usual one
    , instanceProject :: Project
    , instanceZone :: Zone
    , instanceMachineType :: MachineType
    -- ^ compared, by name, with what an existing instance runs: a difference
    -- is never left silently, see 'instanceMachineTypeChange'
    , instanceBootDisk :: BootDisk
    , instanceNetwork :: Text
    , instanceSubnet :: Text
    , instanceServiceAccount :: Maybe Text
    , instanceMetadata :: Map Text Text
    , instanceMetadataFiles :: Map Text FilePath
    -- ^ metadata whose value is read from a local file
    -- (@--metadata-from-file@) -- how a multi-line @startup-script@ is
    -- passed without quoting it into a single argv value.
    , instanceExternalAddress :: ExternalAddress
    -- ^ reserved by name (see 'address'), ephemeral, or none
    , instanceInternalAddress :: InternalAddress
    -- ^ pinned to a declared literal, or left to GCP. Like the rest of the
    -- interface it is only read at create time: 'gceInstance''s check looks
    -- at the instance's status, so an existing instance holding another
    -- address is not noticed and not changed.
    , instanceTags :: [Text]
    , instanceScopes :: AccessScopes
    -- ^ the access scopes, or gcloud's defaults. Create-time only, and
    -- unlike the addresses a difference on an existing instance /is/
    -- noticed, as a 'Failure' the node never repairs: see 'AccessScopes'.
    , instanceMachineTypeChange :: MachineTypeChange
    -- ^ whether @up@ may stop an existing instance to give it the declared
    -- machine type, or fails naming both types ('RefuseMachineTypeChange',
    -- which renders nothing)
    , instanceRunLimit :: RunLimit
    -- ^ how long the instance may run before GCP stops it; once that
    -- deadline has passed a stopped instance is the declared state and is
    -- not started ('NoRunLimit' renders nothing and asks nothing)
    }
    deriving (Eq, Show)

-- | Idempotently manages a GCE instance.
--
-- * 'up': create the instance if absent, then bring it to 'instancePower':
--   start it if @TERMINATED@ or resume it if @SUSPENDED@ for 'PoweredOn',
--   stop it if @RUNNING@ for 'PoweredOff'. See 'planInstanceUp'.
-- * 'down': delete the instance, whatever state it is in.
-- * 'check': report 'Success' if the instance is in the declared state,
--   runs the declared machine type and, when 'instanceScopes' declares
--   some, holds exactly those.
--
-- An existing instance running another machine type than the declared one is
-- a 'Failure' naming both. What @up@ does then is 'instanceMachineTypeChange':
-- it throws the same words without touching the machine, or, permitted to,
-- stops it, sets the type and starts it again ('planExistingInstance').
--
-- Declared scopes that differ from an existing instance's are a 'Failure'
-- for the check and a throw for @up@, before it starts or stops anything;
-- undeclarable ones ('scopeProblems') are the same before any call.
--
-- With an 'instanceRunLimit', GCP stops the instance at its deadline and the
-- node accepts that: after the deadline a @TERMINATED@ instance satisfies a
-- 'PoweredOn' declaration and @up@ starts nothing ('interpretInstanceRun',
-- 'planExistingInstance'). A limit that differs on an existing instance is
-- set when the instance is stopped and is a 'Failure' naming both otherwise.
gceInstance :: Reporter Report -> Track' (Binary "gcloud") -> Instance -> Op
gceInstance r gcloudTrack inst =
    withBinary gcloudTrack computeCommand (InstancesCreate inst) $ \create ->
        withBinary gcloudTrack computeCommand (InstancesStart inst) $ \start ->
            withBinary gcloudTrack computeCommand (InstancesResume inst) $ \resume ->
                withBinary gcloudTrack computeCommand (InstancesStop inst) $ \stop ->
                    withBinary gcloudTrack computeCommand (InstancesSetMachineType inst) $ \setType ->
                        withBinary gcloudTrack computeCommand (InstancesSetScheduling inst) $ \setScheduling ->
                            withBinary gcloudTrack computeCommand (InstancesDelete inst) $ \delete ->
                                op "gcp-instance" nodeps $ \actions ->
                                    actions
                                        { help = Text.unwords [verb, "GCE instance", inst.instanceName]
                                        , notes = actions.notes <> scopeNotes <> resizeNotes <> runLimitNotes
                                        , ref = mkRef "gcp-instance" (inst.instanceProject.projectId, inst.instanceZone.zoneName, inst.instanceName)
                                        , up = bringUp create start resume stop setType setScheduling
                                        , -- presence, not state: a stopped instance is still there to delete
                                          down = Core.downIfPresent (uncurry interpretInstancePresence <$> describeStatus) (delete (contramap (RunComputeCommand (InstancesDelete inst)) r))
                                        , check = checkInstance
                                        }
  where
    rFor cmd = contramap (RunComputeCommand cmd) r

    -- in the notes so that a re-declaration with other scopes is a changed
    -- node under @run serve@; nothing at all when none is declared
    scopeNotes = case inst.instanceScopes of
        DefaultScopes -> []
        DeclaredScopes [] -> ["scopes: none"]
        DeclaredScopes scopes -> ["scopes: " <> Text.intercalate "," scopes]

    -- only with the permission, so that a declaration without it has the
    -- notes it had; with it, a re-declared type is a changed node under
    -- @run serve@
    resizeNotes = case inst.instanceMachineTypeChange of
        RefuseMachineTypeChange -> []
        MayStopToResize -> ["machine type: " <> machineTypeName (renderMachineType inst.instanceMachineType) <> " (may stop to resize)"]

    -- in the notes so that a re-declared limit is a changed node under
    -- @run serve@; nothing at all when none is declared
    runLimitNotes = case inst.instanceRunLimit of
        NoRunLimit -> []
        limit -> ["run limit: " <> renderRunLimit limit]

    declarationProblems :: [Text]
    declarationProblems =
        ["scopes cannot be declared: " <> Text.intercalate "; " problems | problems@(_ : _) <- [scopeProblems inst.instanceScopes]]
            <> ["run limit cannot be declared: " <> Text.intercalate "; " problems | problems@(_ : _) <- [runLimitProblems inst.instanceRunLimit]]

    -- the clock is read here and nowhere else; with no limit declared
    -- neither it nor the instance is asked anything
    judgeRun :: IO RunLimitVerdict
    judgeRun = case inst.instanceRunLimit of
        NoRunLimit -> pure RunUnlimited
        limit -> do
            (code, out, _err) <-
                readCreateProcessWithExitCode
                    (prepare computeCommand (InstancesDescribeRunLimit inst))
                    ""
            now <- getCurrentTime
            pure (judgeRunLimit limit now code (Text.decodeUtf8 out))

    -- the status first, as before; the scopes, the machine type and the run
    -- limit are only asked of an instance that is there: the scopes and the
    -- limit when declared, the machine type always (one more describe each)
    checkInstance = case declarationProblems of
        problems@(_ : _) -> pure (Failure (Text.intercalate "; " problems))
        [] -> do
            (code, status) <- describeStatus
            case code of
                ExitFailure _ -> pure (interpretInstanceStatus inst.instancePower code status)
                ExitSuccess -> do
                    scopes <- case inst.instanceScopes of
                        DeclaredScopes _ -> describeScopes
                        DefaultScopes -> pure Success
                    case scopes of
                        Success -> do
                            (typeCode, typeOut) <- describeMachineType
                            case interpretInstanceMachineType inst.instanceMachineTypeChange inst.instanceMachineType typeCode typeOut of
                                Success -> do
                                    run <- judgeRun
                                    pure (interpretInstanceRun inst.instancePower run code status)
                                differing -> pure differing
                        differing -> pure differing

    describeMachineType :: IO (ExitCode, Text)
    describeMachineType = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                (prepare computeCommand (InstancesDescribeMachineType inst))
                ""
        pure (code, Text.decodeUtf8 out)

    describeScopes :: IO CheckResult
    describeScopes = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                (prepare computeCommand (InstancesDescribeScopes inst))
                ""
        pure (interpretInstanceScopes inst.instanceScopes code (Text.decodeUtf8 out))

    refuse reason = throwIO (userError ("instance " <> Text.unpack inst.instanceName <> ": " <> Text.unpack reason))

    verb = case inst.instancePower of
        PoweredOn -> "creates"
        PoweredOff -> "creates, stopped,"

    describeStatus :: IO (ExitCode, Text)
    describeStatus = do
        (code, out, _err) <-
            readCreateProcessWithExitCode
                (prepare computeCommand (InstancesDescribeStatus inst))
                ""
        pure (code, Text.strip (Text.decodeUtf8 out))

    -- 'create' alone is what 'up' used to be, which made a stopped instance
    -- unrecoverable: the check says 'Failure', 'up' runs @create@, and
    -- @create@ refuses because the instance exists. Asking first costs one
    -- describe that the check has usually just done.
    bringUp create start resume stop setType setScheduling = do
        case declarationProblems of
            [] -> pure ()
            problems -> refuse (Text.intercalate "; " problems)
        (code, status) <- describeStatus
        let plan = planInstanceUp inst.instancePower code status
        -- an instance that exists with other scopes is left exactly as it
        -- is: no start, no stop, and certainly no re-creation
        case (plan, inst.instanceScopes) of
            (CreateInstance, _) -> pure ()
            (_, DefaultScopes) -> pure ()
            (_, DeclaredScopes _) -> do
                scopes <- describeScopes
                case scopes of
                    Failure reason -> refuse reason
                    _ -> pure ()
        -- the machine type is only asked of an instance that is there, and
        -- the whole sequence is decided before its first step: a refusal
        -- leaves the machine exactly as it is
        steps <- case plan of
            CreateInstance -> planAbsentInstance inst.instanceRunLimit inst.instancePower <$> getCurrentTime
            _ -> do
                typeVerdict <- uncurry (compareMachineType inst.instanceMachineType) <$> describeMachineType
                planExistingInstance inst.instanceMachineTypeChange inst.instancePower status typeVerdict <$> judgeRun
        case steps of
            Left reason -> throwIO (userError ("instance " <> Text.unpack inst.instanceName <> " " <> Text.unpack reason))
            Right todo -> mapM_ runStep todo
      where
        runStep step = case step of
            CreateStep -> create (rFor (InstancesCreate inst))
            StartStep -> start (rFor (InstancesStart inst))
            ResumeStep -> resume (rFor (InstancesResume inst))
            StopStep -> stop (rFor (InstancesStop inst))
            SetMachineTypeStep -> setType (rFor (InstancesSetMachineType inst))
            SetSchedulingStep -> setScheduling (rFor (InstancesSetScheduling inst))

-- | The verdict drawn from @gcloud compute instances describe
-- --format=value(status)@ against the declared power state, split out for
-- testability.
interpretInstanceStatus :: InstancePower -> ExitCode -> Text -> CheckResult
interpretInstanceStatus _ (ExitFailure n) _ =
    Failure ("could not describe instance (exit " <> Text.pack (show n) <> ")")
interpretInstanceStatus power ExitSuccess status =
    case status of
        "RUNNING" -> case power of
            PoweredOn -> Success
            PoweredOff -> Failure "instance is RUNNING, stopped wanted"
        "TERMINATED" -> case power of
            PoweredOn -> Failure "instance is TERMINATED"
            PoweredOff -> Success
        "PROVISIONING" -> Unknown
        "STAGING" -> Unknown
        "STOPPING" -> Unknown
        "SUSPENDING" -> Unknown
        "REPAIRING" -> Unknown
        "SUSPENDED" -> Failure "instance is SUSPENDED"
        _ -> Failure ("unexpected instance status: " <> status)

{- | The check's verdict on an existing instance's status given the verdict
on its run limit: 'interpretInstanceStatus' with the deadline taken into
account. Split out of 'gceInstance' for testability.

* Nothing declared, or the limit in place with its deadline ahead: exactly
  'interpretInstanceStatus'.
* The deadline has passed and the declaration is 'PoweredOn': @TERMINATED@
  is 'Success' (the instance ran its allotted time); @RUNNING@ is 'Unknown'
  for 'runLimitGrace' (GCP is stopping it) and then a 'Failure' naming the
  deadline; any other status is what 'interpretInstanceStatus' says.
* A 'PoweredOff' declaration is not changed by a deadline.
* The instance holds another limit: a 'Failure' naming both (a transitional
  status stays 'Unknown').
* The limit could not be read: a 'Failure' saying so, never a pass.
-}
interpretInstanceRun :: InstancePower -> RunLimitVerdict -> ExitCode -> Text -> CheckResult
interpretInstanceRun power run code status = case (code, run) of
    (ExitFailure _, _) -> asBefore
    (_, RunUnlimited) -> asBefore
    (_, RunWithin _) -> asBefore
    (_, RunLimitUndetermined why) -> Failure why
    (_, RunLimitDiffers live wanted) -> case asBefore of
        Unknown -> Unknown
        _ -> Failure ("instance " <> runLimitReason live wanted)
    (_, RunEnding deadline) -> after deadline True
    (_, RunExpired deadline) -> after deadline False
  where
    asBefore = interpretInstanceStatus power code status
    after deadline ending = case (power, status) of
        (PoweredOn, "TERMINATED") -> Success
        (PoweredOn, "RUNNING")
            | ending -> Unknown
            | otherwise -> Failure ("instance is RUNNING past its run limit (deadline " <> renderDeadline deadline <> ")")
        _ -> asBefore

-- | Whether the instance exists at all, whatever it is doing: what @down@ asks.
interpretInstancePresence :: ExitCode -> Text -> CheckResult
interpretInstancePresence (ExitFailure n) _ = Failure ("could not describe instance (exit " <> Text.pack (show n) <> ")")
interpretInstancePresence ExitSuccess _ = Success

-- | What 'gceInstance'\'s 'up' does given the declared power state and the instance's current status.
data InstanceUpPlan
    = CreateInstance
    | StartInstance
    | ResumeInstance
    | StopInstance
    | AlreadyThere
    | -- | a transitional (or unrecognized) status: nothing safe to run now
      CannotActYet Text
    deriving (Eq, Show)

{- | Split out of 'gceInstance' for testability. A failing describe is read
as "absent": if it failed for another reason (credentials, a missing API)
the @create@ that follows fails too, and says why more clearly than a
describe would. A @SUSPENDED@ instance cannot be stopped directly (gcloud
wants it resumed first), so for 'PoweredOff' it is left alone with a word.
-}
planInstanceUp :: InstancePower -> ExitCode -> Text -> InstanceUpPlan
planInstanceUp _ (ExitFailure _) _ = CreateInstance
planInstanceUp PoweredOn ExitSuccess status =
    case status of
        "RUNNING" -> AlreadyThere
        "TERMINATED" -> StartInstance
        "SUSPENDED" -> ResumeInstance
        _ -> CannotActYet status
planInstanceUp PoweredOff ExitSuccess status =
    case status of
        "RUNNING" -> StopInstance
        "TERMINATED" -> AlreadyThere
        "SUSPENDED" -> CannotActYet "SUSPENDED (resume it before declaring it stopped)"
        _ -> CannotActYet status

-- | One gcloud call of 'gceInstance'\'s @up@.
data InstanceStep
    = CreateStep
    | StartStep
    | ResumeStep
    | StopStep
    | SetMachineTypeStep
    | -- | @set-scheduling@ with the declared 'RunLimit'; only on a stopped instance
      SetSchedulingStep
    deriving (Eq, Show)

{- | The calls a plan stands for, or why none is made (worded to follow
@instance NAME@). A created instance runs, so a stopped declaration stops it
right after.
-}
instanceUpSteps :: InstancePower -> InstanceUpPlan -> Either Text [InstanceStep]
instanceUpSteps power plan = case plan of
    CreateInstance -> Right (CreateStep : [StopStep | power == PoweredOff])
    StartInstance -> Right [StartStep]
    ResumeInstance -> Right [ResumeStep]
    StopInstance -> Right [StopStep]
    AlreadyThere -> Right []
    CannotActYet status -> Left ("is " <> status <> "; retry once it settles")

{- | What @up@ does when there is no instance: create it, unless its
declared date has already passed (GCP refuses a termination time in the
past, and a machine that may no longer run is not one to create).
-}
planAbsentInstance :: RunLimit -> InstancePower -> UTCTime -> Either Text [InstanceStep]
planAbsentInstance limit power now = case limit of
    RunUntil date
        | now >= date -> Left ("is absent and its declared deadline " <> renderDeadline date <> " has passed: it is not created")
    _ -> instanceUpSteps power CreateInstance

{- | What @up@ does to an instance that exists, in the given status, given
the verdicts on its machine type and on its run limit. Split out of
'gceInstance' for testability; this is the one place a resize, a start past
a deadline and a change of limit are decided.

The machine type first, as if there were no limit:

* The type is as declared: exactly what 'planInstanceUp' says, whatever the
  permission.
* It could not be read: nothing, and a reason. Not knowing is never a reason
  to stop a machine, nor to report it converged.
* It differs and the declaration is 'RefuseMachineTypeChange': nothing, and
  a reason naming both types, even when the instance happens to be stopped
  (the declaration did not ask for its type to be changed).
* It differs and the declaration is 'MayStopToResize': stop it if @RUNNING@,
  set the type, start it if the declaration is 'PoweredOn'. Any other status
  (transitional, or @SUSPENDED@, which GCP does not resize) is a reason and
  no call.

If a step fails the ones after it are not run: a failed @set-machine-type@
leaves the instance stopped, and the next pass, finding it @TERMINATED@ with
the old type, sets the type and starts it.

Then the run limit decides what of that is kept:

* None declared ('RunUnlimited'), or in place with its deadline ahead
  ('RunWithin'): all of it, unchanged.
* It could not be read: nothing, and a reason.
* The deadline has passed and the declaration is 'PoweredOn': __nothing
  starts, resumes or stops the instance__. A @TERMINATED@ one gets the plan
  without its start, so a permitted resize still sets the type and leaves
  the machine off; any other status is a reason and no call (GCP stops a
  @RUNNING@ one itself; this node never stops a machine declared running).
  A 'PoweredOff' declaration never starts anything and is not changed.
* The instance holds another limit: @set-scheduling@ is added where the plan
  has the machine stopped -- first on a @TERMINATED@ instance, right after
  the stop when the plan (a 'PoweredOff' declaration, a permitted resize)
  already stops a @RUNNING@ one. A machine is never stopped for its limit:
  otherwise a reason naming both limits and no call.

A failed start after a successful @set-scheduling@ leaves a stopped instance
holding the declared limit: for a duration, if its previous start is more
than that duration ago, the next pass reads it as expired and leaves it off
(start it by hand, or declare another duration).
-}
planExistingInstance :: MachineTypeChange -> InstancePower -> Text -> MachineTypeVerdict -> RunLimitVerdict -> Either Text [InstanceStep]
planExistingInstance change power status verdict run = case run of
    RunUnlimited -> typed
    RunWithin _ -> typed
    RunLimitUndetermined why -> Left ("is left as it is: " <> why)
    RunLimitDiffers live wanted -> case (status, typed) of
        (_, Left why) -> Left why
        ("TERMINATED", Right steps) -> Right (SetSchedulingStep : steps)
        ("RUNNING", Right (StopStep : steps)) -> Right (StopStep : SetSchedulingStep : steps)
        _ -> Left (runLimitReason live wanted)
    RunEnding deadline -> after deadline True
    RunExpired deadline -> after deadline False
  where
    after deadline ending = case (power, status) of
        (PoweredOff, _) -> typed
        (PoweredOn, "TERMINATED") -> filter (/= StartStep) <$> typed
        (PoweredOn, "RUNNING")
            | ending -> Left ("is RUNNING at its deadline " <> renderDeadline deadline <> ", when GCP stops it: retry once it settles")
            | otherwise ->
                Left
                    ( "is RUNNING past its run limit (deadline "
                        <> renderDeadline deadline
                        <> "); this node does not stop a machine declared running: stop it, or declare a later limit"
                    )
        (PoweredOn, _) -> Left ("is " <> status <> " past its deadline " <> renderDeadline deadline <> ": it is not started")
    typed = case verdict of
        MachineTypeAsDeclared -> instanceUpSteps power (planInstanceUp power ExitSuccess status)
        MachineTypeUndetermined why -> Left ("is left as it is: " <> why)
        MachineTypeDiffers live wanted -> case change of
            RefuseMachineTypeChange -> Left (machineTypeReason change live wanted)
            MayStopToResize -> case status of
                "RUNNING" -> Right ([StopStep, SetMachineTypeStep] <> restart)
                "TERMINATED" -> Right (SetMachineTypeStep : restart)
                _ ->
                    Left
                        ( "is "
                            <> status
                            <> " and runs machine type "
                            <> live
                            <> ", declared "
                            <> wanted
                            <> "; the type is only set on an instance that is RUNNING (stopped first) or TERMINATED: retry once it settles"
                        )
    restart = [StartStep | power == PoweredOn]

-------------------------------------------------------------------------------

{- | A reserved regional IP, external or internal.

Reserved rather than ephemeral because an ephemeral address is handed out at
instance-create time and taken back when the instance goes away, so nothing
that has to /name/ the machine (an SSH client, a DNS record, a config file)
can be written before it exists. A reserved one is a resource in its own
right: it can be created, read, attached and released on its own schedule.

An external one still cannot be known when the graph is /declared/ -- GCP
picks the address -- which is why 'readAddress' exists as a separate,
out-of-graph read: for a driver to use between two passes, or for a
"Salmon.Builtin.Nodes.Deferred" node to use within one. An internal one can:
see 'InternalAddress'.
-}
data Address = Address
    { addressName :: Text
    , addressProject :: Project
    , addressRegion :: Region
    , addressKind :: AddressKind
    }
    deriving (Eq, Show)

-- | Which side of the VPC's edge a reserved 'Address' is on.
data AddressKind
    = ExternalAddress
    | {- | In this subnet (by name, in the address's region), at this literal
      if one is given and wherever GCP picks in the subnet's range otherwise.
      Give the literal when an instance is to be pinned to it
      ('PinnedInternal' takes the same one): the check then also compares
      what was reserved with what was declared.
      -}
      InternalAddress Text (Maybe Text)
    deriving (Eq, Show)

-- | Idempotently reserves a regional IP.
address :: Reporter Report -> Track' (Binary "gcloud") -> Address -> Op
address r gcloudTrack addr =
    withBinary gcloudTrack computeCommand (AddressesCreate addr) $ \create ->
        withBinary gcloudTrack computeCommand (AddressesDelete addr) $ \delete ->
            op "gcp-address" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["reserves", kindWord, "IP", addr.addressName]
                    , ref = mkRef "gcp-address" (addr.addressProject.projectId, addr.addressRegion.regionName, addr.addressName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create (rFor' (AddressesCreate addr)))
                    , down = Core.downIfPresent checkAddress (delete (rFor' (AddressesDelete addr)))
                    , check = checkAddress
                    }
  where
    rFor' cmd = contramap (RunComputeCommand cmd) r

    kindWord = case addr.addressKind of
        ExternalAddress -> "external"
        InternalAddress _ Nothing -> "internal"
        InternalAddress _ (Just ip) -> "internal (" <> ip <> ")"

    checkAddress :: IO CheckResult
    checkAddress = do
        (code, out, _err) <-
            readCreateProcessWithExitCode (prepare computeCommand (AddressesDescribe addr)) ""
        pure $ interpretAddress addr code (Text.strip (Text.decodeUtf8 out))

-- | The verdict drawn from @gcloud compute addresses describe
-- --format=value(address)@, split out for testability.
interpretAddressDescribe :: Text -> ExitCode -> Text -> CheckResult
interpretAddressDescribe name (ExitFailure _) _ = Failure ("address not reserved: " <> name)
interpretAddressDescribe name ExitSuccess out
    | Text.null out = Failure ("address reserved but has no IP: " <> name)
    | otherwise = Success

{- | 'interpretAddressDescribe', and for an internal address declared with a
literal, that the reservation holds /that/ literal. A reservation of the
same name on another address is a 'Failure' @up@ cannot repair (@create@
refuses an existing name), which is the point: the instance pinned to the
declared literal would otherwise be created on an address nothing reserved.
-}
interpretAddress :: Address -> ExitCode -> Text -> CheckResult
interpretAddress addr code out =
    case (interpretAddressDescribe addr.addressName code out, addr.addressKind) of
        (Success, InternalAddress _ (Just wanted))
            | out /= wanted ->
                Failure ("address " <> addr.addressName <> " is reserved as " <> out <> ", declared " <> wanted)
        (verdict, _) -> verdict

{- | Reads a reserved address's actual IP, outside any graph.

Deliberately not an 'Op': what GCP picked is knowable only after the address
node's @up@, while an 'Op' that needs the IP (an ssh endpoint, say) is built
before any @up@ runs. Either a driver converges once, calls this, and
declares the rest, or the part that needs the IP is built at @up@ from this
read ("Salmon.Builtin.Nodes.Deferred"; @SreBox.Gcp.VmProvision@'s
@provisionedVmReadingHost@ is that for an ssh host) -- @salmon-apps@'s
@GcpToy@ tier 2 does the second and its driver script the first. 'Nothing'
when the address does not exist yet.
-}
readAddress :: Address -> IO (Maybe Text)
readAddress addr = do
    (code, out, _err) <-
        readCreateProcessWithExitCode (prepare computeCommand (AddressesDescribe addr)) ""
    let ip = Text.strip (Text.decodeUtf8 out)
    pure $ case code of
        ExitSuccess | not (Text.null ip) -> Just ip
        _ -> Nothing

-------------------------------------------------------------------------------

-- | An ingress firewall rule on a network, scoped to instances carrying a tag.
data FirewallRule = FirewallRule
    { firewallName :: Text
    , firewallProject :: Project
    , firewallNetwork :: Text
    , firewallAllow :: Text
    -- ^ gcloud's own @--allow@ syntax, e.g. @tcp:22@
    , firewallSourceRanges :: [Text]
    , firewallTargetTags :: [Text]
    }
    deriving (Eq, Show)

-- | Idempotently creates an ingress firewall rule.
firewallRule :: Reporter Report -> Track' (Binary "gcloud") -> FirewallRule -> Op
firewallRule r gcloudTrack fw =
    withBinary gcloudTrack computeCommand (FirewallCreate fw) $ \create ->
        withBinary gcloudTrack computeCommand (FirewallDelete fw) $ \delete ->
            op "gcp-firewall-rule" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["allows", fw.firewallAllow, "to", Text.intercalate "," fw.firewallTargetTags]
                    , ref = mkRef "gcp-firewall-rule" (fw.firewallProject.projectId, fw.firewallName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create (rFor'' (FirewallCreate fw)))
                    , down = Core.downIfPresent checkFirewall (delete (rFor'' (FirewallDelete fw)))
                    , check = checkFirewall
                    }
  where
    rFor'' cmd = contramap (RunComputeCommand cmd) r

    checkFirewall :: IO CheckResult
    checkFirewall = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode (prepare computeCommand (FirewallDescribe fw)) ""
        pure $ interpretFirewallDescribe fw.firewallName code

-- | The verdict drawn from @gcloud compute firewall-rules describe@.
interpretFirewallDescribe :: Text -> ExitCode -> CheckResult
interpretFirewallDescribe _name ExitSuccess = Success
interpretFirewallDescribe name (ExitFailure _) = Failure ("firewall rule not found: " <> name)

-------------------------------------------------------------------------------

{- | What a subnetwork is /for/, which for one kind of subnet is the whole
point of creating it.

@REGIONAL_MANAGED_PROXY@ is the proxy-only subnet a regional
@EXTERNAL_MANAGED@ Application Load Balancer runs its Envoy proxies in.
Nothing is ever placed in it by hand -- it holds no instances, and its range
is where the load balancer's connections to the backends /come from/, which
is what a backend's firewall rule has to allow. One @ACTIVE@ proxy-only
subnet may exist per network per region, and until it does, every attempt to
create such a balancer's forwarding rule fails.
-}
data SubnetPurpose
    = PrivateSubnet
    | RegionalManagedProxy
    deriving (Eq, Show)

renderSubnetPurpose :: SubnetPurpose -> Text
renderSubnetPurpose PrivateSubnet = "PRIVATE"
renderSubnetPurpose RegionalManagedProxy = "REGIONAL_MANAGED_PROXY"

-- | A subnetwork of a VPC network, in one region.
data Subnet = Subnet
    { subnetName :: Text
    , subnetProject :: Project
    , subnetRegion :: Region
    , subnetNetwork :: Text
    , subnetRange :: Text
    -- ^ CIDR. In an /auto mode/ network (which @default@ is), it must not
    -- overlap @10.128.0.0\/9@: that whole block is reserved for the subnets
    -- GCP creates per region on its own, including regions that do not exist
    -- yet.
    , subnetPurpose :: SubnetPurpose
    }
    deriving (Eq, Show)

-- | Idempotently creates a subnetwork.
subnet :: Reporter Report -> Track' (Binary "gcloud") -> Subnet -> Op
subnet r gcloudTrack net =
    withBinary gcloudTrack computeCommand (SubnetsCreate net) $ \create ->
        withBinary gcloudTrack computeCommand (SubnetsDelete net) $ \delete ->
            op "gcp-subnet" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates subnet", net.subnetName, "for", renderSubnetPurpose net.subnetPurpose]
                    , ref = mkRef "gcp-subnet" (net.subnetProject.projectId, net.subnetRegion.regionName, net.subnetName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create (rSub (SubnetsCreate net)))
                    , down = Core.downIfPresent checkSubnet (delete (rSub (SubnetsDelete net)))
                    , check = checkSubnet
                    }
  where
    rSub cmd = contramap (RunComputeCommand cmd) r

    checkSubnet :: IO CheckResult
    checkSubnet = do
        (code, out, _err) <-
            readCreateProcessWithExitCode (prepare computeCommand (SubnetsDescribe net)) ""
        pure $ interpretSubnetDescribe net.subnetName (renderSubnetPurpose net.subnetPurpose) code (Text.strip (Text.decodeUtf8 out))

{- | The verdict drawn from @gcloud compute networks subnets describe
--format=value(purpose)@, split out for testability.

The purpose is compared rather than merely noting the subnet exists, because
a subnet of the wrong purpose is the one failure mode worth catching here: a
plain subnet answers @describe@ perfectly well and then the balancer refuses
to use it, at a point far away from this node.
-}
interpretSubnetDescribe :: Text -> Text -> ExitCode -> Text -> CheckResult
interpretSubnetDescribe name _ (ExitFailure _) _ = Failure ("subnet not found: " <> name)
interpretSubnetDescribe name wanted ExitSuccess out
    | out == wanted = Success
    -- gcloud renders an ordinary subnet's purpose as PRIVATE, but has also
    -- left it empty in the past; an empty answer is only satisfying if that
    -- is what was asked for.
    | Text.null out && wanted == "PRIVATE" = Success
    | otherwise = Failure ("subnet " <> name <> " has purpose " <> out <> ", wanted " <> wanted)

-------------------------------------------------------------------------------

{- | An /unmanaged/, zonal instance group: a bag of instances that already
exist, which is what makes it the right backend for a load balancer in front
of VMs salmon itself declared. (A managed group is the other way round -- it
creates the instances, from a template.)
-}
data InstanceGroup = InstanceGroup
    { groupName :: Text
    , groupProject :: Project
    , groupZone :: Zone
    }
    deriving (Eq, Show)

-- | Idempotently creates an unmanaged instance group.
instanceGroup :: Reporter Report -> Track' (Binary "gcloud") -> InstanceGroup -> Op
instanceGroup r gcloudTrack grp =
    withBinary gcloudTrack computeCommand (InstanceGroupsCreate grp) $ \create ->
        withBinary gcloudTrack computeCommand (InstanceGroupsDelete grp) $ \delete ->
            op "gcp-instance-group" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates unmanaged instance group", grp.groupName]
                    , ref = mkRef "gcp-instance-group" (grp.groupProject.projectId, grp.groupZone.zoneName, grp.groupName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create (rGrp (InstanceGroupsCreate grp)))
                    , down = Core.downIfPresent checkGroup (delete (rGrp (InstanceGroupsDelete grp)))
                    , check = checkGroup
                    }
  where
    rGrp cmd = contramap (RunComputeCommand cmd) r

    checkGroup :: IO CheckResult
    checkGroup = do
        (code, _out, _err) <-
            readCreateProcessWithExitCode (prepare computeCommand (InstanceGroupsDescribe grp)) ""
        pure $ interpretInstanceGroupDescribe grp.groupName code

-- | The verdict drawn from @gcloud compute instance-groups unmanaged describe@.
interpretInstanceGroupDescribe :: Text -> ExitCode -> CheckResult
interpretInstanceGroupDescribe _name ExitSuccess = Success
interpretInstanceGroupDescribe name (ExitFailure _) = Failure ("instance group not found: " <> name)

{- | One instance's membership of an unmanaged group, as a node of its own
rather than a field of 'InstanceGroup'.

Separate because the two effects have genuinely different lifetimes and
different failure modes: the group can exist while the instance does not,
@add-instances@ is an error if the instance is already in, and a teardown
has to take the membership out before either end can go. Keeping them apart
also means the dependency edge that matters -- "the instance must exist
first" -- is expressible, which it would not be if membership were a field
of the group.
-}
instanceGroupMember :: Reporter Report -> Track' (Binary "gcloud") -> InstanceGroup -> Text -> Op
instanceGroupMember r gcloudTrack grp instName =
    withBinary gcloudTrack computeCommand (InstanceGroupsAddInstance grp instName) $ \add ->
        withBinary gcloudTrack computeCommand (InstanceGroupsRemoveInstance grp instName) $ \remove ->
            op "gcp-instance-group-member" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["adds", instName, "to instance group", grp.groupName]
                    , ref = mkRef "gcp-instance-group-member" (grp.groupProject.projectId, grp.groupZone.zoneName, grp.groupName, instName)
                    , up = add (rMem (InstanceGroupsAddInstance grp instName))
                    , down = Core.downIfPresent checkMember (remove (rMem (InstanceGroupsRemoveInstance grp instName)))
                    , check = checkMember
                    }
  where
    rMem cmd = contramap (RunComputeCommand cmd) r

    checkMember :: IO CheckResult
    checkMember = do
        (code, out, _err) <-
            readCreateProcessWithExitCode (prepare computeCommand (InstanceGroupsListInstances grp)) ""
        pure $ interpretGroupMembership instName code (Text.decodeUtf8 out)

{- | The verdict drawn from @gcloud compute instance-groups list-instances
--format=value(instance)@, whose lines are full resource URLs.

Matching on the last path segment rather than by substring, so that an
instance named @web@ is not read as present because @web-canary@ is.
-}
interpretGroupMembership :: Text -> ExitCode -> Text -> CheckResult
interpretGroupMembership name (ExitFailure _) _ = Failure ("could not list the group's instances (looking for " <> name <> ")")
interpretGroupMembership name ExitSuccess out
    | name `elem` map lastSegment (Text.lines out) = Success
    | otherwise = Failure ("instance not in the group: " <> name)
  where
    lastSegment = last . Text.splitOn "/" . Text.strip

-------------------------------------------------------------------------------

data ComputeCommand
    = InstancesCreate Instance
    | InstancesDescribe Instance
    | InstancesDescribeStatus Instance
    | InstancesDescribeScopes Instance
    | InstancesDescribeMachineType Instance
    | InstancesSetMachineType Instance
    | InstancesDescribeRunLimit Instance
    | InstancesSetScheduling Instance
    | InstancesStart Instance
    | InstancesResume Instance
    | InstancesStop Instance
    | InstancesDelete Instance
    | AddressesCreate Address
    | AddressesDescribe Address
    | AddressesDelete Address
    | FirewallCreate FirewallRule
    | FirewallDescribe FirewallRule
    | FirewallDelete FirewallRule
    | SubnetsCreate Subnet
    | SubnetsDescribe Subnet
    | SubnetsDelete Subnet
    | InstanceGroupsCreate InstanceGroup
    | InstanceGroupsDescribe InstanceGroup
    | InstanceGroupsDelete InstanceGroup
    | InstanceGroupsAddInstance InstanceGroup Text
    | InstanceGroupsRemoveInstance InstanceGroup Text
    | InstanceGroupsListInstances InstanceGroup
    deriving (Show)

computeCommand :: Command "gcloud" ComputeCommand
computeCommand = Command $ \cmd -> case cmd of
    InstancesCreate inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "create"
                    , Text.unpack inst.instanceName
                    , "--machine-type"
                    , Text.unpack (renderMachineType inst.instanceMachineType)
                    , "--boot-disk-size"
                    , show inst.instanceBootDisk.bootDiskSizeGb <> "GB"
                    , "--network"
                    , Text.unpack inst.instanceNetwork
                    , "--subnet"
                    , Text.unpack inst.instanceSubnet
                    ]
                )
                <> maybe [] (\img -> ["--image", Text.unpack img]) inst.instanceBootDisk.bootDiskImage
                <> maybe [] (\fam -> ["--image-family", Text.unpack fam]) inst.instanceBootDisk.bootDiskImageFamily
                <> maybe [] (\proj -> ["--image-project", Text.unpack proj]) inst.instanceBootDisk.bootDiskImageProject
                <> maybe [] (\sa -> ["--service-account", Text.unpack sa]) inst.instanceServiceAccount
                <> concatMap (\(k, v) -> ["--metadata", Text.unpack k <> "=" <> Text.unpack v]) (Map.toList inst.instanceMetadata)
                <> concatMap (\(k, v) -> ["--metadata-from-file", Text.unpack k <> "=" <> v]) (Map.toList inst.instanceMetadataFiles)
                <> ( case inst.instanceExternalAddress of
                        EphemeralExternal -> []
                        ReservedExternal addr -> ["--address", Text.unpack addr]
                        NoExternalAddress -> ["--no-address"]
                   )
                <> ( case inst.instanceInternalAddress of
                        EphemeralInternal -> []
                        PinnedInternal ip -> ["--private-network-ip", Text.unpack ip]
                   )
                <> (if null inst.instanceTags then [] else ["--tags", Text.unpack (Text.intercalate "," inst.instanceTags)])
                <> ( case inst.instanceScopes of
                        DefaultScopes -> []
                        DeclaredScopes [] -> ["--no-scopes"]
                        DeclaredScopes scopes -> ["--scopes", Text.unpack (Text.intercalate "," scopes)]
                   )
                <> runLimitArgs inst.instanceRunLimit
    InstancesDescribe inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "describe"
                    , Text.unpack inst.instanceName
                    ]
                )
    InstancesDescribeStatus inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "describe"
                    , Text.unpack inst.instanceName
                    , "--format=value(status)"
                    ]
                )
    InstancesDescribeScopes inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "describe"
                    , Text.unpack inst.instanceName
                    , "--format=value(serviceAccounts[].scopes)"
                    ]
                )
    InstancesDescribeMachineType inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "describe"
                    , Text.unpack inst.instanceName
                    , "--format=value(machineType.basename())"
                    ]
                )
    InstancesSetMachineType inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "set-machine-type"
                    , Text.unpack inst.instanceName
                    , "--machine-type"
                    , Text.unpack (renderMachineType inst.instanceMachineType)
                    ]
                )
    InstancesDescribeRunLimit inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "describe"
                    , Text.unpack inst.instanceName
                    , "--format=json(lastStartTimestamp,scheduling.maxRunDuration.seconds,scheduling.terminationTime,scheduling.instanceTerminationAction)"
                    ]
                )
    -- the declared limit, and the other kind of limit cleared so that the
    -- instance holds exactly what is declared
    InstancesSetScheduling inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    ( [ "compute"
                      , "instances"
                      , "set-scheduling"
                      , Text.unpack inst.instanceName
                      ]
                        <> runLimitArgs inst.instanceRunLimit
                        <> ( case inst.instanceRunLimit of
                                NoRunLimit -> ["--clear-max-run-duration", "--clear-termination-time"]
                                MaxRunDuration _ -> ["--clear-termination-time"]
                                RunUntil _ -> ["--clear-max-run-duration"]
                           )
                    )
                )
    InstancesStart inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "start"
                    , Text.unpack inst.instanceName
                    ]
                )
    InstancesStop inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "stop"
                    , Text.unpack inst.instanceName
                    ]
                )
    InstancesResume inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "resume"
                    , Text.unpack inst.instanceName
                    ]
                )
    InstancesDelete inst ->
        gcloudProc $
            withProject inst.instanceProject
                ( withZone inst.instanceZone
                    [ "compute"
                    , "instances"
                    , "delete"
                    , Text.unpack inst.instanceName
                    , "--quiet"
                    ]
                )
    AddressesCreate addr ->
        gcloudProc $
            withProject addr.addressProject
                [ "compute"
                , "addresses"
                , "create"
                , Text.unpack addr.addressName
                , "--region"
                , Text.unpack addr.addressRegion.regionName
                ]
                -- gcloud has no "internal" switch: naming a subnet is what
                -- makes the reservation an internal one.
                <> case addr.addressKind of
                    ExternalAddress -> []
                    InternalAddress sub ip ->
                        ["--subnet", Text.unpack sub] <> maybe [] (\lit -> ["--addresses", Text.unpack lit]) ip
    AddressesDescribe addr ->
        gcloudProc $
            withProject addr.addressProject
                [ "compute"
                , "addresses"
                , "describe"
                , Text.unpack addr.addressName
                , "--region"
                , Text.unpack addr.addressRegion.regionName
                , "--format=value(address)"
                ]
    AddressesDelete addr ->
        gcloudProc $
            withProject addr.addressProject
                [ "compute"
                , "addresses"
                , "delete"
                , Text.unpack addr.addressName
                , "--region"
                , Text.unpack addr.addressRegion.regionName
                , "--quiet"
                ]
    FirewallCreate fw ->
        gcloudProc $
            withProject fw.firewallProject
                [ "compute"
                , "firewall-rules"
                , "create"
                , Text.unpack fw.firewallName
                , "--network"
                , Text.unpack fw.firewallNetwork
                , "--allow"
                , Text.unpack fw.firewallAllow
                , "--source-ranges"
                , Text.unpack (Text.intercalate "," fw.firewallSourceRanges)
                ]
                <> if null fw.firewallTargetTags then [] else ["--target-tags", Text.unpack (Text.intercalate "," fw.firewallTargetTags)]
    FirewallDescribe fw ->
        gcloudProc $
            withProject fw.firewallProject
                [ "compute"
                , "firewall-rules"
                , "describe"
                , Text.unpack fw.firewallName
                ]
    FirewallDelete fw ->
        gcloudProc $
            withProject fw.firewallProject
                [ "compute"
                , "firewall-rules"
                , "delete"
                , Text.unpack fw.firewallName
                , "--quiet"
                ]
    SubnetsCreate net ->
        gcloudProc $
            withProject net.subnetProject
                [ "compute"
                , "networks"
                , "subnets"
                , "create"
                , Text.unpack net.subnetName
                , "--network"
                , Text.unpack net.subnetNetwork
                , "--region"
                , Text.unpack net.subnetRegion.regionName
                , "--range"
                , Text.unpack net.subnetRange
                , "--purpose"
                , Text.unpack (renderSubnetPurpose net.subnetPurpose)
                ]
                -- a proxy-only subnet is either the region's ACTIVE one or a
                -- BACKUP held for a migration; gcloud demands the choice.
                <> case net.subnetPurpose of
                    RegionalManagedProxy -> ["--role", "ACTIVE"]
                    PrivateSubnet -> []
    SubnetsDescribe net ->
        gcloudProc $
            withProject net.subnetProject
                [ "compute"
                , "networks"
                , "subnets"
                , "describe"
                , Text.unpack net.subnetName
                , "--region"
                , Text.unpack net.subnetRegion.regionName
                , "--format=value(purpose)"
                ]
    SubnetsDelete net ->
        gcloudProc $
            withProject net.subnetProject
                [ "compute"
                , "networks"
                , "subnets"
                , "delete"
                , Text.unpack net.subnetName
                , "--region"
                , Text.unpack net.subnetRegion.regionName
                , "--quiet"
                ]
    InstanceGroupsCreate grp ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    ["compute", "instance-groups", "unmanaged", "create", Text.unpack grp.groupName]
                )
    InstanceGroupsDescribe grp ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    ["compute", "instance-groups", "unmanaged", "describe", Text.unpack grp.groupName]
                )
    InstanceGroupsDelete grp ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    ["compute", "instance-groups", "unmanaged", "delete", Text.unpack grp.groupName, "--quiet"]
                )
    InstanceGroupsAddInstance grp instName ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    [ "compute"
                    , "instance-groups"
                    , "unmanaged"
                    , "add-instances"
                    , Text.unpack grp.groupName
                    , "--instances"
                    , Text.unpack instName
                    ]
                )
    InstanceGroupsRemoveInstance grp instName ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    [ "compute"
                    , "instance-groups"
                    , "unmanaged"
                    , "remove-instances"
                    , Text.unpack grp.groupName
                    , "--instances"
                    , Text.unpack instName
                    ]
                )
    InstanceGroupsListInstances grp ->
        gcloudProc $
            withProject grp.groupProject
                ( withZone
                    grp.groupZone
                    [ "compute"
                    , "instance-groups"
                    , "list-instances"
                    , Text.unpack grp.groupName
                    , "--format=value(instance)"
                    ]
                )
