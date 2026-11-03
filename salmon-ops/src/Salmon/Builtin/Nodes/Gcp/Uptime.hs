{-# LANGUAGE OverloadedStrings #-}

{- | Cloud Monitoring uptime checks on a public HTTP(S) URL.

An uptime check gets a __server-assigned id__, so, as for the alert policies
of "Salmon.Builtin.Nodes.Gcp.Monitoring", the identity is the __display
name__ within the project: @check@ lists the project's checks and picks the
one with that name.

Unlike a policy, a check is compared __field by field__, not by a
fingerprint: the host, the protocol, the port, the path, the period, the
timeout and the checker regions are read back from @list-configs@ and
compared with the declaration, so an edit in the console is seen.

A check salmon creates carries the 'Monitoring.ownerLabelKey' user label.
One found by display name __without__ it was not made by salmon: @check@ is
a 'Failure' saying so, @up@ refuses to touch it and @down@ leaves it. With
the label, @up@ updates what can be updated in place (path, port, period,
timeout, regions); a check on another host or protocol cannot be moved
(the API has no flag for it) and @up@ refuses, since deleting and
recreating it gives it a new id that an alert condition may name
('Monitoring.uptimeCheckFailing' takes that id).

Everything goes through @gcloud monitoring uptime@ (GA) and needs
@monitoring.googleapis.com@ enabled ('Monitoring.monitoringApi').
-}
module Salmon.Builtin.Nodes.Gcp.Uptime (
    UptimeProtocol (..),
    UptimeRegion (..),
    allUptimeRegions,
    UptimeCheck (..),
    httpsUptimeCheck,
    uptimeCheck,
    uptimeProblems,
    effectivePort,
    FoundUptime (..),
    lookupUptime,
    interpretUptimeList,
    Report (..),
    UptimeCommand (..),
    uptimeCommand,
) where

import Control.Exception (throwIO)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.IO.Error (userError)
import System.Process.ByteString (readCreateProcessWithExitCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), untrackedExec, withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), gcloudProc, withProject)
import Salmon.Builtin.Nodes.Gcp.Monitoring (Lookup (..), firstLine, nestedTextField, ownerLabelKey, ownerLabelValue, shortId, textField)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunUptimeCommand !UptimeCommand !Binary.Report
    deriving (Show)

data UptimeProtocol = Http | Https
    deriving (Eq, Show, Enum, Bounded)

-- | The checker regions gcloud names; Google wants at least three.
data UptimeRegion
    = AsiaPacific
    | Europe
    | SouthAmerica
    | UsaIowa
    | UsaOregon
    | UsaVirginia
    deriving (Eq, Ord, Show, Enum, Bounded)

allUptimeRegions :: [UptimeRegion]
allUptimeRegions = [minBound .. maxBound]

-- | The region as @--regions@ spells it.
regionFlag :: UptimeRegion -> Text
regionFlag r = case r of
    AsiaPacific -> "asia-pacific"
    Europe -> "europe"
    SouthAmerica -> "south-america"
    UsaIowa -> "usa-iowa"
    UsaOregon -> "usa-oregon"
    UsaVirginia -> "usa-virginia"

-- | The region as the API's @selectedRegions@ spells it.
regionApi :: UptimeRegion -> Text
regionApi = Text.toUpper . Text.replace "-" "_" . regionFlag

data UptimeCheck = UptimeCheck
    { ucProject :: Project
    , ucDisplayName :: Text
    -- ^ the identity, see the module header
    , ucHost :: Text
    -- ^ the @uptime_url@ resource's host, e.g. @api.example.org@
    , ucProtocol :: UptimeProtocol
    , ucPort :: Maybe Int
    -- ^ 'Nothing': 80 for 'Http', 443 for 'Https' (always passed explicitly)
    , ucPath :: Text
    -- ^ e.g. @\/healthz@
    , ucPeriodMinutes :: Int
    -- ^ one of 1, 5, 10, 15
    , ucTimeoutSeconds :: Int
    -- ^ 1 to 60
    , ucRegions :: [UptimeRegion]
    -- ^ at least three
    }
    deriving (Eq, Show)

-- | An HTTPS check of @path@ on @host@, every minute, 10s timeout, from
-- every region.
httpsUptimeCheck :: Project -> Text -> Text -> Text -> UptimeCheck
httpsUptimeCheck project name host path =
    UptimeCheck
        { ucProject = project
        , ucDisplayName = name
        , ucHost = host
        , ucProtocol = Https
        , ucPort = Nothing
        , ucPath = path
        , ucPeriodMinutes = 1
        , ucTimeoutSeconds = 10
        , ucRegions = allUptimeRegions
        }

effectivePort :: UptimeCheck -> Int
effectivePort uc = fromMaybe (case uc.ucProtocol of Http -> 80; Https -> 443) uc.ucPort

-- | What gcloud or Google would refuse, checked by @up@ before any command.
uptimeProblems :: UptimeCheck -> [Text]
uptimeProblems uc =
    ["the period must be 1, 5, 10 or 15 minutes" | uc.ucPeriodMinutes `notElem` [1, 5, 10, 15]]
        <> ["the timeout must be 1 to 60 seconds" | uc.ucTimeoutSeconds < 1 || uc.ucTimeoutSeconds > 60]
        <> ["an uptime check needs at least three regions" | length (nub uc.ucRegions) < 3]
        <> ["the path must start with /" | not ("/" `Text.isPrefixOf` uc.ucPath)]
        <> ["the host must not be empty" | Text.null uc.ucHost]

{- | An uptime check, found by display name. @up@ creates it, or updates a
salmon-labelled one that differs; @down@ deletes a salmon-labelled one.
-}
uptimeCheck :: Reporter Report -> Track' (Binary "gcloud") -> UptimeCheck -> Op
uptimeCheck r gcloudTrack uc =
    withBinary gcloudTrack uptimeCommand (UptimeList uc.ucProject) $ \_list ->
        op "gcp-uptime-check" nodeps $ \actions ->
            actions
                { help = Text.unwords ["uptime check", uc.ucDisplayName, "on", protocolText uc.ucProtocol <> "://" <> uc.ucHost <> ":" <> showT (effectivePort uc) <> uc.ucPath]
                , notes =
                    [ "every " <> showT uc.ucPeriodMinutes <> "min, timeout " <> showT uc.ucTimeoutSeconds <> "s"
                    , "regions: " <> Text.intercalate "," (map regionFlag (sort (nub uc.ucRegions)))
                    ]
                , ref = mkRef "gcp-uptime-check" (uc.ucProject.projectId, uc.ucDisplayName)
                , up = upCheck
                , down = downCheck
                , check = interpretUptimeList uc <$> list
                }
  where
    run :: UptimeCommand -> IO ()
    run cmd = untrackedExec uptimeCommand cmd "" (contramap (RunUptimeCommand cmd) r)

    list :: IO (ExitCode, ByteString, ByteString)
    list = readCreateProcessWithExitCode (prepare uptimeCommand (UptimeList uc.ucProject)) ""

    refuse :: Text -> IO a
    refuse why = throwIO (userError (Text.unpack ("uptime check " <> uc.ucDisplayName <> ": " <> why)))

    upCheck = do
        case uptimeProblems uc of
            [] -> pure ()
            problems -> refuse (Text.intercalate "; " problems)
        (code, out, err) <- list
        case lookupUptime uc code out err of
            LookupFailed why -> refuse ("listing uptime checks failed: " <> why)
            Absent -> run (UptimeCreate uc)
            Present found
                | not found.fuOwned -> refuse "exists without salmon's label, so salmon did not make it; not taking it over"
                | not found.fuSameTarget -> refuse "exists on another host or protocol, which cannot be changed in place; delete it to recreate it (its check id will change)"
                | found.fuMatches -> pure ()
                | otherwise -> run (UptimeUpdate (shortId found.fuName) uc)

    downCheck = do
        (code, out, err) <- list
        case lookupUptime uc code out err of
            Present found | found.fuOwned -> run (UptimeDelete uc.ucProject (shortId found.fuName))
            -- absent, unreachable, or not salmon's: leave it
            _ -> pure ()

protocolText :: UptimeProtocol -> Text
protocolText Http = "http"
protocolText Https = "https"

showT :: (Show a) => a -> Text
showT = Text.pack . show

-- | An uptime check found by display name.
data FoundUptime = FoundUptime
    { fuName :: Text
    -- ^ @projects\/P\/uptimeCheckConfigs\/ID@
    , fuOwned :: Bool
    -- ^ carries salmon's label
    , fuSameTarget :: Bool
    -- ^ same host and protocol (what an update cannot change)
    , fuMatches :: Bool
    -- ^ every declared field as declared
    }
    deriving (Eq, Show)

-- | Pure: what @gcloud monitoring uptime list-configs --format json@ said
-- about the declared check.
lookupUptime :: UptimeCheck -> ExitCode -> ByteString -> ByteString -> Lookup FoundUptime
lookupUptime uc code out err =
    case code of
        ExitFailure n -> LookupFailed (Text.pack ("exit " <> show n) <> firstLine err)
        ExitSuccess -> case Aeson.decodeStrict' out of
            Just (Array items) | (item : _) <- filter named (toList items) ->
                let sameTarget =
                        nestedTextField "monitoredResource" "type" item == Just "uptime_url"
                            && labelOf "host" item == Just uc.ucHost
                            && boolAt ["httpCheck", "useSsl"] item == (uc.ucProtocol == Https)
                 in Present
                        FoundUptime
                            { fuName = fromMaybe "" (textField "name" item)
                            , fuOwned = nestedTextField "userLabels" ownerLabelKey item == Just ownerLabelValue
                            , fuSameTarget = sameTarget
                            , fuMatches =
                                sameTarget
                                    && intAt ["httpCheck", "port"] item == Just (effectivePort uc)
                                    && textAt ["httpCheck", "path"] item == Just uc.ucPath
                                    && textField "period" item == Just (showT (uc.ucPeriodMinutes * 60) <> "s")
                                    && textField "timeout" item == Just (showT uc.ucTimeoutSeconds <> "s")
                                    && sort (nub (regionsOf item)) == sort (nub (map regionApi uc.ucRegions))
                            }
            Just (Array _) -> Absent
            _ -> LookupFailed "unparseable list output"
  where
    named item = textField "displayName" item == Just uc.ucDisplayName
    labelOf k item = at ["monitoredResource", "labels", k] item >>= asText
    regionsOf item = case at ["selectedRegions"] item of
        Just (Array rs) -> mapMaybe asText (toList rs)
        _ -> []

-- | The 'CheckResult' for a check: there, salmon's, and as declared.
interpretUptimeList :: UptimeCheck -> (ExitCode, ByteString, ByteString) -> CheckResult
interpretUptimeList uc (code, out, err) =
    case lookupUptime uc code out err of
        LookupFailed why -> Failure ("cannot list uptime checks: " <> why)
        Absent -> Failure ("no uptime check named " <> uc.ucDisplayName)
        Present found
            | not found.fuOwned -> Failure ("uptime check " <> uc.ucDisplayName <> " exists without salmon's label")
            | found.fuMatches -> Success
            | otherwise -> Failure ("uptime check " <> uc.ucDisplayName <> " differs from its declaration")

at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KeyMap.lookup (Key.fromText k) o >>= at ks
at _ _ = Nothing

asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing

textAt :: [Text] -> Value -> Maybe Text
textAt ks v = at ks v >>= asText

boolAt :: [Text] -> Value -> Bool
boolAt ks v = at ks v == Just (Bool True)

-- | A number, or a number in a string (int32 fields are numbers in the API's
-- JSON, but be lenient).
intAt :: [Text] -> Value -> Maybe Int
intAt ks v = case at ks v of
    Just n@(Number _) | Aeson.Success i <- Aeson.fromJSON n -> Just i
    Just (String t) | [(n, "")] <- reads (Text.unpack t) -> Just n
    _ -> Nothing

-------------------------------------------------------------------------------

data UptimeCommand
    = UptimeList Project
    | UptimeCreate UptimeCheck
    | -- | update the check with this id (the last segment of its name)
      UptimeUpdate Text UptimeCheck
    | UptimeDelete Project Text
    deriving (Show)

regionsArg :: UptimeCheck -> String
regionsArg uc = Text.unpack (Text.intercalate "," (map regionFlag (sort (nub uc.ucRegions))))

uptimeCommand :: Command "gcloud" UptimeCommand
uptimeCommand = Command $ \cmd -> case cmd of
    UptimeList project ->
        gcloudProc $ withProject project ["monitoring", "uptime", "list-configs", "--format", "json"]
    UptimeCreate uc ->
        gcloudProc $
            withProject
                uc.ucProject
                [ "monitoring"
                , "uptime"
                , "create"
                , Text.unpack uc.ucDisplayName
                , "--resource-type"
                , "uptime-url"
                , "--resource-labels"
                , Text.unpack ("host=" <> uc.ucHost <> ",project_id=" <> uc.ucProject.projectId)
                , "--protocol"
                , Text.unpack (protocolText uc.ucProtocol)
                , "--port"
                , show (effectivePort uc)
                , "--path"
                , Text.unpack uc.ucPath
                , "--period"
                , show uc.ucPeriodMinutes
                , "--timeout"
                , show uc.ucTimeoutSeconds
                , "--regions"
                , regionsArg uc
                , "--user-labels"
                , Text.unpack (ownerLabelKey <> "=" <> ownerLabelValue)
                ]
    UptimeUpdate checkId uc ->
        gcloudProc $
            withProject
                uc.ucProject
                [ "monitoring"
                , "uptime"
                , "update"
                , Text.unpack checkId
                , "--port"
                , show (effectivePort uc)
                , "--path"
                , Text.unpack uc.ucPath
                , "--period"
                , show uc.ucPeriodMinutes
                , "--timeout"
                , show uc.ucTimeoutSeconds
                , "--set-regions"
                , regionsArg uc
                ]
    UptimeDelete project checkId ->
        gcloudProc $ withProject project ["monitoring", "uptime", "delete", Text.unpack checkId, "--quiet"]
