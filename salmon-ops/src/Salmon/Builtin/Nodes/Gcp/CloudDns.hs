{-# LANGUAGE OverloadedStrings #-}

{- | Cloud DNS managed zones: the zone itself, and a read of the name servers
Cloud DNS assigned to it.

The zone is split from the records that go in it because two things need it
before any record exists. The assigned name servers are what an operator
enters at the registrar to delegate the domain, and until that is done
nothing in the zone is visible to the world -- delegation takes time to
propagate, so it is worth starting early. And a check that compares the
delegation the parent servers hand out with what the zone was assigned needs
exactly this set.

Like an external "Salmon.Builtin.Nodes.Gcp.Compute".@Address@, the name
servers cannot be known when the graph is /declared/: Cloud DNS picks them
(one of several shards, @ns-cloud-a1@ … @ns-cloud-e4@) at creation. So they
are an out-of-graph read, 'readNameServers', for a driver to use after the
pass that created the zone.

Enabling the API ('dnsApi') is the caller's dependency to declare, as with
every other node under "Salmon.Builtin.Nodes.Gcp".
-}
module Salmon.Builtin.Nodes.Gcp.CloudDns (
    dnsApi,
    ManagedZone (..),
    managedZone,
    readNameServers,
    ZoneDescription (..),
    parseZoneDescribe,
    interpretZoneDescribe,
    fqdn,
    Report (..),
    CloudDnsCommand (..),
    cloudDnsCommand,
) where

import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.!=), (.:), (.:?))
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), gcloudProc, withProject)
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunCloudDnsCommand !CloudDnsCommand !Binary.Report
    deriving (Show)

-- | The API a project needs enabled before any zone can be created.
dnsApi :: Text
dnsApi = "dns.googleapis.com"

-------------------------------------------------------------------------------

{- | A public Cloud DNS managed zone.

'zoneName' is the resource's name within the project (what every later
@gcloud dns@ command addresses); 'zoneDnsName' is the domain it is
authoritative for, with or without the trailing dot.
-}
data ManagedZone = ManagedZone
    { zoneName :: Text
    , zoneProject :: Project
    , zoneDnsName :: Text
    , zoneDescription :: Text
    -- ^ @gcloud dns managed-zones create@ requires one
    }
    deriving (Eq, Show)

-- | A DNS name as Cloud DNS spells it: lower-case, with the trailing dot.
fqdn :: Text -> Text
fqdn name = Text.dropWhileEnd (== '.') (Text.toLower (Text.strip name)) <> "."

{- | Idempotently creates a public managed zone.

* 'check': describes the zone; 'Success' if it exists /for the declared DNS
  name/. A zone of that name serving another domain is a 'Failure' naming
  both, and @up@ then fails on gcloud's "already exists" rather than taking
  the zone over: a zone's DNS name cannot be changed, and deleting somebody
  else's zone to make room is not this node's call.
* 'up': creates the zone.
* 'down': deletes it, if it is there. Cloud DNS refuses to delete a zone
  that still holds records other than its own apex @NS@ and @SOA@, so record
  nodes declared on top of the zone have to go down first -- which is what
  depending on this node gets them.

The 'ref' is keyed on the project and the zone's name, not on the DNS name:
that pair is the effect site.
-}
managedZone :: Reporter Report -> Track' (Binary "gcloud") -> ManagedZone -> Op
managedZone r gcloudTrack zone =
    withBinary gcloudTrack cloudDnsCommand (ZonesCreate zone) $ \create ->
        withBinary gcloudTrack cloudDnsCommand (ZonesDelete zone) $ \delete ->
            op "gcp-dns-zone" nodeps $ \actions ->
                actions
                    { help = Text.unwords ["creates Cloud DNS zone", zone.zoneName, "for", fqdn zone.zoneDnsName]
                    , ref = mkRef "gcp-dns-zone" (zone.zoneProject.projectId, zone.zoneName)
                    , up = Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay (create (rFor (ZonesCreate zone)))
                    , -- a zone of that name serving another domain reads
                      -- 'Failure' here too, so it is left alone
                      down = Core.downIfPresent checkZone (delete (rFor (ZonesDelete zone)))
                    , check = checkZone
                    }
  where
    rFor cmd = contramap (RunCloudDnsCommand cmd) r

    checkZone :: IO CheckResult
    checkZone = do
        (code, out) <- describeZone zone
        pure $ interpretZoneDescribe zone code out

describeZone :: ManagedZone -> IO (ExitCode, ByteString)
describeZone zone = do
    (code, out, _err) <-
        readCreateProcessWithExitCode (prepare cloudDnsCommand (ZonesDescribe zone)) ""
    pure (code, out)

{- | The name servers Cloud DNS assigned to the zone, in the order it lists
them and with their trailing dots. 'Nothing' when the zone does not exist
(or its description could not be read).

Out of graph, like "Salmon.Builtin.Nodes.Gcp.Compute".@readAddress@: these
are what goes to the registrar, and what a delegation check compares against.
-}
readNameServers :: ManagedZone -> IO (Maybe [Text])
readNameServers zone = do
    (code, out) <- describeZone zone
    pure $ case (code, parseZoneDescribe out) of
        (ExitSuccess, Right desc) | not (null desc.describedNameServers) -> Just desc.describedNameServers
        _ -> Nothing

-------------------------------------------------------------------------------

-- | What this module reads of @gcloud dns managed-zones describe --format json@.
data ZoneDescription = ZoneDescription
    { describedDnsName :: Text
    , describedNameServers :: [Text]
    }
    deriving (Eq, Show)

instance FromJSON ZoneDescription where
    parseJSON = withObject "ManagedZone" $ \o ->
        ZoneDescription <$> o .: "dnsName" <*> o .:? "nameServers" .!= []

parseZoneDescribe :: ByteString -> Either String ZoneDescription
parseZoneDescribe = eitherDecodeStrict'

{- | The verdict drawn from @gcloud dns managed-zones describe@'s exit code
and JSON output, split out for testability.
-}
interpretZoneDescribe :: ManagedZone -> ExitCode -> ByteString -> CheckResult
interpretZoneDescribe zone (ExitFailure _) _ =
    Failure ("DNS zone not found: " <> zone.zoneName)
interpretZoneDescribe zone ExitSuccess out =
    case parseZoneDescribe out of
        -- it answered, and what it said could not be read: cannot tell
        Left _ -> Unknown
        Right desc
            | fqdn desc.describedDnsName == fqdn zone.zoneDnsName -> Success
            | otherwise ->
                Failure
                    ( Text.unwords
                        [ "DNS zone"
                        , zone.zoneName
                        , "exists for"
                        , fqdn desc.describedDnsName <> ","
                        , "not for"
                        , fqdn zone.zoneDnsName
                        ]
                    )

-------------------------------------------------------------------------------

data CloudDnsCommand
    = ZonesCreate ManagedZone
    | ZonesDescribe ManagedZone
    | ZonesDelete ManagedZone
    deriving (Show)

cloudDnsCommand :: Command "gcloud" CloudDnsCommand
cloudDnsCommand = Command $ \cmd -> case cmd of
    ZonesCreate z ->
        gcloudProc $
            withProject z.zoneProject
                [ "dns"
                , "managed-zones"
                , "create"
                , Text.unpack z.zoneName
                , "--dns-name"
                , Text.unpack (fqdn z.zoneDnsName)
                , "--description"
                , Text.unpack z.zoneDescription
                , "--visibility"
                , "public"
                ]
    ZonesDescribe z ->
        gcloudProc $
            withProject z.zoneProject
                [ "dns"
                , "managed-zones"
                , "describe"
                , Text.unpack z.zoneName
                , "--format"
                , "json"
                ]
    ZonesDelete z ->
        gcloudProc $
            withProject z.zoneProject
                [ "dns"
                , "managed-zones"
                , "delete"
                , Text.unpack z.zoneName
                , "--quiet"
                ]
