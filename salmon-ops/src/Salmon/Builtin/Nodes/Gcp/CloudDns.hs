{-# LANGUAGE OverloadedStrings #-}

{- | Cloud DNS: a managed zone, a read of the name servers Cloud DNS assigned
to it, and the record sets that go in it ('recordSet').

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

    -- * record sets
    RecordType (..),
    renderRecordType,
    RecordSet (..),
    recordSet,
    resolvedRecordSet,
    recordSetProblems,
    RecordDescription (..),
    parseRecordDescribe,
    interpretRecordDescribe,
    recordUpCommand,
    renderRrdatas,
    txtRdata,
    txtContent,
    Report (..),
    CloudDnsCommand (..),
    cloudDnsCommand,
) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.!=), (.:), (.:?))
import Data.ByteString (ByteString)
import Data.List (sort)
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
-- Record sets

-- | The record types this module writes.
data RecordType = A | AAAA | CNAME | TXT
    deriving (Eq, Ord, Show)

renderRecordType :: RecordType -> Text
renderRecordType = Text.pack . show

{- | A record set: every record of one type at one name.

'recordName' is the full name (@www.example.org@, with or without the
trailing dot), not a label relative to the zone. 'recordData' is one entry
per record, in the type's own terms:

* 'A', 'AAAA': an address literal;
* 'CNAME': the one target name (the trailing dot is added);
* 'TXT': the text itself, /unquoted/ -- quoting, escaping and the split into
  255-character strings that the wire format wants are 'txtRdata''s job.
-}
data RecordSet = RecordSet
    { recordZone :: ManagedZone
    , recordName :: Text
    , recordType :: RecordType
    , recordTtl :: Int
    -- ^ seconds
    , recordData :: [Text]
    }
    deriving (Eq, Show)

{- | Idempotently makes a record set hold exactly the declared data.

* 'check': describes the record set and compares its TTL and data with what
  is declared (as a set: Cloud DNS does not promise an order). Absent, or
  holding anything else, is a 'Failure' saying which; a declaration that
  cannot be written ('recordSetProblems') is a 'Failure' too.
* 'up': creates the record set, or updates it in place when one already
  exists at that name and type -- so a changed address is a rewrite of this
  node, and a record made by hand is taken over. That is deliberate, and the
  opposite of what 'managedZone' does with a zone it did not make: the name
  and type are the effect site and can hold only one set, so "declared" has
  to mean "this is what it holds".
* 'down': deletes the record set at that name and type if there is one,
  whatever it holds, for the same reason (and because the zone under it
  cannot be deleted while it is there).

The node has no dependencies of its own: the zone may be one this graph
declares ('managedZone', which the caller then puts underneath with
@inject@ -- and must, for @down@ to be ordered) or one that already exists.

The 'ref' is keyed on project, zone, name and type; TTL and data ride in
'notes', so a re-declaration with another address is a changed node under
@run serve@.
-}
recordSet :: Reporter Report -> Track' (Binary "gcloud") -> RecordSet -> Op
recordSet r gcloudTrack rs =
    recordSetNode r gcloudTrack rs (pure (Just rs.recordData)) $
        [ "ttl " <> Text.pack (show rs.recordTtl)
        , Text.unwords (sort (map (normalizeDatum rs.recordType) rs.recordData))
        ]

{- | 'recordSet' for data that cannot be known when the graph is declared --
the address GCP picked for a balancer or a reserved IP, which exists only
after the node that makes it has run. The data is read by the given action
each time the node is checked or brought up; 'recordData' of the argument is
not read.

'Nothing' means "not readable yet": the 'check' then answers 'Unknown' and
@up@ throws. Put the node that makes the value underneath this one.

What the 'notes' cannot say here is the data, so under @run serve@ a change
of the resolved value is noticed by this node's 'check', not by a
re-declaration.
-}
resolvedRecordSet :: Reporter Report -> Track' (Binary "gcloud") -> RecordSet -> IO (Maybe [Text]) -> Op
resolvedRecordSet r gcloudTrack rs resolve =
    recordSetNode r gcloudTrack rs resolve ["ttl " <> Text.pack (show rs.recordTtl), "data resolved when applied"]

recordSetNode :: Reporter Report -> Track' (Binary "gcloud") -> RecordSet -> IO (Maybe [Text]) -> [Text] -> Op
recordSetNode r gcloudTrack rs0 resolve noteLines =
    withBinary gcloudTrack cloudDnsCommand (RecordSetsDelete rs0) $ \delete ->
        op "gcp-dns-record" nodeps $ \actions ->
            actions
                { help = Text.unwords ["sets DNS record", renderRecordType rs0.recordType, fqdn rs0.recordName, "in zone", rs0.recordZone.zoneName]
                , notes = noteLines
                , ref =
                    mkRef
                        "gcp-dns-record"
                        ( rs0.recordZone.zoneProject.projectId
                        , rs0.recordZone.zoneName
                        , fqdn rs0.recordName
                        , renderRecordType rs0.recordType
                        )
                , up = bringUp
                , down = do
                    (code, _out) <- describeRecord rs0
                    case code of
                        ExitSuccess -> delete (rFor (RecordSetsDelete rs0))
                        ExitFailure _ -> pure ()
                , check = checkRecord
                }
  where
    rFor cmd = contramap (RunCloudDnsCommand cmd) r

    resolved :: IO (Maybe RecordSet)
    resolved = fmap (\ds -> rs0{recordData = ds}) <$> resolve

    checkRecord :: IO CheckResult
    checkRecord = do
        mrs <- resolved
        case mrs of
            Nothing -> pure Unknown
            Just rs -> do
                (code, out) <- describeRecord rs
                pure $ interpretRecordDescribe rs code out

    bringUp :: IO ()
    bringUp = do
        mrs <- resolved
        rs <- case mrs of
            Nothing -> throwIO (userError ("the data for DNS record " <> Text.unpack (fqdn rs0.recordName) <> " could not be read"))
            Just rs -> pure rs
        case recordSetProblems rs of
            [] -> pure ()
            problems -> throwIO (userError (Text.unpack (problemText rs problems)))
        Core.retryingIO Core.afterEnableRetries Core.afterEnableDelay $ do
            (code, _out) <- describeRecord rs
            let cmd = recordUpCommand rs code
            Binary.untrackedExec cloudDnsCommand cmd "" (rFor cmd)

describeRecord :: RecordSet -> IO (ExitCode, ByteString)
describeRecord rs = do
    (code, out, _err) <-
        readCreateProcessWithExitCode (prepare cloudDnsCommand (RecordSetsDescribe rs)) ""
    pure (code, out)

{- | What @up@ runs, given whether @describe@ found a record set at that name
and type: there is no "set" verb, @create@ fails on one that exists and
@update@ on one that does not.
-}
recordUpCommand :: RecordSet -> ExitCode -> CloudDnsCommand
recordUpCommand rs ExitSuccess = RecordSetsUpdate rs
recordUpCommand rs (ExitFailure _) = RecordSetsCreate rs

{- | Why a declared record set cannot be written, if it cannot. Checked
before anything is sent, since Cloud DNS's own refusals come back after the
retries and say less.
-}
recordSetProblems :: RecordSet -> [Text]
recordSetProblems rs =
    ["it has no data" | null rs.recordData]
        <> ["a CNAME has exactly one target" | rs.recordType == CNAME, length rs.recordData /= 1]
        <> ["a CNAME cannot sit at the zone's apex" | rs.recordType == CNAME, name == apex]
        <> [name <> " is not in " <> apex | name /= apex, not (("." <> apex) `Text.isSuffixOf` name)]
        <> ["the TTL is negative" | rs.recordTtl < 0]
  where
    name = fqdn rs.recordName
    apex = fqdn rs.recordZone.zoneDnsName

problemText :: RecordSet -> [Text] -> Text
problemText rs problems =
    Text.unwords ["DNS record", renderRecordType rs.recordType, fqdn rs.recordName, "cannot be written:"]
        <> " "
        <> Text.intercalate "; " problems

-- | What this module reads of @gcloud dns record-sets describe --format json@.
data RecordDescription = RecordDescription
    { describedTtl :: Int
    , describedRrdatas :: [Text]
    }
    deriving (Eq, Show)

instance FromJSON RecordDescription where
    parseJSON = withObject "ResourceRecordSet" $ \o ->
        RecordDescription <$> o .: "ttl" <*> o .:? "rrdatas" .!= []

parseRecordDescribe :: ByteString -> Either String RecordDescription
parseRecordDescribe = eitherDecodeStrict'

{- | The verdict drawn from @gcloud dns record-sets describe@'s exit code and
JSON output, split out for testability.

The reasons quote the record's data: DNS records are public.
-}
interpretRecordDescribe :: RecordSet -> ExitCode -> ByteString -> CheckResult
interpretRecordDescribe rs code out
    | problems@(_ : _) <- recordSetProblems rs = Failure (problemText rs problems)
    | ExitFailure _ <- code = Failure (Text.unwords ["DNS record not found:", label])
    | otherwise = case parseRecordDescribe out of
        -- it answered, and what it said could not be read: cannot tell
        Left _ -> Unknown
        Right desc
            | live desc /= wanted ->
                Failure (Text.unwords ["DNS record", label, "holds", shown (live desc) <> ",", "not", shown wanted])
            | desc.describedTtl /= rs.recordTtl ->
                Failure
                    ( Text.unwords
                        ["DNS record", label, "has TTL", Text.pack (show desc.describedTtl) <> ",", "not", Text.pack (show rs.recordTtl)]
                    )
            | otherwise -> Success
  where
    label = Text.unwords [renderRecordType rs.recordType, fqdn rs.recordName]
    wanted = sort (map (normalizeDatum rs.recordType) rs.recordData)
    live :: RecordDescription -> [Text]
    live desc = sort (map (normalizeRdata rs.recordType) desc.describedRrdatas)
    shown = Text.intercalate ", "

-- | A declared datum, in the form two equal records compare equal in.
normalizeDatum :: RecordType -> Text -> Text
normalizeDatum A = Text.strip
normalizeDatum AAAA = Text.toLower . Text.strip
normalizeDatum CNAME = fqdn
normalizeDatum TXT = id

-- | The same form, from what Cloud DNS reports (a TXT comes back quoted).
normalizeRdata :: RecordType -> Text -> Text
normalizeRdata TXT = txtContent
normalizeRdata t = normalizeDatum t

-- | A declared datum as it is handed to gcloud.
datumRdata :: RecordType -> Text -> Text
datumRdata TXT = txtRdata
datumRdata t = normalizeDatum t

{- | A TXT record's text as record data: double-quoted, with @\\@ and @"@
escaped, and split into strings of at most 255 characters (the limit is in
bytes on the wire, so this is exact for ASCII only). Unquoted, gcloud would
split the text into one string per space.
-}
txtRdata :: Text -> Text
txtRdata t = Text.unwords (map quote (chunks t))
  where
    chunks x
        | Text.length x <= 255 = [x]
        | otherwise = Text.take 255 x : chunks (Text.drop 255 x)
    quote x = "\"" <> Text.concatMap escape x <> "\""
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = Text.singleton c

{- | The text a TXT record's data stands for: its quoted strings unescaped
and concatenated, which is what a resolver's client reads. Inverse of
'txtRdata'; data that is not quoted at all is taken as it is.
-}
txtContent :: Text -> Text
txtContent = Text.pack . outside . Text.unpack . Text.strip
  where
    outside [] = []
    outside ('"' : rest) = inside rest
    outside (c : rest)
        | c == ' ' && startsQuoted rest = outside rest
        | otherwise = c : outside rest
    inside [] = []
    inside ('\\' : c : rest) = c : inside rest
    inside ('"' : rest) = outside rest
    inside (c : rest) = c : inside rest
    startsQuoted rest = case dropWhile (== ' ') rest of
        ('"' : _) -> True
        _ -> False

{- | The value of @--rrdatas@, which gcloud reads as a list. Its separator is
a comma unless the value starts with @^DELIM^@ (see @gcloud topic escaping@),
so a datum holding a comma -- an SPF or DKIM TXT, typically -- moves the
whole list to the first separator none of the data contains.
-}
renderRrdatas :: [Text] -> Text
renderRrdatas rdatas
    | sep == "," = Text.intercalate sep rdatas
    | otherwise = "^" <> sep <> "^" <> Text.intercalate sep rdatas
  where
    free d = not (any (d `Text.isInfixOf`) rdatas)
    singles = [",", ";", "|", "#", "~", "%", "@", "!"]
    -- total: only finitely many runs of "|" can occur in the data
    sep = case filter free singles of
        d : _ -> d
        [] -> until free (<> "|") "||"

-------------------------------------------------------------------------------

data CloudDnsCommand
    = ZonesCreate ManagedZone
    | ZonesDescribe ManagedZone
    | ZonesDelete ManagedZone
    | RecordSetsCreate RecordSet
    | RecordSetsUpdate RecordSet
    | RecordSetsDescribe RecordSet
    | RecordSetsDelete RecordSet
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
    RecordSetsCreate rs -> gcloudProc (writeRecord "create" rs)
    RecordSetsUpdate rs -> gcloudProc (writeRecord "update" rs)
    RecordSetsDescribe rs -> gcloudProc (addressRecord "describe" rs <> ["--format", "json"])
    RecordSetsDelete rs -> gcloudProc (addressRecord "delete" rs)
  where
    writeRecord :: String -> RecordSet -> [String]
    writeRecord verb rs =
        addressRecord verb rs
            <> [ "--ttl"
               , show rs.recordTtl
               , -- one argument with "=": a TXT's data starts with a quote and
                 -- may hold anything, and must not be read as a flag
                 "--rrdatas=" <> Text.unpack (renderRrdatas (map (datumRdata rs.recordType) rs.recordData))
               ]
    addressRecord :: String -> RecordSet -> [String]
    addressRecord verb rs =
        withProject rs.recordZone.zoneProject
            [ "dns"
            , "record-sets"
            , verb
            , Text.unpack (fqdn rs.recordName)
            , "--zone"
            , Text.unpack rs.recordZone.zoneName
            , "--type"
            , Text.unpack (renderRecordType rs.recordType)
            ]
