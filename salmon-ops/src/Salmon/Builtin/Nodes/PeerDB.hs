{- | PeerDB peers and CDC mirrors, declared through PeerDB's flow API (the
HTTP/JSON gateway of @flow-api@, port 8113 upstream).

What this module covers is what PeerDB holds: a Postgres source peer, a
ClickHouse destination peer, and a CDC mirror between them. PeerDB itself
(its catalog, Temporal, @flow-api@ and the workers, and the S3-compatible
store ClickHouse mirrors stage through) is not run here: the caller gives a
'Track'' 'Api' that stands for it being up.

The flow API is spoken in plain HTTP with no credential: run it where only
the operator reaches it. A @flow-api@ that wants a password, or TLS, is not
covered.

Secrets. A peer's password is a file, read at @up@ only (the check cannot
use it: PeerDB gives every password back as @********@) and sent in the
request body, never in a URL, a report, a failure or a node's @help@ or
@notes@. What PeerDB answers is cut of the password before it is reported or
thrown.

Nothing here drops anything: @down@ of a peer or a mirror is nothing.
Dropping a mirror is PeerDB's @TERMINATED@ state change, which removes its
slot and publication on the source and, unless told not to, its tables on
the destination: an operator's decision, as is a resync.
-}
module Salmon.Builtin.Nodes.PeerDB where

import Control.Exception (Exception, throwIO, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (toList)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import qualified Data.Text.IO as Text
import Network.HTTP.Client (
    HttpException,
    Manager,
    RequestBody (..),
    defaultManagerSettings,
    httpLbs,
    managerResponseTimeout,
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseStatus,
    responseTimeoutMicro,
 )
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem (File, withFile)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

-- | Where the flow API answers, e.g. @http:\/\/127.0.0.1:8113@ (no trailing slash).
newtype Api = Api {apiBaseUrl :: Text}
    deriving (Eq, Ord, Show)

{- | One request: what it was for, the method and path, and the HTTP status
(@0@ when nothing answered). Never a body: a peer's body holds its password.
-}
data Report
    = ApiCall !Text !Text !Int
    deriving (Show)

-------------------------------------------------------------------------------
-- Peers

type PeerName = Text

data PostgresPeer
    = PostgresPeer
    { pgHost :: Text
    , pgPort :: Int
    , pgUser :: Text
    , pgDatabase :: Text
    , pgRequireTls :: Bool
    }
    deriving (Eq, Ord, Show)

data ClickHousePeer
    = ClickHousePeer
    { chHost :: Text
    , chPort :: Int
    -- ^ the native protocol port (9000, or 9440 with TLS)
    , chUser :: Text
    , chDatabase :: Text
    , chDisableTls :: Bool
    }
    deriving (Eq, Ord, Show)

data PeerConfig
    = PostgresSource PostgresPeer
    | ClickHouseTarget ClickHousePeer
    deriving (Eq, Ord, Show)

data Peer = Peer {peerName :: PeerName, peerConfig :: PeerConfig}
    deriving (Eq, Ord, Show)

{- | A peer, created or updated (@allow_update@), with the password read from
this file at @up@, verbatim (a trailing newline is part of it, as for the
user nodes of "Salmon.Builtin.Nodes.Postgres" and
"Salmon.Builtin.Nodes.ClickHouse", which can read the same file).

PeerDB validates a peer before it keeps it: it connects with it, and for a
ClickHouse peer checks the user may read S3 (see
"Salmon.Builtin.Nodes.ClickHouse".'Salmon.Builtin.Nodes.ClickHouse.sourceReadGrant'),
so @up@ fails while the database it names does not let it in.

The check reads the peer back and compares what PeerDB shows (the type,
host, port, user, database and TLS switch). It cannot compare the password,
which PeerDB shows as @********@: a rotated file is sent by the next @up@,
which a passing check skips, so a rotation needs the node forced. @down@ is
nothing.
-}
peer :: Reporter Report -> Track' Api -> Api -> File "passfile" -> Peer -> Op
peer r apiT api passfile p =
    withFile passfile $ \path ->
        op "peerdb-peer" (deps [run apiT api]) $ \actions ->
            actions
                { ref = mkRef "peerdb-peer" (api.apiBaseUrl, p.peerName)
                , help = Text.unwords ["PeerDB", peerTypeName p.peerConfig, "peer", p.peerName, "with the password from file at", Text.pack path]
                , notes = ["flow api: " <> api.apiBaseUrl, peerSummary p.peerConfig]
                , check = checkPeer api p
                , up = do
                    pwd <- Text.readFile path
                    createPeer r api p (Password pwd)
                }

newtype Password = Password {revealPassword :: Text}

instance Show Password where
    show _ = "<password>"

peerTypeName :: PeerConfig -> Text
peerTypeName (PostgresSource _) = "POSTGRES"
peerTypeName (ClickHouseTarget _) = "CLICKHOUSE"

-- | The non-secret part of a peer, as its notes show it.
peerSummary :: PeerConfig -> Text
peerSummary (PostgresSource c) =
    "postgres " <> c.pgUser <> "@" <> c.pgHost <> ":" <> showT c.pgPort <> "/" <> c.pgDatabase <> (if c.pgRequireTls then " tls required" else "")
peerSummary (ClickHouseTarget c) =
    "clickhouse " <> c.chUser <> "@" <> c.chHost <> ":" <> showT c.chPort <> "/" <> c.chDatabase <> (if c.chDisableTls then " no tls" else " tls")

{- | The fields of a peer's configuration, as the flow API names them in a
request. The password is the one secret field.
-}
peerConfigFields :: PeerConfig -> Password -> (Text, Value)
peerConfigFields (PostgresSource c) pwd =
    ( "postgres_config"
    , object
        [ "host" .= c.pgHost
        , "port" .= c.pgPort
        , "user" .= c.pgUser
        , "password" .= pwd.revealPassword
        , "database" .= c.pgDatabase
        , "require_tls" .= c.pgRequireTls
        ]
    )
peerConfigFields (ClickHouseTarget c) pwd =
    ( "clickhouse_config"
    , object
        [ "host" .= c.chHost
        , "port" .= c.chPort
        , "user" .= c.chUser
        , "password" .= pwd.revealPassword
        , "database" .= c.chDatabase
        , "disable_tls" .= c.chDisableTls
        ]
    )

-- | The body of @POST \/v1\/peers\/create@. Holds the password.
createPeerBody :: Peer -> Password -> Value
createPeerBody p pwd =
    object
        [ "peer"
            .= object
                [ "name" .= p.peerName
                , "type" .= peerTypeName p.peerConfig
                , Key.fromText field .= cfg
                ]
        , "allow_update" .= True
        ]
  where
    (field, cfg) = peerConfigFields p.peerConfig pwd

createPeer :: Reporter Report -> Api -> Peer -> Password -> IO ()
createPeer r api p pwd = do
    (code, body) <- call r ("peer " <> p.peerName) api "POST" "/v1/peers/create" (Just (createPeerBody p pwd))
    let answer = redact [pwd.revealPassword] (decode body)
    case (code, jsonField "status" =<< Aeson.decodeStrict body) of
        (200, Just (String "CREATED")) -> pure ()
        _ -> throwIO (PeerDBRefused ("peer " <> p.peerName <> ": " <> statusText code <> errorMessage answer))

{- | @GET \/v1\/peers\/info\/NAME@: 404 is a 'Failure' (no such peer),
another answer is compared by 'interpretPeerInfo', a flow API that does not
answer is 'Unknown'.
-}
checkPeer :: Api -> Peer -> IO CheckResult
checkPeer api p = do
    res <- request api "GET" ("/v1/peers/info/" <> pathSegment p.peerName) Nothing
    pure $ case res of
        Left _ -> Unknown
        Right (404, _) -> Failure ("PeerDB has no peer " <> p.peerName)
        Right (200, body) -> maybe Unknown (interpretPeerInfo p) (Aeson.decodeStrict body)
        Right _ -> Unknown

{- | What @\/v1\/peers\/info@ answers, read against the declaration. The
answer's fields are in lowerCamelCase; the password is not looked at (it is
@********@).
-}
interpretPeerInfo :: Peer -> Value -> CheckResult
interpretPeerInfo p v =
    case jsonField "peer" v of
        Nothing -> Unknown
        Just held -> case differences held of
            [] -> Success
            ds -> Failure ("PeerDB peer " <> p.peerName <> " differs: " <> Text.intercalate ", " ds)
  where
    differences held =
        [ "type is " <> renderValue t
        | Just t <- [jsonField "type" held]
        , t /= String (peerTypeName p.peerConfig)
        ]
            <> case p.peerConfig of
                PostgresSource c ->
                    fieldDiffs
                        (jsonField "postgresConfig" held)
                        [ ("host", String c.pgHost)
                        , ("port", Number (fromIntegral c.pgPort))
                        , ("user", String c.pgUser)
                        , ("database", String c.pgDatabase)
                        , ("requireTls", Bool c.pgRequireTls)
                        ]
                ClickHouseTarget c ->
                    fieldDiffs
                        (jsonField "clickhouseConfig" held)
                        [ ("host", String c.chHost)
                        , ("port", Number (fromIntegral c.chPort))
                        , ("user", String c.chUser)
                        , ("database", String c.chDatabase)
                        , ("disableTls", Bool c.chDisableTls)
                        ]
    fieldDiffs Nothing _ = ["not a " <> peerTypeName p.peerConfig <> " peer"]
    fieldDiffs (Just cfg) expected =
        [ k <> " is " <> maybe "unset" renderValue held <> ", declared " <> renderValue want
        | (k, want) <- expected
        , let held = jsonField k cfg
        , not (sameField want held)
        ]
    -- protobuf JSON leaves a false or empty field out
    sameField (Bool False) Nothing = True
    sameField (String "") Nothing = True
    sameField want held = Just want == held

-------------------------------------------------------------------------------
-- Mirrors

type MirrorName = Text

-- | A source table and the destination table it is mirrored to.
data TableMapping = TableMapping {sourceTable :: Text, destinationTable :: Text}
    deriving (Eq, Ord, Show)

{- | A CDC mirror from a Postgres peer to another peer.

'mirrorPublication' and 'mirrorSlot' name ones declared on the source
("Salmon.Builtin.Nodes.Postgres.Logical"); 'Nothing' lets PeerDB make its
own (it needs to own the tables for a publication, which a CDC role
usually does not).

A declared slot goes with no initial snapshot: PeerDB refuses an existing
slot for a snapshot (@irrecoverable_existing_slot@, the mirror goes to
@STATUS_FAILED@; seen at @stable-v0.37.12@), since the snapshot it copies
is the one it exports when it creates the slot itself. Such a mirror
streams what the slot holds from its creation on, and the rows already in
the tables are not copied. 'mirrorProblems' refuses the combination before
anything is sent.
-}
data Mirror
    = Mirror
    { mirrorName :: MirrorName
    , mirrorSource :: PeerName
    , mirrorDestination :: PeerName
    , mirrorTables :: [TableMapping]
    , mirrorPublication :: Maybe Text
    , mirrorSlot :: Maybe Text
    , mirrorInitialSnapshot :: Bool
    , mirrorMaxBatchSize :: Maybe Int
    , mirrorIdleTimeoutSeconds :: Maybe Int
    }
    deriving (Eq, Ord, Show)

-- | An initial snapshot, PeerDB's own publication and slot, its default batch size and timeout.
mirror :: MirrorName -> PeerName -> PeerName -> [TableMapping] -> Mirror
mirror name src dst tables =
    Mirror
        { mirrorName = name
        , mirrorSource = src
        , mirrorDestination = dst
        , mirrorTables = tables
        , mirrorPublication = Nothing
        , mirrorSlot = Nothing
        , mirrorInitialSnapshot = True
        , mirrorMaxBatchSize = Nothing
        , mirrorIdleTimeoutSeconds = Nothing
        }

{- | A CDC mirror, created when PeerDB has none of that name. It needs its
two peers: @inject@ them (or pass them through the 'Track'' 'Api').

@up@ asks for the mirror's status first, and:

* none: creates it (@POST \/v1\/flows\/cdc\/create@);
* one with the declared peers and tables, not failed: does nothing;
* one with other peers or tables, or one PeerDB reports failed or
  terminated: refuses (throws), and changes nothing. Changing a running
  mirror's tables, resyncing it, or dropping and recreating it are an
  operator's decisions.

The check is 'interpretMirrorStatus'. @down@ is nothing.
-}
mirrorOp :: Reporter Report -> Track' Api -> Api -> Mirror -> Op
mirrorOp r apiT api m =
    op "peerdb-mirror" (deps [run apiT api]) $ \actions ->
        actions
            { ref = mkRef "peerdb-mirror" (api.apiBaseUrl, m.mirrorName)
            , help = Text.unwords ["PeerDB CDC mirror", m.mirrorName, "from", m.mirrorSource, "to", m.mirrorDestination]
            , notes =
                ["flow api: " <> api.apiBaseUrl]
                    <> [t.sourceTable <> " -> " <> t.destinationTable | t <- sort m.mirrorTables]
                    <> ["publication " <> p | Just p <- [m.mirrorPublication]]
                    <> ["slot " <> s | Just s <- [m.mirrorSlot]]
                    <> ["no initial snapshot" | not m.mirrorInitialSnapshot]
            , check = checkMirror api m
            , up = ensureMirror r api m
            }

-- | What makes a declaration one PeerDB would accept and then fail, or refuse.
mirrorProblems :: Mirror -> [Text]
mirrorProblems m =
    ["no table is declared" | null m.mirrorTables]
        <> [ "slot " <> s <> " is declared with an initial snapshot, which PeerDB refuses on an existing slot: declare no snapshot, or no slot"
           | Just s <- [m.mirrorSlot]
           , m.mirrorInitialSnapshot
           ]

-- | The body of @POST \/v1\/flows\/cdc\/create@.
createMirrorBody :: Mirror -> Value
createMirrorBody m =
    object
        [ "connection_configs"
            .= object
                ( [ "flow_job_name" .= m.mirrorName
                  , "source_name" .= m.mirrorSource
                  , "destination_name" .= m.mirrorDestination
                  , "table_mappings"
                        .= [ object ["source_table_identifier" .= t.sourceTable, "destination_table_identifier" .= t.destinationTable]
                           | t <- m.mirrorTables
                           ]
                  , "do_initial_snapshot" .= m.mirrorInitialSnapshot
                  ]
                    <> ["publication_name" .= p | Just p <- [m.mirrorPublication]]
                    <> ["replication_slot_name" .= s | Just s <- [m.mirrorSlot]]
                    <> ["max_batch_size" .= n | Just n <- [m.mirrorMaxBatchSize]]
                    <> ["idle_timeout_seconds" .= n | Just n <- [m.mirrorIdleTimeoutSeconds]]
                )
        ]

-- | The body of @POST \/v1\/mirrors\/status@.
mirrorStatusBody :: MirrorName -> Value
mirrorStatusBody name = object ["flow_job_name" .= name, "include_flow_info" .= True, "exclude_batches" .= True]

-- | What PeerDB says about a mirror.
data MirrorState
    = NoSuchMirror
    | MirrorFound Value
    | MirrorUnreadable Text
    deriving (Show)

mirrorState :: Maybe (Reporter Report) -> Api -> MirrorName -> IO MirrorState
mirrorState r api name = do
    res <- case r of
        Just rep -> Right <$> call rep ("mirror " <> name) api "POST" "/v1/mirrors/status" (Just (mirrorStatusBody name))
        Nothing -> request api "POST" "/v1/mirrors/status" (Just (mirrorStatusBody name))
    pure $ case res of
        Left e -> MirrorUnreadable e
        Right (404, _) -> NoSuchMirror
        Right (200, body) -> maybe (MirrorUnreadable "an answer that is not JSON") MirrorFound (Aeson.decodeStrict body)
        Right (code, body) -> MirrorUnreadable (statusText code <> errorMessage (decode body))

checkMirror :: Api -> Mirror -> IO CheckResult
checkMirror api m = do
    st <- mirrorState Nothing api m.mirrorName
    pure $ case st of
        NoSuchMirror -> Failure ("PeerDB has no mirror " <> m.mirrorName)
        MirrorFound v -> interpretMirrorStatus m v
        MirrorUnreadable _ -> Unknown

ensureMirror :: Reporter Report -> Api -> Mirror -> IO ()
ensureMirror r api m = do
    st <- mirrorState (Just r) api m.mirrorName
    case st of
        MirrorUnreadable why -> throwIO (PeerDBRefused ("mirror " <> m.mirrorName <> ": cannot read its status: " <> why))
        MirrorFound v -> case interpretMirrorStatus m v of
            Failure why -> throwIO (PeerDBRefused (why <> "; salmon does not change, resync or recreate a mirror"))
            _ -> pure ()
        NoSuchMirror -> do
            case mirrorProblems m of
                [] -> pure ()
                problems -> throwIO (PeerDBRefused ("mirror " <> m.mirrorName <> ": " <> Text.intercalate "; " problems))
            (code, body) <- call r ("mirror " <> m.mirrorName) api "POST" "/v1/flows/cdc/create" (Just (createMirrorBody m))
            case code of
                200 -> pure ()
                _ -> throwIO (PeerDBRefused ("mirror " <> m.mirrorName <> ": " <> statusText code <> errorMessage (decode body)))

{- | A mirror's status read against its declaration:

* other peers, or another set of table mappings: 'Failure' naming them;
* @STATUS_FAILED@, @STATUS_TERMINATING@ or @STATUS_TERMINATED@: 'Failure';
* @STATUS_RUNNING@, @STATUS_PAUSED@ (an operator paused it: salmon does not
  resume it) or @STATUS_COMPLETED@: 'Success';
* anything else (setting up, snapshotting, pausing, modifying, resyncing,
  unknown): 'Unknown', it is on its way somewhere.
-}
interpretMirrorStatus :: Mirror -> Value -> CheckResult
interpretMirrorStatus m v =
    case (configDiffs, state) of
        (d : ds, _) -> Failure ("PeerDB mirror " <> m.mirrorName <> " differs: " <> Text.intercalate "; " (d : ds))
        ([], Just s)
            | s `elem` ["STATUS_FAILED", "STATUS_TERMINATING", "STATUS_TERMINATED"] -> Failure ("PeerDB mirror " <> m.mirrorName <> " is " <> s)
            | s `elem` ["STATUS_RUNNING", "STATUS_PAUSED", "STATUS_COMPLETED"] -> Success
        _ -> Unknown
  where
    state = case jsonField "currentFlowState" v of
        Just (String s) -> Just s
        _ -> Nothing
    config = jsonField "cdcStatus" v >>= jsonField "config"
    configDiffs = case config of
        Nothing -> ["not a CDC mirror"]
        Just c ->
            [ "source is " <> renderMaybe (jsonField "sourceName" c) | jsonField "sourceName" c /= Just (String m.mirrorSource)
            ]
                <> ["destination is " <> renderMaybe (jsonField "destinationName" c) | jsonField "destinationName" c /= Just (String m.mirrorDestination)]
                <> ["tables are " <> renderMappings held | let held = heldMappings c, held /= sort m.mirrorTables]
    heldMappings c = case jsonField "tableMappings" c of
        Just (Array xs) ->
            sort
                [ TableMapping s d
                | x <- toList xs
                , Just (String s) <- [jsonField "sourceTableIdentifier" x]
                , Just (String d) <- [jsonField "destinationTableIdentifier" x]
                ]
        _ -> []
    renderMappings :: [TableMapping] -> Text
    renderMappings [] = "none"
    renderMappings ts = Text.intercalate ", " [t.sourceTable <> " -> " <> t.destinationTable | t <- ts]
    renderMaybe = maybe "unset" renderValue

-------------------------------------------------------------------------------
-- HTTP

data PeerDBRefused = PeerDBRefused Text
    deriving (Show)

instance Exception PeerDBRefused

newManager' :: IO Manager
newManager' = newManager defaultManagerSettings{managerResponseTimeout = responseTimeoutMicro 60000000}

{- | One call to the flow API, reported (the method, path and status, never
the body). Throws a 'PeerDBRefused' when nothing answered.
-}
call :: Reporter Report -> Text -> Api -> Text -> Text -> Maybe Value -> IO (Int, ByteString.ByteString)
call r what api verb path body = do
    res <- request api verb path body
    case res of
        Left why -> do
            runReporter r (ApiCall what (verb <> " " <> path) 0)
            throwIO (PeerDBRefused (what <> ": the flow API at " <> api.apiBaseUrl <> " does not answer: " <> why))
        Right (code, out) -> do
            runReporter r (ApiCall what (verb <> " " <> path) code)
            pure (code, out)

-- | 'Left' when nothing answered; otherwise the status and the body.
request :: Api -> Text -> Text -> Maybe Value -> IO (Either Text (Int, ByteString.ByteString))
request api verb path body = do
    res <- try $ do
        mgr <- newManager'
        req0 <- parseRequest (Text.unpack (api.apiBaseUrl <> path))
        let req =
                req0
                    { method = Text.encodeUtf8 verb
                    , requestHeaders = [("Content-Type", "application/json") | Just _ <- [body]]
                    , requestBody = maybe (requestBody req0) (RequestBodyLBS . Aeson.encode) body
                    }
        rsp <- httpLbs req mgr
        pure (statusCode (responseStatus rsp), LByteString.toStrict (responseBody rsp))
    pure $ case res of
        -- the exception names the request (method, host, path), not its body
        Left (e :: HttpException) -> Left (Text.take 200 (httpExceptionText e))
        Right ok -> Right ok

httpExceptionText :: HttpException -> Text
httpExceptionText (Http.HttpExceptionRequest _ content) = Text.pack (show content)
httpExceptionText e = Text.pack (show e)

-- | A name as one URL path segment.
pathSegment :: Text -> Text
pathSegment = Text.decodeUtf8 . urlEncode False . Text.encodeUtf8

-- | The @message@ of a gRPC-gateway error, or the body as it is.
errorMessage :: Text -> Text
errorMessage body = case Aeson.decodeStrict (Text.encodeUtf8 body) >>= jsonField "message" of
    Just (String msg) -> ": " <> Text.take 400 msg
    _ -> if Text.null (Text.strip body) then "" else ": " <> Text.take 400 body

statusText :: Int -> Text
statusText code = "the flow API answered " <> showT code

redact :: [Text] -> Text -> Text
redact secrets t = foldr (\s acc -> if Text.null s then acc else Text.replace s "<redacted>" acc) t secrets

jsonField :: Text -> Value -> Maybe Value
jsonField k (Object o) = KeyMap.lookup (Key.fromText k) o
jsonField _ _ = Nothing

renderValue :: Value -> Text
renderValue (String s) = s
renderValue (Bool b) = if b then "true" else "false"
renderValue other = decode (LByteString.toStrict (Aeson.encode other))

showT :: (Show a) => a -> Text
showT = Text.pack . show

decode :: ByteString.ByteString -> Text
decode = Text.decodeUtf8With TextError.lenientDecode
