{- | A ClickHouse server and what lives in it: the server as a podman quadlet,
its configuration drop-ins, and databases, users and grants made with SQL.

Everything that talks to a server goes through @clickhouse-client@, either
the host's ('LocalClient') or the container's own through @podman exec@
('InContainer'), as an administrator that needs no password: the
@default@ user, which 'renderAdminAccess' gives SQL access management and
confines to the loopback addresses. A server administered over the network,
or with an administrator password, is not covered.

Nothing here reads a secret at graph build. A user's password is a file
('userPassFile') read at @up@ and @check@ time; what @up@ sends is a salted
SHA-256 of it, on @clickhouse-client@'s standard input (ClickHouse masks it in
@system.query_log@), and what @check@ does is log in as the user with the
password in @CLICKHOUSE_PASSWORD@, in the client's environment, never its
argv. Neither the password nor the hash is in a report, a failure or a node's
@help@ or @notes@.

Nothing here drops anything: @down@ leaves databases, users and grants in
place (the server's own @down@ stops the container and leaves its data
directory).
-}
module Salmon.Builtin.Nodes.ClickHouse where

import Control.Exception (Exception, IOException, throwIO, try)
import qualified Crypto.Hash as Hash
import qualified Crypto.Random as Random
import qualified Data.ByteString as ByteString
import Data.Char (isAlphaNum, isAscii, isDigit, toUpper)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import qualified Data.Text.IO as Text
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), justInstall, withBinaryStdin)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem (File, withFile)
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.Podman.Quadlet as Quadlet
import qualified Salmon.Builtin.Nodes.Readiness as Readiness
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = CHCreateDatabase !DatabaseName !Binary.Report
    | CHSetUser !UserName !Binary.Report
    | CHGrant !Grant !Binary.Report
    | CHSourceGrant !Text !UserName !Binary.Report
    deriving (Show)

type DatabaseName = Text
type UserName = Text

-------------------------------------------------------------------------------
-- Configuration drop-ins

{- | The server settings salmon renders into one @config.d@ drop-in. ClickHouse
merges every file of @config.d@ over its own @config.xml@, so a setting left
out here is the packaged default.
-}
data ServerConfig
    = ServerConfig
    { serverListenHosts :: [Text]
    -- ^ @listen_host@, one element each; empty renders none (the packaged
    -- default, which in the official image is every address)
    , serverHttpPort :: Maybe Int
    , serverTcpPort :: Maybe Int
    , serverSettings :: [(Text, Text)]
    -- ^ other top-level scalar settings, as @(element, value)@
    -- (@("max_connections", "2048")@); the value is escaped, the element must
    -- be a plain XML name ('configProblems')
    }
    deriving (Eq, Ord, Show)

-- | Renders nothing but the root element: every setting is the packaged default.
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig [] Nothing Nothing []

-- | Native protocol port, the one @clickhouse-client@ speaks.
defaultTcpPort :: Int
defaultTcpPort = 9000

-- | HTTP interface port.
defaultHttpPort :: Int
defaultHttpPort = 8123

renderServerConfig :: ServerConfig -> Text
renderServerConfig cfg =
    Text.unlines $
        mconcat
            [ ["<clickhouse>"]
            , [element "listen_host" h | h <- cfg.serverListenHosts]
            , [element "http_port" (showT p) | Just p <- [cfg.serverHttpPort]]
            , [element "tcp_port" (showT p) | Just p <- [cfg.serverTcpPort]]
            , [element k v | (k, v) <- cfg.serverSettings]
            , ["</clickhouse>"]
            ]
  where
    element k v = "    <" <> k <> ">" <> xmlEscape v <> "</" <> k <> ">"

{- | What makes a 'ServerConfig' unrenderable: a setting whose element is not a
plain XML name, which would otherwise be written as markup.
-}
configProblems :: ServerConfig -> [Text]
configProblems cfg =
    ["setting " <> k <> " is not a plain XML element name" | (k, _) <- cfg.serverSettings, not (plainName k)]
  where
    plainName k = case Text.uncons k of
        Just (c, rest) -> (isAscii c && (c == '_' || isAlphaNum c) && not (isDigit c)) && Text.all (\x -> isAscii x && (x == '_' || isAlphaNum x)) rest
        Nothing -> False

{- | The @users.d@ drop-in that makes the @default@ user the administrator
the SQL nodes use: @access_management@ (so it may @CREATE USER@ and @GRANT@),
and reachable from the loopback addresses only. The packaged @users.xml@
lets @default@ in from anywhere with an empty password, so a server whose
port is published without this file is open to whoever reaches it.
-}
renderAdminAccess :: Text
renderAdminAccess =
    Text.unlines
        [ "<clickhouse>"
        , "    <users>"
        , "        <default>"
        , "            <access_management>1</access_management>"
        , "            <networks replace=\"replace\">"
        , "                <ip>::1</ip>"
        , "                <ip>127.0.0.1</ip>"
        , "            </networks>"
        , "        </default>"
        , "    </users>"
        , "</clickhouse>"
        ]

xmlEscape :: Text -> Text
xmlEscape = Text.concatMap esc
  where
    esc '&' = "&amp;"
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '"' = "&quot;"
    esc '\'' = "&apos;"
    esc c = Text.singleton c

data InvalidConfig = InvalidConfig !FilePath ![Text]
    deriving (Show)

instance Exception InvalidConfig

{- | One drop-in file, written in a directory shared with files salmon does
not declare (@\/etc\/clickhouse-server\/config.d@ on a packaged install).
A 'ServerConfig' with 'configProblems' fails at @up@ and writes nothing.
-}
serverConfigFile :: FilePath -> ServerConfig -> Op
serverConfigFile path cfg =
    case configProblems cfg of
        [] -> FS.filecontentsInSharedDir (FS.FileContents path (renderServerConfig cfg))
        problems ->
            op "clickhouse-config" nodeps $ \actions ->
                actions
                    { help = "refuses to write " <> Text.pack path
                    , notes = problems
                    , ref = mkRef "file-contents" path
                    , up = throwIO (InvalidConfig path problems)
                    }

-- | 'renderAdminAccess' at this path (a @users.d@ drop-in).
adminAccessFile :: FilePath -> Op
adminAccessFile path = FS.filecontentsInSharedDir (FS.FileContents path renderAdminAccess)

-------------------------------------------------------------------------------
-- The server as a quadlet

-- | A ClickHouse server run from the official image as a system-scope quadlet.
data Server
    = Server
    { serverName :: Podman.ContainerName
    , serverImage :: Text
    -- ^ the full reference, e.g. @docker.io\/clickhouse\/clickhouse-server:25.8@
    , serverConfigDir :: FilePath
    -- ^ a host directory salmon owns, holding the two drop-ins, which are
    -- bind-mounted read-only into the container's @config.d@ and @users.d@
    , serverDataDir :: FilePath
    -- ^ the host directory mounted at @\/var\/lib\/clickhouse@, created if
    -- missing; the image's entrypoint gives it to the @clickhouse@ user at
    -- start. Never removed.
    , serverConfig :: ServerConfig
    , serverPorts :: [Podman.PortMapping]
    -- ^ published ports; empty publishes none (the SQL nodes reach the server
    -- through @podman exec@ either way)
    , serverReadyTimeout :: Int
    -- ^ seconds @up@ waits for the server to answer a query after a start
    , serverDropIns :: [DropIn]
    -- ^ further @config.d@ drop-ins (a Keeper member, a cluster: see
    -- "Salmon.Builtin.Nodes.ClickHouse.Cluster"), written in
    -- 'serverConfigDir' next to the two above, bound read-only and watched.
    -- Empty adds no file, bind or watch: the quadlet is the one it was.
    , serverEnvFile :: Maybe FilePath
    -- ^ a pre-provisioned @EnvironmentFile=@ (@KEY=VALUE@ lines) podman
    -- reads on the host at every start, and watched; where a drop-in's
    -- @from_env@ settings (a cluster secret, interserver credentials) find
    -- their values, so that no secret is in a file salmon renders.
    -- 'Nothing' renders nothing.
    , serverNetwork :: Maybe Text
    -- ^ the podman network (@Network=@), for members that reach each other
    -- by container name; 'Nothing' renders nothing
    , serverWaitReady :: Bool
    -- ^ whether @up@ waits for the server to answer (@True@, the default).
    -- A server embedding a member of a Keeper ensemble of several answers
    -- nothing before the ensemble has a quorum (seen on 25.8: "waiting for
    -- quorum"), so a graph that starts the members one after another would
    -- wait on the first for members started after it: such a member sets it
    -- @False@ and the graph waits once, after all of them
    -- ("Salmon.Builtin.Nodes.ClickHouse.Cluster".@keeperQuorum@). Never
    -- rendered.
    }
    deriving (Eq, Show)

{- | One more @config.d@ file: its name in 'serverConfigDir' and in the
container's @config.d@, its rendered contents, and what makes it
unrenderable (when not empty, @up@ refuses to write it, as
'serverConfigFile' does). The contents are public (they are what the node
shows to tell a change): nothing secret goes in one, a secret is a
@from_env@ reference to 'serverEnvFile'.
-}
data DropIn = DropIn
    { dropInName :: FilePath
    , dropInContents :: Text
    , dropInProblems :: [Text]
    }
    deriving (Eq, Show)

dropInPath :: Server -> DropIn -> FilePath
dropInPath s d = s.serverConfigDir </> d.dropInName

-- | What makes a drop-in's name unusable: not a plain @.xml@ file name, or one of the two salmon always writes.
dropInNameProblems :: DropIn -> [Text]
dropInNameProblems d =
    [ "drop-in " <> name <> " is not a plain .xml file name"
    | Text.null base || not (Text.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("-_." :: String))) name) || not (".xml" `Text.isSuffixOf` name) || "." `Text.isPrefixOf` name
    ]
        <> ["drop-in " <> name <> " is a file salmon already writes" | d.dropInName `elem` ["salmon-server.xml", "salmon-admin.xml"]]
  where
    name = Text.pack d.dropInName
    base = maybe "" id (Text.stripSuffix ".xml" name)

{- | A drop-in file, refused at @up@ (nothing written) when it has problems,
as 'serverConfigFile' is.
-}
dropInFile :: FilePath -> DropIn -> Op
dropInFile path d =
    case dropInNameProblems d <> d.dropInProblems of
        [] -> FS.filecontentsInSharedDir (FS.FileContents path d.dropInContents)
        problems ->
            op "clickhouse-config" nodeps $ \actions ->
                actions
                    { help = "refuses to write " <> Text.pack path
                    , notes = problems
                    , ref = mkRef "file-contents" path
                    , up = throwIO (InvalidConfig path problems)
                    }

-- | No port published, the default configuration, 60 seconds to answer.
server :: Podman.ContainerName -> Text -> FilePath -> FilePath -> Server
server name image configDir dataDir =
    Server
        { serverName = name
        , serverImage = image
        , serverConfigDir = configDir
        , serverDataDir = dataDir
        , serverConfig = defaultServerConfig
        , serverPorts = []
        , serverReadyTimeout = 60
        , serverDropIns = []
        , serverEnvFile = Nothing
        , serverNetwork = Nothing
        , serverWaitReady = True
        }

serverConfigPath :: Server -> FilePath
serverConfigPath s = s.serverConfigDir </> "salmon-server.xml"

adminAccessPath :: Server -> FilePath
adminAccessPath s = s.serverConfigDir </> "salmon-admin.xml"

-- | The client the SQL nodes use for this server: its own, in the container.
serverClient :: Server -> Client
serverClient s = Client (InContainer s.serverName) (maybe defaultTcpPort id s.serverConfig.serverTcpPort)

{- | The quadlet: the image, the data directory, the two drop-ins (watched, so
a changed one restarts the container), and a readiness probe asking the
server for @SELECT 1@ through its own client.
-}
serverContainer :: Server -> Quadlet.Container
serverContainer s =
    (Quadlet.container s.serverName s.serverImage)
        { Quadlet.containerDescription = "ClickHouse " <> Podman.getContainerName s.serverName <> " (salmon)"
        , Quadlet.containerPorts = s.serverPorts
        , Quadlet.containerBinds =
            [ Quadlet.bind s.serverDataDir "/var/lib/clickhouse"
            , readOnlyFile (serverConfigPath s) "/etc/clickhouse-server/config.d/salmon-server.xml"
            , readOnlyFile (adminAccessPath s) "/etc/clickhouse-server/users.d/salmon-admin.xml"
            ]
                <> [readOnlyFile (dropInPath s d) ("/etc/clickhouse-server/config.d" </> d.dropInName) | d <- s.serverDropIns]
        , Quadlet.containerWatched = [serverConfigPath s, adminAccessPath s] <> fmap (dropInPath s) s.serverDropIns
        , Quadlet.containerEnvFile = s.serverEnvFile
        , Quadlet.containerNetwork = s.serverNetwork
        , Quadlet.containerReady = if s.serverWaitReady then Just (Quadlet.readyWhen (readyProbe (serverClient s)) s.serverReadyTimeout) else Nothing
        }
  where
    readOnlyFile host guest =
        (Quadlet.bind host guest){Quadlet.bindMode = Podman.ReadOnly, Quadlet.bindCreate = Nothing}

-- | @SELECT 1@ through this client exits 0.
readyProbe :: Client -> Readiness.Probe
readyProbe c = case clientArgv c ["--query", "SELECT 1"] of
    (exe : args) -> Readiness.ProbeCommand exe (fmap Text.pack args)
    [] -> Readiness.ProbeCommand "false" []

{- | The server: its drop-ins written, then the quadlet ('Quadlet.quadletContainer'),
whose @up@ returns once the server answers. Use it as the @Track'@ of the
SQL nodes' client: @Track (const (serverOp r systemctl s))@.
-}
serverOp :: Reporter Systemd.Report -> Track' (Binary "systemctl") -> Server -> Op
serverOp r systemctl s =
    Quadlet.quadletContainer r systemctl (Track (const files)) (serverContainer s)
  where
    files =
        op "clickhouse-config-files" (deps ([serverConfigFile (serverConfigPath s) s.serverConfig, adminAccessFile (adminAccessPath s)] <> [dropInFile (dropInPath s d) d | d <- s.serverDropIns])) $ \actions ->
            actions{help = "ClickHouse drop-ins in " <> Text.pack s.serverConfigDir}

-------------------------------------------------------------------------------
-- The client

-- | Where @clickhouse-client@ runs.
data Via
    = -- | the host's, against the loopback address
      LocalClient
    | -- | the container's own, through @podman exec@
      InContainer Podman.ContainerName
    deriving (Eq, Ord, Show)

data Client = Client {clientVia :: Via, clientPort :: Int}
    deriving (Eq, Ord, Show)

-- | The host's client against @localhost:9000@.
localClient :: Client
localClient = Client LocalClient defaultTcpPort

-- | Which server a node is about, for its 'Ref' and its @notes@.
clientKey :: Client -> Text
clientKey c = case c.clientVia of
    LocalClient -> "localhost:" <> showT c.clientPort
    InContainer name -> "container:" <> Podman.getContainerName name <> ":" <> showT c.clientPort

{- | The argv of a client call: @clickhouse-client --port P@ and these
arguments, behind @podman exec -i NAME@ for a container. Statements go on
standard input, not here.
-}
clientArgv :: Client -> [String] -> [String]
clientArgv c args = case c.clientVia of
    LocalClient -> base
    InContainer name -> ["podman", "exec", "-i", Text.unpack (Podman.getContainerName name)] <> base
  where
    base = ["clickhouse-client", "--port", show c.clientPort] <> args

-- | The administrator's batch: every statement read from standard input.
data AdminBatch = AdminBatch
    deriving (Show)

adminRun :: Client -> Command "clickhouse-client" AdminBatch
adminRun c = Command $ \AdminBatch -> argvProc (clientArgv c ["--multiquery"])

argvProc :: [String] -> CreateProcess
argvProc (exe : args) = proc exe args
argvProc [] = proc "false" []

{- | One query as the administrator, its output back; 'Left' with the
client's error when it fails. For the checks: the query names databases and
users, never a secret.
-}
adminQuery :: Client -> Text -> IO (Either Text Text)
adminQuery c sql = do
    r <- try (readCreateProcessWithExitCode (argvProc (clientArgv c [])) (Text.encodeUtf8 sql)) :: IO (Either IOException (ExitCode, ByteString.ByteString, ByteString.ByteString))
    pure $ case r of
        Left e -> Left (Text.pack (show e))
        Right (ExitSuccess, out, _) -> Right (decode out)
        Right (ExitFailure _, _, err) -> Left (Text.strip (decode err))

-------------------------------------------------------------------------------
-- SQL quoting

-- | A backquoted identifier; a backquote or backslash in the name is escaped.
quoteIdent :: Text -> Text
quoteIdent t = "`" <> Text.concatMap esc t <> "`"
  where
    esc '`' = "\\`"
    esc '\\' = "\\\\"
    esc c = Text.singleton c

-- | A single-quoted string literal; a quote or backslash in it is escaped.
quoteLiteral :: Text -> Text
quoteLiteral t = "'" <> Text.concatMap esc t <> "'"
  where
    esc '\'' = "\\'"
    esc '\\' = "\\\\"
    esc c = Text.singleton c

-------------------------------------------------------------------------------
-- Databases

{- | A database, created if missing (engine left to the server, @Atomic@).
@down@ drops nothing.
-}
database :: Reporter Report -> Track' (Binary "clickhouse-client") -> Client -> DatabaseName -> Op
database r clientT c name =
    withBinaryStdin clientT (adminRun c) AdminBatch (Text.encodeUtf8 (createDatabaseSql name)) $ \create ->
        op "clickhouse-database" nodeps $ \actions ->
            actions
                { ref = mkRef "clickhouse-db" (clientKey c, name)
                , help = "create ClickHouse database " <> name
                , notes = ["server: " <> clientKey c]
                , check = checkDatabase c name
                , up = create (contramap (CHCreateDatabase name) r)
                }

createDatabaseSql :: DatabaseName -> Text
createDatabaseSql name = "CREATE DATABASE IF NOT EXISTS " <> quoteIdent name <> ";\n"

databaseExistsSql :: DatabaseName -> Text
databaseExistsSql name = "SELECT count() FROM system.databases WHERE name = " <> quoteLiteral name

checkDatabase :: Client -> DatabaseName -> IO CheckResult
checkDatabase c name = either (const Unknown) (interpretCount ("database " <> name <> " does not exist")) <$> adminQuery c (databaseExistsSql name)

-- | @1@ is 'Success', @0@ a 'Failure' with this text, anything else 'Unknown'.
interpretCount :: Text -> Text -> CheckResult
interpretCount missing out = case Text.strip out of
    "1" -> Success
    "0" -> Failure missing
    _ -> Unknown

-------------------------------------------------------------------------------
-- Users

newtype Password = Password {revealPassword :: Text}

instance Show Password where
    show _ = "<password>"

{- | A user whose password is the content of a file, read at @check@ and @up@
time (never at graph build), verbatim (a trailing newline is part of it, as
for "Salmon.Builtin.Nodes.Postgres".'Salmon.Builtin.Nodes.Postgres.userPassFile').

@up@ creates the user if missing and sets the password either way, so a
rotated file reaches the server: @CREATE USER IF NOT EXISTS@ then @ALTER USER
... IDENTIFIED WITH sha256_hash BY HASH SALT SALT@, the hash of the password
and a fresh random salt, on the client's standard input. The user may log in
from any host. Its grants are left alone.

The check is 'checkUser': the user exists, and a login as it with the file's
password succeeds.
-}
userPassFile :: Reporter Report -> Track' (Binary "clickhouse-client") -> Client -> File "passfile" -> UserName -> Op
userPassFile r clientT c passfile name =
    withFile passfile $ \path ->
        op "clickhouse-user" (deps [justInstall clientT]) $ \actions ->
            actions
                { ref = mkRef "clickhouse-user" (clientKey c, name)
                , help = Text.unwords ["set ClickHouse user", name, "password from file at", Text.pack path]
                , notes = ["server: " <> clientKey c]
                , check = checkUser c name (readPassword path)
                , up = do
                    pwd <- readPassword path
                    salt <- Random.getRandomBytes saltBytes
                    setUser (adminRun c) name pwd salt (contramap (CHSetUser name) r)
                }

readPassword :: FilePath -> IO Password
readPassword path = Password <$> Text.readFile path

saltBytes :: Int
saltBytes = 16

-- | ClickHouse's @sha256_hash@: the hex SHA-256 of the password followed by the salt.
sha256Hash :: Password -> Text -> Text
sha256Hash pwd salt =
    Text.pack (show (Hash.hashWith Hash.SHA256 (Text.encodeUtf8 (pwd.revealPassword <> salt))))

hexSalt :: ByteString.ByteString -> Text
hexSalt = Text.pack . concatMap byte . ByteString.unpack
  where
    byte w = [digits !! fromIntegral (w `div` 16), digits !! fromIntegral (w `mod` 16)]
    digits = "0123456789abcdef"

{- | The statements 'userPassFile' sends, for this password and salt. Holds a
digest of the password: never reported, only written to the client's stdin.
-}
userSql :: UserName -> Password -> ByteString.ByteString -> Text
userSql name pwd saltRaw =
    Text.unlines
        [ "CREATE USER IF NOT EXISTS " <> quoteIdent name <> identified <> ";"
        , "ALTER USER " <> quoteIdent name <> identified <> ";"
        ]
  where
    salt = hexSalt saltRaw
    identified = " IDENTIFIED WITH sha256_hash BY " <> quoteLiteral (sha256Hash pwd salt) <> " SALT " <> quoteLiteral salt

-- | Thrown by a failed 'setUser', with the hash and salt cut out of the client's output.
data SetUserFailed = SetUserFailed !UserName !Int !Text
    deriving (Show)

instance Exception SetUserFailed

{- | Runs 'userSql'. Not 'Binary.untrackedExec': its failure carries the
client's output, and a ClickHouse syntax error quotes the statement, which
holds the hash. Here the reported and thrown output has the hash and the salt
replaced by @<redacted>@.
-}
setUser :: Command "clickhouse-client" AdminBatch -> UserName -> Password -> ByteString.ByteString -> Reporter Binary.Report -> IO ()
setUser cmd name pwd saltRaw r = do
    let p = prepare cmd AdminBatch
        salt = hexSalt saltRaw
        redact = redactAll [sha256Hash pwd salt, salt]
    runReporter r (Binary.CommandStart p)
    (code, out, err) <- readCreateProcessWithExitCode p (Text.encodeUtf8 (userSql name pwd saltRaw))
    let out' = redactBytes redact out
        err' = redactBytes redact err
    runReporter r (Binary.CommandStopped p code out' err')
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> throwIO (SetUserFailed name n (decode err'))

redactAll :: [Text] -> Text -> Text
redactAll secrets t = foldr (\s acc -> if Text.null s then acc else Text.replace s "<redacted>" acc) t secrets

redactBytes :: (Text -> Text) -> ByteString.ByteString -> ByteString.ByteString
redactBytes f = Text.encodeUtf8 . f . decode

userExistsSql :: UserName -> Text
userExistsSql name = "SELECT count() FROM system.users WHERE name = " <> quoteLiteral name

{- | The check of a @clickhouse-user@ node. ClickHouse never gives a password
or its hash back (@system.users@ shows the scheme only), so the password is
tried: a login as the user, with the password in @CLICKHOUSE_PASSWORD@ in the
client's environment (through @podman exec -e@ for a container), running
@SELECT currentUser()@.

* the file cannot be read, or the server cannot be asked: 'Unknown'
* no such user: 'Failure'
* the login is refused (@AUTHENTICATION_FAILED@): 'Failure'
* the login works: 'Success'

Nothing secret is in a verdict. A refused login is in the server's log, as
every wrong password is.
-}
checkUser :: Client -> UserName -> IO Password -> IO CheckResult
checkUser c name readPass = do
    pass <- try readPass :: IO (Either IOException Password)
    case pass of
        Left _ -> pure Unknown
        Right pwd -> do
            exists <- adminQuery c (userExistsSql name)
            case interpretCount ("user " <> name <> " does not exist") <$> exists of
                Left _ -> pure Unknown
                Right Success -> loginAs c name pwd
                Right other -> pure other

loginAs :: Client -> UserName -> Password -> IO CheckResult
loginAs c name pwd = do
    environment <- getEnvironment
    let password = Text.unpack pwd.revealPassword
        argv = loginArgv c name
        p = (argvProc argv){env = Just (("CLICKHOUSE_PASSWORD", password) : filter ((/= "CLICKHOUSE_PASSWORD") . fst) environment)}
    r <- try (readCreateProcessWithExitCode p "") :: IO (Either IOException (ExitCode, ByteString.ByteString, ByteString.ByteString))
    pure $ case r of
        Left _ -> Unknown
        Right (code, _, err) -> interpretLogin name code (decode err)

{- | @clickhouse-client --user NAME --query 'SELECT currentUser()'@; for a
container, @podman exec -e CLICKHOUSE_PASSWORD@, which passes podman's own
variable in, so the value is on no command line.
-}
loginArgv :: Client -> UserName -> [String]
loginArgv c name = case c.clientVia of
    LocalClient -> base
    InContainer cname -> ["podman", "exec", "-i", "-e", "CLICKHOUSE_PASSWORD", Text.unpack (Podman.getContainerName cname)] <> base
  where
    base = ["clickhouse-client", "--port", show c.clientPort, "--user", Text.unpack name, "--query", "SELECT currentUser()"]

interpretLogin :: UserName -> ExitCode -> Text -> CheckResult
interpretLogin _ ExitSuccess _ = Success
interpretLogin name (ExitFailure _) err
    | "AUTHENTICATION_FAILED" `Text.isInfixOf` err = Failure ("ClickHouse refused the declared password of user " <> name)
    | otherwise = Unknown

-------------------------------------------------------------------------------
-- Grants

{- | Privileges on every table of one database, for one user (or role).

Name the privileges as ClickHouse lists them in @SHOW GRANTS@ (@SELECT@,
@INSERT@, @ALTER@, @CREATE TABLE@, @DROP TABLE@, ...). A compound one such as
@ALL@ or @CREATE@ is stored expanded into what it holds, so the check never
finds it as written and @up@ runs again each pass (harmless, a @GRANT@ is
idempotent, but never satisfied).
-}
data Grant
    = Grant
    { grantPrivileges :: [Text]
    , grantDatabase :: DatabaseName
    , grantTo :: UserName
    }
    deriving (Eq, Ord, Show)

{- | @GRANT privs ON db.* TO user@. The check is 'checkGrant'. @down@ revokes
nothing. The user and the database must exist first: declare them as
predecessors (@grant ... \`inject\` userOp@), the node does not.
-}
grant :: Reporter Report -> Track' (Binary "clickhouse-client") -> Client -> Grant -> Op
grant r clientT c g =
    withBinaryStdin clientT (adminRun c) AdminBatch (Text.encodeUtf8 (grantSql g)) $ \apply ->
        op "clickhouse-grant" nodeps $ \actions ->
            actions
                { ref = mkRef "clickhouse-grant" (clientKey c, g.grantTo, g.grantDatabase, sort (normalizedPrivileges g))
                , help = Text.unwords ["grant", Text.intercalate ", " (normalizedPrivileges g), "on", g.grantDatabase <> ".*", "to", g.grantTo]
                , notes = ["server: " <> clientKey c]
                , check = checkGrant c g
                , up = apply (contramap (CHGrant g) r)
                }

normalizedPrivileges :: Grant -> [Text]
normalizedPrivileges g = fmap (Text.unwords . Text.words . Text.map toUpper) g.grantPrivileges

grantSql :: Grant -> Text
grantSql g =
    "GRANT "
        <> Text.intercalate ", " (normalizedPrivileges g)
        <> " ON "
        <> quoteIdent g.grantDatabase
        <> ".* TO "
        <> quoteIdent g.grantTo
        <> ";\n"

-- | The database-wide privileges the grantee holds on the database, one per line.
grantedSql :: Grant -> Text
grantedSql g =
    "SELECT access_type FROM system.grants WHERE (user_name = "
        <> quoteLiteral g.grantTo
        <> " OR role_name = "
        <> quoteLiteral g.grantTo
        <> ") AND database = "
        <> quoteLiteral g.grantDatabase
        <> " AND table IS NULL AND column IS NULL AND is_partial_revoke = 0"

checkGrant :: Client -> Grant -> IO CheckResult
checkGrant c g = either (const Unknown) (interpretGrants g) <$> adminQuery c (grantedSql g)

-- | 'Success' when every declared privilege is among the lines, compared without case.
interpretGrants :: Grant -> Text -> CheckResult
interpretGrants g out =
    case filter (`notElem` held) (normalizedPrivileges g) of
        [] -> Success
        missing -> Failure (g.grantTo <> " lacks " <> Text.intercalate ", " missing <> " on " <> g.grantDatabase <> ".*")
  where
    held = fmap (Text.map toUpper . Text.strip) (filter (not . Text.null . Text.strip) (Text.lines out))

{- | @GRANT READ ON SOURCE TO user@: leave to read an external source, such as
@S3@ (what a PeerDB ClickHouse peer needs: it stages rows in S3 and has
ClickHouse read them, and PeerDB refuses the peer without it). The source
name is ClickHouse's (@S3@, @URL@, @FILE@, ...), upper case. Checked on the
@system.grants@ row (@access_type = READ@, @access_object@ the source), as
25.8 stores it. @down@ revokes nothing. Inject the user first.
-}
sourceReadGrant :: Reporter Report -> Track' (Binary "clickhouse-client") -> Client -> Text -> UserName -> Op
sourceReadGrant r clientT c rawSource name =
    withBinaryStdin clientT (adminRun c) AdminBatch (Text.encodeUtf8 (sourceReadGrantSql source name)) $ \apply ->
        op "clickhouse-source-grant" nodeps $ \actions ->
            actions
                { ref = mkRef "clickhouse-source-grant" (clientKey c, name, source)
                , help = Text.unwords ["grant READ on", source, "to", name]
                , notes = ["server: " <> clientKey c]
                , check = either (const Unknown) (interpretCount (name <> " may not read " <> source)) <$> adminQuery c (sourceReadGrantedSql source name)
                , up = apply (contramap (CHSourceGrant source name) r)
                }
  where
    source = sourceName rawSource

-- | A source as ClickHouse names it: upper case, letters and digits only.
sourceName :: Text -> Text
sourceName = Text.filter isAlphaNum . Text.toUpper

sourceReadGrantSql :: Text -> UserName -> Text
sourceReadGrantSql source name = "GRANT READ ON " <> sourceName source <> " TO " <> quoteIdent name <> ";\n"

sourceReadGrantedSql :: Text -> UserName -> Text
sourceReadGrantedSql source name =
    "SELECT count() > 0 FROM system.grants WHERE user_name = "
        <> quoteLiteral name
        <> " AND access_type = 'READ' AND access_object = "
        <> quoteLiteral (sourceName source)
        <> " AND is_partial_revoke = 0"

-------------------------------------------------------------------------------

showT :: (Show a) => a -> Text
showT = Text.pack . show

decode :: ByteString.ByteString -> Text
decode = Text.decodeUtf8With TextError.lenientDecode
