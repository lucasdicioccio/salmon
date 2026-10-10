{-# LANGUAGE OverloadedStrings #-}

{- | A machine enrolled as a NetBird peer.

"SreBox.WireGuardMesh" covers machines with stable endpoints and
deliberately does not rebuild NAT traversal. For a roaming or NAT-bound
device the complement is NetBird's own client: this module enrols a machine
against a management server with a setup key and answers \"is it enrolled
and connected\" from the client's own status report. It does not provision
the management server, and it does not manage what the management server
decides (groups, policies, routes, the peer's address).

= The pieces

* 'peer' is the enrolment: @netbird up --setup-key-file FILE@ against the
  declared management URL, with a 'check' read from @netbird status --json@.
* 'netbirdRepository' and 'package' are the upstream apt repository and the
  @netbird@ package, for a caller that wants salmon to install the client.
  The package's own post-install script installs and starts the daemon's
  system service (@netbird service install@, @netbird service start@), so
  nothing here writes a unit file. A caller with another source for the
  binary passes its own @'Track'' ('Binary' \"netbird\")@ and makes sure the
  daemon runs.

= The setup key

The setup key is a bearer credential: whoever holds it can add a machine to
the network. It is a pre-provisioned file (how it got there is the caller's
business, and the caller's 'Track'), it is named on the command line as a
path and never as a value, and it is read when the node runs, never when the
graph is built. The client reads the file itself and hands the key to the
daemon over its local socket. Because the client's error text is not under
this module's control, whatever a failed @netbird up@ printed has the key's
bytes replaced before it reaches a report or an exception ('redact').

A setup key is only spent on a peer that is not enrolled yet: on an enrolled
and connected peer @netbird up@ answers @Already connected@ and reads
nothing, so the file may be removed, or the key may expire, without the
node failing afterwards.

= What the node refuses

__Moving a connected peer to another management server.__ A peer that is
connected to a management URL other than the declared one was enrolled by
somebody, into some other network; @netbird up@ would not move it anyway
(it answers @Already connected@), and taking it down to re-enrol it is a
decision this node does not take. 'up' throws 'EnrolledElsewhere' and the
operator runs @netbird down@ (or deregisters the peer) on purpose.

= down

@netbird down@: the peer disconnects and its interface goes away. The peer
__stays registered__ on the management server, and its local state stays on
disk: removing it there needs the management API and a credential this
module does not hold.

= What was verified, and what was not

The flags (@--setup-key-file@, @--management-url@, @--hostname@,
@status --json@), the JSON field names, the @daemonStatus@ values and the
package's post-install behaviour were read in the upstream repository's
client sources and the repository's install documentation (the release
current at the time was v0.80.0). __Nothing here has been run against a
NetBird daemon or a management server__: the fixtures in the tests are
written by hand from the upstream structs, not captured. In particular the
exact spelling of @management.url@ in the status output (with or without
scheme and port) was not observed, which is why 'sameManagement' compares
leniently, and the release that introduced @--setup-key-file@ and the
@daemonStatus@ field was not looked up ('interpretStatus' tolerates a
missing @daemonStatus@).
-}
module Salmon.Builtin.Nodes.Netbird (
    Report (..),
    Enrolment (..),
    enrolment,
    peer,
    EnrolledElsewhere (..),
    MissingSetupKey (..),

    -- * Installing the client
    netbirdRepository,
    package,

    -- * Pieces, exposed for tests
    NetbirdCommand (..),
    netbirdcommand,
    Status (..),
    parseStatus,
    interpretStatus,
    connectedElsewhere,
    sameManagement,
    normalizeManagementUrl,
    redact,
    runRedacted,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import qualified Data.List.NonEmpty as NEList
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), CommandFailed (..), justInstall, untrackedExec)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Debian.AptRepository (AptRepository (..), Pinning (..), Suite (..))
import Salmon.Builtin.Nodes.Debian.Package (Package (..), deb)
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------
data Report
    = RunNetbird !NetbirdCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

-- | What a machine declares about its enrolment.
data Enrolment = Enrolment
    { enrolSetupKeyFile :: FilePath
    -- ^ Pre-provisioned file holding the setup key (surrounding whitespace
    -- is ignored, by the client as by this module).
    , enrolManagementUrl :: Maybe Text
    -- ^ @https://netbird.example.org@ for a self-hosted management server;
    -- 'Nothing' leaves the client's default (the vendor's hosted service)
    -- and then no URL is compared.
    , enrolHostname :: Maybe Text
    -- ^ The name the peer registers under; 'Nothing' leaves the machine's
    -- host name. Only read by the management server at enrolment: a
    -- changed value does not rename an enrolled peer and is not checked.
    }
    deriving (Eq, Show)

-- | An enrolment from a setup-key file, against the client's default
-- management server and under the machine's own name.
enrolment :: FilePath -> Enrolment
enrolment keyFile = Enrolment keyFile Nothing Nothing

-- | Thrown by 'peer''s @up@ when the daemon is connected to a management
-- server other than the declared one. Both URLs are public.
data EnrolledElsewhere = EnrolledElsewhere
    { elsewhereDeclared :: Text
    , elsewhereFound :: Text
    }

instance Show EnrolledElsewhere where
    show e =
        "netbird is connected to another management server ("
            <> Text.unpack e.elsewhereFound
            <> ", declared "
            <> Text.unpack e.elsewhereDeclared
            <> "); not re-enrolling it. Run `netbird down` on purpose first."

instance Exception EnrolledElsewhere

-- | Thrown by 'peer''s @up@ before anything is run when the setup-key file
-- is empty: without a key the client falls back to an interactive login,
-- which nothing would ever answer.
newtype MissingSetupKey = MissingSetupKey FilePath

instance Show MissingSetupKey where
    show (MissingSetupKey path) = "netbird setup-key file is empty: " <> path

instance Exception MissingSetupKey

{- | The machine's NetBird daemon, enrolled and connected.

There is one daemon per machine (the client's default daemon address), so
that is the effect site: two declarations with different management URLs
collide and are reported, rather than fighting over the daemon.

* 'check' is 'interpretStatus' over @netbird status --json@. A daemon that
  cannot be reached is a 'Failure' (the following @up@ then fails with the
  client's own message, which names the service to start).
* 'up' reads the status first, refuses a peer connected elsewhere
  ('EnrolledElsewhere'), refuses an empty key file ('MissingSetupKey'), and
  runs @netbird up@. It throws on a non-zero exit, with the key redacted
  from the output it quotes.
* 'down' is @netbird down@; see the module header for what that leaves.

The second argument provisions the binary ('package', or the caller's own);
the third provisions the setup-key file (@ignoreTrack@ when it is already
on the machine).
-}
peer ::
    Reporter Report ->
    Track' (Binary "netbird") ->
    Track' FilePath ->
    Enrolment ->
    Op
peer r netbird key enrol =
    op "netbird-peer" (deps [justInstall netbird, run key enrol.enrolSetupKeyFile]) $ \actions ->
        actions
            { help = "netbird peer enrolled against " <> fromMaybe "the default management server" enrol.enrolManagementUrl
            , notes =
                [ "setup key read from: " <> Text.pack enrol.enrolSetupKeyFile
                , "registers as: " <> fromMaybe "the machine's host name" enrol.enrolHostname
                ]
            , ref = mkRef "netbird-peer" ("default-daemon" :: Text)
            , check = either id (interpretStatus enrol) <$> readStatus
            , up = do
                current <- readStatus
                case current of
                    Right st | Just found <- connectedElsewhere enrol st ->
                        throwIO (EnrolledElsewhere (fromMaybe "" enrol.enrolManagementUrl) found)
                    _ -> pure ()
                secret <- C8.strip <$> ByteString.readFile enrol.enrolSetupKeyFile
                when (ByteString.null secret) $ throwIO (MissingSetupKey enrol.enrolSetupKeyFile)
                let cmd = Up enrol
                runRedacted secret (prepare netbirdcommand cmd) (r' cmd)
            , down = untrackedExec netbirdcommand Down "" (r' Down)
            }
  where
    r' cmd = contramap (RunNetbird cmd) r

-- | @netbird status --json@, parsed. 'Left' is the verdict when there is
-- nothing to interpret.
readStatus :: IO (Either CheckResult Status)
readStatus = do
    (code, out, _) <- readCreateProcessWithExitCode (prepare netbirdcommand StatusJson) ""
    pure $ case code of
        ExitFailure _ -> Left (Failure "netbird daemon not reachable")
        ExitSuccess -> maybe (Left Unknown) Right (parseStatus out)

{- | 'Binary.untrackedExec' for a command whose output may quote a secret:
same reports, same 'CommandFailed' on a non-zero exit, with every occurrence
of the secret replaced in both streams first. The output stays captured
(never streamed) for the same reason.
-}
runRedacted :: ByteString -> CreateProcess -> Reporter Binary.Report -> IO ()
runRedacted secret p r = do
    runReporter r (Binary.CommandStart p)
    (code, out0, err0) <- readCreateProcessWithExitCode p ""
    let out = redact secret out0
        err = redact secret err0
    runReporter r (Binary.CommandStopped p code out err)
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> throwIO (CommandFailed p n out err)

{- | Replace every occurrence of the secret with @[redacted]@. An empty
secret redacts nothing (it would match everywhere).
-}
redact :: ByteString -> ByteString -> ByteString
redact secret
    | ByteString.null secret = id
    | otherwise = go
  where
    go hay =
        let (before, rest) = ByteString.breakSubstring secret hay
         in if ByteString.null rest
                then before
                else before <> "[redacted]" <> go (ByteString.drop (ByteString.length secret) rest)

-------------------------------------------------------------------------------

-- | The part of @netbird status --json@ the check reads.
data Status = Status
    { statusDaemon :: Maybe Text
    -- ^ @daemonStatus@: @Idle@, @Connecting@, @Connected@, @NeedsLogin@,
    -- @LoginFailed@, @SessionExpired@. Absent from clients that predate the
    -- field.
    , statusManagementUrl :: Text
    , statusManagementConnected :: Bool
    }
    deriving (Eq, Show)

instance Aeson.FromJSON Status where
    parseJSON = Aeson.withObject "netbird status" $ \o -> do
        management <- o .: "management"
        Status
            <$> o .:? "daemonStatus"
            <*> (fromMaybe "" <$> management .:? "url")
            <*> management .: "connected"

parseStatus :: ByteString -> Maybe Status
parseStatus = Aeson.decodeStrict

{- | The verdict on an enrolment from the daemon's status.

* Connected to the management server, and to the declared one when a URL is
  declared: 'Success'.
* Connected to another management server: 'Failure' (and 'peer''s @up@ will
  refuse, see 'EnrolledElsewhere').
* @Connecting@: 'Unknown', a transitional state that an @up@ would not help.
* Anything else (@NeedsLogin@, @LoginFailed@, @SessionExpired@, @Idle@, a
  daemon that says @Connected@ while the management connection is down, or
  an older client reporting only that the management connection is down):
  'Failure' naming the state.

The reasons quote the daemon's state and the two URLs, nothing else from
the report.
-}
interpretStatus :: Enrolment -> Status -> CheckResult
interpretStatus enrol st =
    case st.statusDaemon of
        Just "Connecting" -> Unknown
        Just "Connected" -> whenConnected
        Just other -> Failure ("netbird daemon is " <> other)
        Nothing -> whenConnected
  where
    whenConnected
        | not st.statusManagementConnected = Failure "not connected to the management server"
        | Just found <- connectedElsewhere enrol st = Failure ("connected to another management server: " <> found)
        | otherwise = Success

-- | The management URL the daemon is connected to, when a URL is declared
-- and it is not that one.
connectedElsewhere :: Enrolment -> Status -> Maybe Text
connectedElsewhere enrol st = do
    declared <- enrol.enrolManagementUrl
    if st.statusManagementConnected && not (Text.null st.statusManagementUrl) && not (sameManagement declared st.statusManagementUrl)
        then Just st.statusManagementUrl
        else Nothing

-- | Whether two management URLs name the same server; see
-- 'normalizeManagementUrl'.
sameManagement :: Text -> Text -> Bool
sameManagement a b = normalizeManagementUrl a == normalizeManagementUrl b

{- | Scheme, host and port of a management URL, lower-cased, with the path
dropped, a missing scheme read as @https@ and a missing port filled in from
the scheme (443, or 80 for @http@). Lenient on purpose: how the client spells
the URL in its status report was not observed.
-}
normalizeManagementUrl :: Text -> (Text, Text, Text)
normalizeManagementUrl url = (scheme, host, port)
  where
    lowered = Text.toLower (Text.strip url)
    (scheme, rest) = case Text.breakOn "://" lowered of
        (s, r) | not (Text.null r) -> (s, Text.drop 3 r)
        _ -> ("https", lowered)
    authority = Text.takeWhile (/= '/') rest
    (host, port) = case Text.breakOnEnd ":" authority of
        (h, p)
            | not (Text.null h) && not (Text.null p) && Text.all (`elem` ("0123456789" :: String)) p ->
                (Text.dropEnd 1 h, p)
        _ -> (authority, if scheme == "http" then "80" else "443")

-------------------------------------------------------------------------------

data NetbirdCommand
    = Up Enrolment
    | Down
    | StatusJson
    deriving (Show)

-- | The setup key is only ever named by its file.
netbirdcommand :: Command "netbird" NetbirdCommand
netbirdcommand = Command $ \cmd -> case cmd of
    Up enrol ->
        netbird $
            mconcat
                [ ["up", "--setup-key-file", enrol.enrolSetupKeyFile]
                , maybe [] (\u -> ["--management-url", Text.unpack u]) enrol.enrolManagementUrl
                , maybe [] (\h -> ["--hostname", Text.unpack h]) enrol.enrolHostname
                ]
    Down -> netbird ["down"]
    StatusJson -> netbird ["status", "--json"]
  where
    netbird :: [String] -> CreateProcess
    netbird = proc "netbird"

-------------------------------------------------------------------------------

{- | The upstream apt repository (@pkgs.netbird.io@), pinned to the
@netbird@ package only. The signing key is a file the caller provisioned and
its fingerprint is the caller's to declare, as for every
'Salmon.Builtin.Nodes.Debian.AptRepository.AptRepository'.
-}
netbirdRepository :: FilePath -> Text -> AptRepository
netbirdRepository keyFile fingerprint =
    AptRepository
        { repoName = "netbird"
        , repoUris = "https://pkgs.netbird.io/debian"
        , repoSuite = FixedSuite "stable"
        , repoComponents = ["main"]
        , repoKeyFile = keyFile
        , repoKeyFingerprint = fingerprint
        , repoPin = OnlyPackages ("netbird" NEList.:| [])
        , repoAptDir = "/etc/apt"
        , repoListsDir = "/var/lib/apt/lists"
        }

{- | The @netbird@ package, as a provider of the binary. The argument makes
the package installable: 'Salmon.Builtin.Nodes.Debian.AptRepository.viaRepository'
of a 'netbirdRepository', or @ignoreTrack@ when it already is.

Installing the package starts the daemon (its post-install script installs
and starts the system service). The daemon does nothing on the network until
a peer is enrolled.
-}
package :: Track' () -> Track' (Binary "netbird")
package source = Track $ \_ -> deb (Package "netbird") `inject` run source ()
