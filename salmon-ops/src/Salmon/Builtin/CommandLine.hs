{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Salmon.Builtin.CommandLine where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, void, when)
import Data.Foldable (traverse_)
import Control.Monad.Identity
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBysteString
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (doesFileExist)
import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Options.Applicative
import qualified Options.Applicative
import Options.Generic
import System.Exit (exitFailure)
import Control.Exception (IOException, try)
import System.IO (BufferMode (..), Handle, IOMode (AppendMode), hPutStrLn, hSetBuffering, openFile, stderr, stdin, stdout)

import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Concurrency as Concurrency
import Salmon.Op.Configure
import qualified Salmon.Op.Dag as Dag
import Salmon.Op.PassPhase (PassPhase (..))
import qualified Salmon.Op.PassPhase as PassPhase
import Salmon.Op.Ref (Ref)
import Salmon.Op.Rewrite (Phase (..), Rewrite, Rewritten)
import qualified Salmon.Op.Rewrite as Rewrite
import Salmon.Op.Eval
import Salmon.Op.OpGraph
import Salmon.Op.Track

import Salmon.Actions.Dot as Dot
import qualified Salmon.Actions.Follow as Follow
import qualified Salmon.Actions.Follow.Registry as Registry
import qualified Salmon.Actions.Follow.Registry.Http as Registry.Http
import qualified Salmon.Actions.Follow.Scheduler as Scheduler
import qualified Salmon.Actions.Follow.Signature as Signature
import Salmon.Actions.Help as Help
import qualified Salmon.Actions.Query as Query
import qualified Salmon.Actions.Serve as Serve
import qualified Salmon.Actions.Serve.Events as Events
#ifdef SALMON_HTTP_SERVER
import qualified Salmon.Actions.Serve.Http as Http
#else
-- built with -http-server: the same names, refusing (see 'httpServerAvailable')
import qualified Salmon.Actions.Serve.NoHttp as Http
#endif
import qualified Salmon.Actions.Serve.Socket as Socket
import qualified Salmon.Actions.Serve.StatusSink as StatusSink
-- 'CheckResult' constructors are hidden: 'Success'/'Failure' collide with
-- optparse-applicative's 'ParserResult' ones, which this module pattern
-- matches on. Nothing here needs a 'CheckResult'.
import Salmon.Actions.UpDown as UpDown hiding (Failure, Success)
import Salmon.Builtin.Extension
import qualified Salmon.Op.Window as Window
import Salmon.Reporter
import qualified Salmon.Builtin.NodeLog as NodeLog
import qualified Salmon.Reporter.Tagged as Tagged

data Command seed
    = Config seed
    | Query QueryCommand
    | Run RunCommand
    deriving (Eq, Ord, Generic, Show)

-- | Kept for the JSON/remote-call contract in "Salmon.Builtin.Nodes.Self"
-- ('CLI.RemoteCall'/'argForBaseCommand') — a self-call is always a plain
-- 'Up' today, never plan-aware, so this never needs a plan file field.
data BaseCommand
    = Up
    | Down
    | Tree
    | DAG
    | Serve
    deriving (Eq, Ord, Generic, Show, Read)

argForBaseCommand :: BaseCommand -> Text
argForBaseCommand = \case
    Up -> "up"
    Down -> "down"
    Tree -> "tree"
    DAG -> "dag"
    Serve -> "serve"

-- | The @run@ subcommand's own subcommands. Parsed by hand (rather than via
-- 'Options.Generic''s derived 'ParseRecord', which 'BaseCommand' still uses
-- for its own, unrelated JSON contract) so that @up@ can carry an optional
-- @--plan@/@--force-stale-plan@ pair.
data RunCommand
    = -- | @run up@, optionally honoring a @query plan@-emitted 'Query.Plan' file.
      -- The windows (@--maintenance-window@) outside which 'Window.disruptive'
      -- nodes are held, and whether to ignore them (@--override-window@).
      -- Last, @--phase build@: run only the build phase of a two-phase pass
      -- ("Salmon.Op.PassPhase"); 'Nothing', the default, runs everything.
      RunUp !(Maybe FilePath) !Bool !ReportFormat ![Text] !Bool !(Maybe PassPhase)
    | RunDown !ReportFormat
    | RunTree
    | RunDAG
    | -- | @run serve@, optionally capping how many nodes converge at once
      -- per pass (R6 in @specs/per-node-state-machines-remaining.md@;
      -- 'Nothing' is unbounded, matching every version of @serve@ before
      -- this flag existed), what the loop does before reading any input
      -- ('StartupOptions': @--no-supervise@, @--up-directive FILE@), and
      -- optionally starting with @autoconverge@ off
      -- (@--no-autoconverge@; 'False' is the default, matching every
      -- version of @serve@ before the setting existed). Then pull mode
      -- ("Salmon.Actions.Follow"): a registry to follow (@--follow
      -- REGISTRY@, a directory, @git+URL@, an HTTP URL, @dns:ZONE@ or a
      -- bucket — see "Salmon.Actions.Follow.Registry"), the labels to
      -- fetch from it (@--label L@,
      -- repeatable; both or neither), and the fetcher's schedule
      -- ('FollowOptions'). Last, optionally listening for the same
      -- line protocol on a unix socket (@--listen PATH@, milestone 2 of
      -- @specs/generic-server.md@; see "Salmon.Actions.Serve.Socket"),
      -- stdin still read beside it, and for HTTP on a second unix socket
      -- (@--http PATH@, milestone 3; see "Salmon.Actions.Serve.Http").
      -- Last, the status sink ('SinkOptions', milestone 5 of
      -- @specs/pull-mode.md@; see "Salmon.Actions.Serve.StatusSink").
      -- The HTTP server's event stream keeps @--events-ring N@ events for a
      -- client to resume from (milestone 4; see "Salmon.Actions.Serve.Events").
      -- Last of all, the same HTTP over the network ('TcpOptions', milestone
      -- 8): @--http-tcp HOST:PORT@ with @--tls-cert@, @--tls-key@ and
      -- @--token-file@, all three or nothing. And whether every HTTP
      -- listener refuses @POST \/command@ (@--http-read-only@; 'False', the
      -- default, is every version before the flag; see 'Http.Access').
      -- Then the maintenance windows and @--override-window@, as for
      -- 'RunUp' (see 'Serve.serveObservedWith' for what @serve@ does with them).
      -- Finally @--once@ ('Serve.AfterFirstPass': end after the first
      -- convergence pass and exit with its verdict) and @--linger SECONDS@
      -- (keep the listeners answering reads that long before exiting; only
      -- with @--once@).
      RunServe !(Maybe Int) !StartupOptions !Bool !ReportFormat !(Maybe FilePath) ![Text] !FollowOptions !(Maybe FilePath) !(Maybe FilePath) !Int !SinkOptions !TcpOptions !Bool ![Text] !Bool !Bool !(Maybe Int)
    deriving (Eq, Ord, Generic, Show)

{- | The four flags that put the HTTP surface on a network, as typed. What
they mean together is 'validateTcpOptions': there is no way to spell a
plaintext listener, and the only accepted shapes are none of them or all of
them.
-}
data TcpOptions = TcpOptions
    { tcpBind :: !(Maybe String)
    -- ^ @--http-tcp HOST:PORT@
    , tcpCert :: !(Maybe FilePath)
    -- ^ @--tls-cert FILE@
    , tcpKey :: !(Maybe FilePath)
    -- ^ @--tls-key FILE@
    , tcpTokenFile :: !(Maybe FilePath)
    -- ^ @--token-file FILE@
    , tcpSessionLifetime :: !(Maybe Double)
    -- ^ @--session-lifetime SECONDS@, @0@ for none
    , tcpSessionIdle :: !(Maybe Double)
    -- ^ @--session-idle SECONDS@, @0@ for none
    }
    deriving (Eq, Ord, Generic, Show)

instance FromJSON TcpOptions
instance ToJSON TcpOptions

{- | Whether this @salmon-ops@ was built with its @http-server@ flag (the
default), i.e. whether @run serve --http@\/@--http-tcp@ can do anything.
Without it the library has no warp, warp-tls or wai in its plan, and the
loop runs over standard input and @--listen@ only.
-}
httpServerAvailable :: Bool
#ifdef SALMON_HTTP_SERVER
httpServerAvailable = True
#else
httpServerAvailable = False
#endif

-- | What @run serve@ says when given an HTTP flag it was built without.
httpServerUnavailable :: String
httpServerUnavailable = "this binary was built with salmon-ops -http-server: --http and --http-tcp are not available (rebuild with the flag on, its default)"

-- | No network listener at all: the default.
noTcp :: TcpOptions
noTcp = TcpOptions Nothing Nothing Nothing Nothing Nothing Nothing

-- | A validated 'TcpOptions': where to listen and the three files, all present.
data TcpListen = TcpListen
    { tcpHost :: !String
    , tcpPort :: !Int
    , tcpCertFile :: !FilePath
    , tcpKeyFile :: !FilePath
    , tcpTokenPath :: !FilePath
    , tcpSessionPolicy :: !Http.SessionPolicy
    -- ^ 'Http.defaultSessionPolicy' with whatever @--session-*@ said
    }
    deriving (Eq, Show)

{- | The loud default, as a pure function so it can be tested without a
process: 'Nothing' when none of the four is given; a 'TcpListen' when all
four are and the address parses; otherwise the message the binary exits
with, naming every flag that is missing — so an operator who typed
@--http-tcp@ alone is told about all three at once rather than one per
attempt — or, for @--tls-cert@\/@--tls-key@\/@--token-file@ without
@--http-tcp@, that they do nothing on their own. @HOST@ is never implied:
@:8443@ is refused, since listening on every address is exactly the thing
that should have to be spelled out (@0.0.0.0:8443@ does it). An IPv6 address
is written in brackets, @[::1]:8443@. @--session-lifetime@\/@--session-idle@
are the browser sign-in's limits ('Http.SessionPolicy'), @0@ turning one
off, a negative one refused, and like the files they do nothing without
@--http-tcp@.
-}
validateTcpOptions :: TcpOptions -> Either Text (Maybe TcpListen)
validateTcpOptions opts =
    case opts.tcpBind of
        Nothing
            | null given -> Right Nothing
            | otherwise -> Left (Text.intercalate ", " given <> " need --http-tcp HOST:PORT to apply to; there is no network listener without it")
        Just hostPort ->
            case missing of
                [] -> do
                    (host, port) <- parseHostPort hostPort
                    lifetime <- limit "--session-lifetime" opts.tcpSessionLifetime Http.defaultSessionPolicy.sessionLifetime
                    idle <- limit "--session-idle" opts.tcpSessionIdle Http.defaultSessionPolicy.sessionIdle
                    Right (Just (TcpListen host port (fromJust opts.tcpCert) (fromJust opts.tcpKey) (fromJust opts.tcpTokenFile) (Http.SessionPolicy lifetime idle)))
                _ ->
                    Left
                        ( "--http-tcp needs "
                            <> Text.intercalate ", " missing
                            <> ": a salmon server never listens on a network without TLS and a token"
                        )
  where
    named =
        [ ("--tls-cert", opts.tcpCert)
        , ("--tls-key", opts.tcpKey)
        , ("--token-file", opts.tcpTokenFile)
        ]
    given =
        [flag | (flag, Just _) <- named]
            ++ [flag | (flag, True) <- [("--session-lifetime", isJust opts.tcpSessionLifetime), ("--session-idle", isJust opts.tcpSessionIdle)]]
    missing = [flag | (flag, Nothing) <- named]
    limit :: Text -> Maybe Double -> Maybe Double -> Either Text (Maybe Double)
    limit flag typed dflt =
        case typed of
            Nothing -> Right dflt
            Just 0 -> Right Nothing
            Just n
                | n > 0 -> Right (Just n)
                | otherwise -> Left (flag <> ": not a number of seconds: " <> Text.pack (show n) <> " (0 turns it off)")

-- | @HOST:PORT@, with @[v6]:PORT@ for an IPv6 address; the port is 0..65535.
parseHostPort :: String -> Either Text (String, Int)
parseHostPort s =
    case break (== ':') (reverse s) of
        (portRev, ':' : hostRev) -> do
            let host = unbracket (reverse hostRev)
                portText = reverse portRev
            port <- case reads portText of
                [(n, "")] | n >= 0 && n <= 65535 -> Right n
                _ -> Left ("--http-tcp: not a port: " <> Text.pack (show portText))
            when (null host) (Left ("--http-tcp: no host in " <> Text.pack (show s) <> "; spell the address, 0.0.0.0 included"))
            Right (host, port)
        _ -> Left ("--http-tcp: expected HOST:PORT, got " <> Text.pack (show s))
  where
    unbracket h
        | Just inner <- stripBrackets h = inner
        | otherwise = h
    stripBrackets ('[' : rest) | not (null rest) && last rest == ']' = Just (init rest)
    stripBrackets _ = Nothing

{- | @--status-sink PATH|URL@, @--status-sink-interval SECONDS@ (default
'StatusSink.defaultInterval') and @--status-sink-host NAME@: where this
host's status document is written, how often between the writes a
convergence pass or a follow injection triggers on their own, and what the
document's @host@ says — 'StatusSink.hostName' (@uname -n@) when not given.
Naming it is for two loops on one machine (a fold shows two rows naming one
host otherwise) and for a container whose node name means nothing to the
reader. 'Nothing' for the path writes none.
-}
data SinkOptions = SinkOptions
    { sinkPath :: !(Maybe FilePath)
    , sinkInterval :: !Int
    , sinkHost :: !(Maybe Text)
    }
    deriving (Eq, Ord, Generic, Show)

instance FromJSON SinkOptions
instance ToJSON SinkOptions

{- | What @run serve@ does before it reads a line of input, so that a loop
started by a unit file or a script needs nothing typed on standard input:
@--no-supervise@ is @supervise off@ and each @--up-directive FILE@
(repeatable, in order) is @up-directive FILE@, run as the loop's first
commands under the @startup@ origin and before standard input, a socket
client, an HTTP request or the fetcher's first round is read (see
'Serve.linesProducer'). Neither changes what the commands mean: an
@--up-directive@ converges at once unless @--no-autoconverge@ holds it for a
@converge@, and a @supervise on@ typed later is obeyed. The defaults run
nothing, which is every version before the flags.
-}
data StartupOptions = StartupOptions
    { startupSupervise :: !Bool
    -- ^ 'False' with @--no-supervise@
    , startupDirectives :: ![FilePath]
    -- ^ @--up-directive FILE@, in the order given
    }
    deriving (Eq, Ord, Generic, Show)

instance FromJSON StartupOptions
instance ToJSON StartupOptions

-- | Supervision on, no directive: what @run serve@ starts with by default.
noStartup :: StartupOptions
noStartup = StartupOptions True []

{- | The serve-command lines 'StartupOptions' stands for, in the order they
run: @supervise off@ first, so that nothing a directive brings up is tended
in between, then each directive.
-}
startupLines :: StartupOptions -> [String]
startupLines o =
    ["supervise off" | not o.startupSupervise]
        <> ["up-directive " <> Serve.quoteArg path | path <- o.startupDirectives]

instance FromJSON RunCommand
instance ToJSON RunCommand

{- | The @--follow-*@ flags: the scheduler's numbers
("Salmon.Actions.Follow.Scheduler"), in seconds where they are durations,
then the cache directory (@--follow-cache DIR@; none by default, in which
case nothing survives a restart) and @--follow-refuse-older@ (see
'Follow.followRefuseOlder'). @--follow-interval@ is milestone 2's name for
the base delay, kept as a synonym of @--follow-base@; either may be given,
the base's own flag wins. Then the backends' own knobs (milestone 6):
@--follow-timeout@ for the HTTP-backed ones, @--follow-workdir@ for the git
checkout, @--follow-bucket-endpoint@ for an S3-compatible store. Last,
@--follow-key FILE@ (repeatable): the public keys a document must be signed
by ("Salmon.Actions.Follow.Signature"); with none given, documents are
taken as they come — __unsigned mode is the default__.
-}
data FollowOptions = FollowOptions
    { followBase :: !(Maybe Int)
    , followInterval :: !(Maybe Int)
    , followFactor :: !Double
    , followCap :: !Int
    , followJitter :: !Double
    , followDebounce :: !Int
    , followMaxWait :: !Int
    , followCacheDir :: !(Maybe FilePath)
    , followRefuseOlder :: !Bool
    , followTimeout :: !Int
    , followWorkdir :: !(Maybe FilePath)
    , followBucketEndpoint :: !(Maybe Text)
    , followKeys :: ![FilePath]
    -- ^ each @FILE@ (a key that speaks for any label) or @LABEL=FILE@ (one that
    -- speaks for that label only)
    , followAcceptUnlabelled :: !Bool
    -- ^ the migration flag: accept a signed document that names no label
    }
    deriving (Eq, Ord, Generic, Show)

instance FromJSON FollowOptions
instance ToJSON FollowOptions

followSchedule :: FollowOptions -> Scheduler.Config
followSchedule o =
    Scheduler.Config
        { Scheduler.schedBase = seconds (fromMaybe defaultBase (o.followBase <|> o.followInterval))
        , Scheduler.schedFactor = max 1 o.followFactor
        , Scheduler.schedCap = seconds o.followCap
        , Scheduler.schedJitter = max 0 (min 1 o.followJitter)
        , Scheduler.schedDebounce = seconds o.followDebounce
        , Scheduler.schedMaxWait = seconds o.followMaxWait
        }
  where
    defaultBase = Scheduler.defaultConfig.schedBase `div` 1000000

-- | A flag in seconds, as the microseconds the schedule and the backends take.
seconds :: Int -> Int
seconds n = max 0 n * 1000000

{- | How the commands that execute something (@run up@, @run down@, @run
serve@) report. @--json@ selects 'ReportJson': one JSON object per line on
stdout, in the encoding "Salmon.Reporter.Tagged" defines, in place of the
binary's own text reporters — so @run up --json | jq@ works, and a client
of the server @specs\/generic-server.md@ sketches reads the same objects.
Absent, 'ReportText' hands every report to the reporters the binary
passed in, untouched. @run serve --json-log FILE@ selects
'ReportTextJsonLog': the text reports on the terminal exactly as
'ReportText' prints them, and the very objects @--json@ would print
appended to FILE, one per line, flushed per report (so @tail -f FILE | jq@
follows a loop whose operator reads text). It excludes @--json@; @run
up@\/@run down@ do not take it.
-}
data ReportFormat
    = ReportText
    | ReportJson
    | ReportTextJsonLog !FilePath
    deriving (Eq, Ord, Generic, Show)

instance FromJSON ReportFormat
instance ToJSON ReportFormat

data QueryCommand
    = -- | @query show@: annotate the directive's tree with [selected]/[excluded].
      -- The 'Bool's are dedupe (print each 'Salmon.Op.Ref.Ref' only once, at
      -- its first-encountered path; on by default, @--no-dedupe@ turns it
      -- off) and descriptions (@--descriptions@: print each node's help text
      -- on an indented line below its path).
      QueryShow !QuerySelection !Bool !Bool
    | -- | @query plan@: emit a 'Query.Plan' (JSON) for @run up --plan@.
      QueryPlan !QuerySelection !Bool
    | -- | @query extract-directive@: recover an embedded directive from a
      -- 'Query.Plan' file made with @query plan --embed-directive@.
      QueryExtractDirective !FilePath
    deriving (Eq, Ord, Generic, Show)

instance FromJSON QueryCommand
instance ToJSON QueryCommand

data QuerySelection = QuerySelection
    { querySelect :: [Text]
    , queryExclude :: [Text]
    }
    deriving (Eq, Ord, Generic, Show)

instance FromJSON QuerySelection
instance ToJSON QuerySelection

instance (ParseRecord seed) => ParseRecord (Command seed) where
    parseRecord =
        combo <**> helper
      where
        combo =
            hsubparser $
                mconcat
                    [ command "config" (info (Config <$> parseRecord) cfg)
                    , command "query" (info (Query <$> queryCommandParser) qry)
                    , command "run" (info (Run <$> runCommandParser) run)
                    , commandGroup "Salmon Commands."
                    ]
        cfg = progDesc "Prints a config."
        qry = progDesc "Inspects, or plans an exclusion against, a directive on stdin."
        run = progDesc "Runs a config."

runCommandParser :: Parser RunCommand
runCommandParser =
    hsubparser $
        mconcat
            [ command "up" (info upP (progDesc "Runs (up) the directive on stdin."))
            , command "down" (info (RunDown <$> reportFormatP) (progDesc "Tears down (down) the directive on stdin."))
            , command "tree" (info (pure RunTree) (progDesc "Prints a human-readable dependency tree."))
            , command "dag" (info (pure RunDAG) (progDesc "Prints Graphviz dot output."))
            , command "serve" (info serveP (progDesc "Reads a stream of seed declarations on stdin and converges."))
            ]
  where
    serveP =
        RunServe
            <$> optional
                ( Options.Applicative.option
                    Options.Applicative.auto
                    ( long "max-concurrency"
                        <> Options.Applicative.metavar "N"
                        <> Options.Applicative.help "Cap how many nodes converge (check/up/down) at once per pass. Omitted = unbounded."
                    )
                )
            <*> startupOptionsP
            <*> switch
                ( long "no-autoconverge"
                    <> Options.Applicative.help
                        "Start with `autoconverge off`: declarations are recorded but not converged until an explicit `converge`."
                )
            <*> (jsonLogP <|> reportFormatP)
            <*> optional
                ( strOption
                    ( long "follow"
                        <> Options.Applicative.metavar "REGISTRY"
                        <> Options.Applicative.help "Pull mode: fetch declarations (one document per --label) from a registry: a directory (DIR/<label>.json), git+URL[#BRANCH[:SUBDIR]] (SUBDIR/<label>.json in the checkout), an http(s):// URL (<base>/<label>.json, or {label} placed in it), dns:ZONE (a TXT index at <label>.ZONE naming an https URL and a sha256), or s3://BUCKET/PREFIX / gs://BUCKET/PREFIX (public or presigned object URLs; no SDK, no credentials)."
                    )
                )
            <*> many
                ( strOption
                    ( long "label"
                        <> Options.Applicative.metavar "LABEL"
                        <> Options.Applicative.help "A label to follow in the --follow registry; repeatable, the desired set is the union."
                    )
                )
            <*> followOptionsP
            <*> optional
                ( strOption
                    ( long "listen"
                        <> Options.Applicative.metavar "PATH"
                        <> Options.Applicative.help
                            "Also accept the line protocol on a unix socket at PATH (created owner-only); each client is answered on its own connection, as JSON lines. Stdin keeps working alongside."
                    )
                )
            <*> optional
                ( strOption
                    ( long "http"
                        <> Options.Applicative.metavar "PATH"
                        <> Options.Applicative.help
                            "Also serve HTTP on a unix socket at PATH (created owner-only): GET /dag, /status, /history, /help/seed and POST /command[?async]. Stdin keeps working alongside."
                    )
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "events-ring"
                    <> Options.Applicative.metavar "N"
                    <> Options.Applicative.value (Events.configRing Events.defaultConfig)
                    <> Options.Applicative.showDefault
                    <> Options.Applicative.help "How many events --http's /events keeps for a client to resume from with ?since=; a client further behind is sent a gap event."
                )
            <*> sinkOptionsP
            <*> tcpOptionsP
            <*> switch
                ( long "http-read-only"
                    <> Options.Applicative.help
                        "Spectate-only HTTP: --http and --http-tcp serve the page, the reads and /events, and answer every POST /command with 403, so no HTTP request changes the declarations, the wanted nodes or supervision, or ends the loop. Stdin and --listen are unaffected."
                )
            <*> maintenanceWindowsP "Nodes marked disruptive are brought up only inside these windows (repeatable): outside them a pass skips them without counting them converged, `status` shows until when they are held, and a tended node waits for the next opening (a `force` still goes through). Teardowns are not held. DAY is Mon..Sun; a start later than the end crosses midnight; the zone is a fixed offset, UTC by default."
            <*> overrideWindowP
            <*> switch
                ( long "once"
                    <> Options.Applicative.help
                        "End after the first convergence pass (a declaration's, an explicit `converge`'s or a fetched document's) and exit with its verdict: 0 when it converged (nothing failed, nothing left), 1 when a node failed, was blocked or remains, or when the session ended before any pass ran. Input queued behind the command that ran the pass is not read. Starts with supervision off."
                )
            <*> optional
                ( Options.Applicative.option
                    Options.Applicative.auto
                    ( long "linger"
                        <> Options.Applicative.metavar "SECONDS"
                        <> Options.Applicative.help
                            "With --once: after the pass, keep --listen, --http and --http-tcp up this long (reads answer from the final state, commands are refused) before exiting, so a client such as salmon-tui can show the end state."
                    )
                )
    startupOptionsP =
        StartupOptions
            <$> ( not
                    <$> switch
                        ( long "no-supervise"
                            <> Options.Applicative.help
                                "Start with `supervise off`: nodes are left alone between passes until a `supervise on`."
                        )
                )
            <*> many
                ( strOption
                    ( long "up-directive"
                        <> Options.Applicative.metavar "FILE"
                        <> Options.Applicative.help
                            "Declare the directive JSON in FILE before reading any input, as a first `up-directive FILE` would (repeatable, in order; converged at once unless --no-autoconverge). The file must exist at start-up. With --once the first directive's pass is the one pass: later directives and stdin are not read."
                    )
                )
    tcpOptionsP =
        TcpOptions
            <$> optional
                ( strOption
                    ( long "http-tcp"
                        <> Options.Applicative.metavar "HOST:PORT"
                        <> Options.Applicative.help
                            "Also serve the same HTTP over TCP at HOST:PORT, with TLS and a bearer token on every request. Requires --tls-cert, --tls-key and --token-file; there is no plaintext option. Spell the host ([::1]:8443 for IPv6)."
                    )
                )
            <*> optional
                ( strOption
                    ( long "tls-cert"
                        <> Options.Applicative.metavar "FILE"
                        <> Options.Applicative.help "The PEM certificate (with its chain, if any) --http-tcp serves."
                    )
                )
            <*> optional
                ( strOption
                    ( long "tls-key"
                        <> Options.Applicative.metavar "FILE"
                        <> Options.Applicative.help "The PEM private key for --tls-cert."
                    )
                )
            <*> optional
                ( strOption
                    ( long "token-file"
                        <> Options.Applicative.metavar "FILE"
                        <> Options.Applicative.help "A file holding the bearer token every --http-tcp request must present (surrounding whitespace ignored). Refused if readable by others."
                    )
                )
            <*> optional
                ( Options.Applicative.option
                    Options.Applicative.auto
                    ( long "session-lifetime"
                        <> Options.Applicative.metavar "SECONDS"
                        <> Options.Applicative.help "How long a browser's sign-in at /auth on --http-tcp lasts, however busy; an event stream it holds is cut at the end (default 43200, 12h; 0 for no limit)."
                    )
                )
            <*> optional
                ( Options.Applicative.option
                    Options.Applicative.auto
                    ( long "session-idle"
                        <> Options.Applicative.metavar "SECONDS"
                        <> Options.Applicative.help "How long a browser's sign-in on --http-tcp lasts with nothing using it; an open event stream counts as use (default 3600, 1h; 0 for no limit)."
                    )
                )
    sinkOptionsP =
        SinkOptions
            <$> optional
                ( strOption
                    ( long "status-sink"
                        <> Options.Applicative.metavar "PATH|URL"
                        <> Options.Applicative.help "Write this host's status document (JSON: host, mode, applied documents per label, the `status` object, the last converge and follow reports) to PATH, atomically, or POST it to an http(s):// URL, after every convergence pass and follow injection and on a timer; `salmon-fleet status DIR` folds a directory of the files."
                    )
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "status-sink-interval"
                    <> Options.Applicative.metavar "SECONDS"
                    <> Options.Applicative.value (StatusSink.defaultInterval `div` 1000000)
                    <> showDefault
                    <> Options.Applicative.help "Seconds between two status sink writes when nothing triggers one."
                )
            <*> optional
                ( strOption
                    ( long "status-sink-host"
                        <> Options.Applicative.metavar "NAME"
                        <> Options.Applicative.help "What the status document's `host` field says (default: this machine's node name, `uname -n`); give one when two loops on one machine write documents, or the node name means nothing to whoever folds them."
                    )
                )
    followOptionsP =
        FollowOptions
            <$> optional
                ( Options.Applicative.option
                    Options.Applicative.auto
                    ( long "follow-base"
                        <> Options.Applicative.metavar "SECONDS"
                        <> Options.Applicative.help ("Seconds between two rounds of fetching the followed labels while rounds succeed (default " <> show (defaultSecs (.schedBase)) <> ").")
                    )
                )
            <*> optional
                ( Options.Applicative.option
                    Options.Applicative.auto
                    ( long "follow-interval"
                        <> Options.Applicative.metavar "SECONDS"
                        <> Options.Applicative.help "Same as --follow-base (the older name)."
                    )
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "follow-factor"
                    <> Options.Applicative.metavar "FACTOR"
                    <> Options.Applicative.value Scheduler.defaultConfig.schedFactor
                    <> showDefault
                    <> Options.Applicative.help "How much slower each consecutive failed round makes the next one."
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "follow-cap"
                    <> Options.Applicative.metavar "SECONDS"
                    <> Options.Applicative.value (defaultSecs (.schedCap))
                    <> showDefault
                    <> Options.Applicative.help "The longest a failing registry is left alone between rounds."
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "follow-jitter"
                    <> Options.Applicative.metavar "FRACTION"
                    <> Options.Applicative.value Scheduler.defaultConfig.schedJitter
                    <> showDefault
                    <> Options.Applicative.help "Every delay is scaled by a uniform draw from [1-j, 1+j], so a fleet does not poll in step."
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "follow-debounce"
                    <> Options.Applicative.metavar "SECONDS"
                    <> Options.Applicative.value (defaultSecs (.schedDebounce))
                    <> showDefault
                    <> Options.Applicative.help "How long the registry must be quiet after a change before the change is applied; 0 applies at once."
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "follow-max-wait"
                    <> Options.Applicative.metavar "SECONDS"
                    <> Options.Applicative.value (defaultSecs (.schedMaxWait))
                    <> showDefault
                    <> Options.Applicative.help "The longest a change waits to be applied while the registry keeps changing."
                )
            <*> optional
                ( strOption
                    ( long "follow-cache"
                        <> Options.Applicative.metavar "DIR"
                        <> Options.Applicative.help "Keep each label's last applied document in DIR, and replay it at startup when the registry cannot be reached (status then says `mode: replay`). Without it a restart against an unreachable registry declares nothing."
                    )
                )
            <*> switch
                ( long "follow-refuse-older"
                    <> Options.Applicative.help "Refuse a fetched document whose `published` timestamp is older than the one already applied for its label (reported as stale, not injected). Documents without `published` are never refused."
                )
            <*> Options.Applicative.option
                Options.Applicative.auto
                ( long "follow-timeout"
                    <> Options.Applicative.metavar "SECONDS"
                    <> Options.Applicative.value (Registry.Http.defaultOptions.optTimeout `div` 1000000)
                    <> showDefault
                    <> Options.Applicative.help "The longest one HTTP fetch may take, for the http(s)://, dns: and bucket registries; longer is a failed round."
                )
            <*> optional
                ( strOption
                    ( long "follow-workdir"
                        <> Options.Applicative.metavar "DIR"
                        <> Options.Applicative.help "Where a git+ registry is checked out (cloned once, then fetched and reset every round). Default: `checkout` under --follow-cache, else a directory under the system temporary directory named by the repository."
                    )
                )
            <*> optional
                ( strOption
                    ( long "follow-bucket-endpoint"
                        <> Options.Applicative.metavar "URL"
                        <> Options.Applicative.help "For an s3:// registry, an S3-compatible endpoint (MinIO, Ceph RGW...) to address the bucket under, path-style: URL/BUCKET/PREFIX/<label>.json. Without it, https://BUCKET.s3.amazonaws.com."
                    )
                )
            <*> many
                ( strOption
                    ( long "follow-key"
                        <> Options.Applicative.metavar "[LABEL=]FILE"
                        <> Options.Applicative.help "A public key (JWK, as `salmon-fleet keygen` writes FILE.pub) every fetched or replayed document must carry a signature by; repeatable, any one suffices. LABEL=FILE makes the key speak for that label only (repeat the flag for more labels); a bare FILE speaks for any label. A signed document must also name the label it was fetched for (`salmon-fleet sign --label`). Without --follow-key documents are not required to be signed (the default). With it, an unsigned document is refused and never applied."
                    )
                )
            <*> switch
                ( long "follow-accept-unlabelled"
                    <> Options.Applicative.help "The migration flag: accept a signed document that names no label (signed before documents named theirs). Off by default; it lets a validly signed document be served at another label's address, so turn it off once documents are re-signed with --label."
                )
    defaultSecs :: (Scheduler.Config -> Int) -> Int
    defaultSecs f = f Scheduler.defaultConfig `div` 1000000
    upP =
        RunUp
            <$> optional
                ( strOption
                    ( long "plan"
                        <> Options.Applicative.metavar "FILE"
                        <> Options.Applicative.help "A `query plan`-emitted Plan: force-skip its excluded nodes."
                    )
                )
            <*> switch
                ( long "force-stale-plan"
                    <> Options.Applicative.help "Proceed even if the plan's directive digest doesn't match stdin."
                )
            <*> reportFormatP
            <*> maintenanceWindowsP "Nodes marked disruptive run only inside these windows (repeatable); outside them they are skipped and reported on stderr. DAY is Mon..Sun; a start later than the end crosses midnight; the zone is a fixed offset, UTC by default."
            <*> overrideWindowP
            <*> optional
                ( option
                    (eitherReader passPhaseOnly)
                    ( long "phase"
                        <> Options.Applicative.metavar "build"
                        <> Options.Applicative.help "Run only the build phase: the nodes marked build (inBuildPhase) and what they depend on; every other node is skipped. Refuses, running nothing, when no node is marked build or when a build node depends on a node marked deploy. Omitted = everything."
                    )
                )
    -- only the build phase runs alone: a deploy phase without its builds is
    -- what the barrier exists to prevent.
    passPhaseOnly t = case PassPhase.parsePassPhase (Text.pack t) of
        Right BuildPhase -> Right BuildPhase
        Right DeployPhase -> Left "only --phase build is supported: the deploy phase does not run without its builds"
        Left err -> Left (Text.unpack err)
    maintenanceWindowsP helpText =
        many
            ( option
                (eitherReader (\t -> either (Left . Text.unpack) (const (Right (Text.pack t))) (Window.parseWindow (Text.pack t))))
                ( long "maintenance-window"
                    <> Options.Applicative.metavar "[DAY:]HH:MM-HH:MM[@UTC|@+HH:MM]"
                    <> Options.Applicative.help helpText
                )
            )
    overrideWindowP =
        switch
            ( long "override-window"
                <> Options.Applicative.help "Run disruptive nodes whatever the maintenance windows say."
            )
    jsonLogP =
        ReportTextJsonLog
            <$> strOption
                ( long "json-log"
                    <> Options.Applicative.metavar "FILE"
                    <> Options.Applicative.help "Keep the text report on the terminal and also append every report, as the one-JSON-object-per-line --json prints, to FILE (created if missing, flushed per report). Not with --json."
                )
    reportFormatP =
        Options.Applicative.flag
            ReportText
            ReportJson
            ( long "json"
                <> Options.Applicative.help "Report as one JSON object per line on stdout (see Salmon.Reporter.Tagged) instead of text."
            )

queryCommandParser :: Parser QueryCommand
queryCommandParser =
    hsubparser $
        mconcat
            [ command "show" (info showP (progDesc "Prints the directive's tree, annotating [selected]/[excluded] nodes."))
            , command "plan" (info planP (progDesc "Emits a Plan (JSON) excluding --exclude matches."))
            , command "extract-directive" (info extractP (progDesc "Prints a plan file's embedded directive (see `query plan --embed-directive`)."))
            ]
  where
    selectionP =
        QuerySelection
            <$> many (Text.pack <$> strOption (long "select" <> Options.Applicative.metavar "PATTERN" <> Options.Applicative.help "May repeat; union. Omitted entirely = everything."))
            <*> many (Text.pack <$> strOption (long "exclude" <> Options.Applicative.metavar "PATTERN" <> Options.Applicative.help "May repeat; union, then subtracted from the selection."))
    showP =
        QueryShow
            <$> selectionP
            <*> (not <$> switch
                    ( long "no-dedupe"
                        <> Options.Applicative.help "Print every path a node is reachable from, instead of only its first-encountered one (dedupe is on by default)."
                    ))
            <*> switch
                ( long "descriptions"
                    <> Options.Applicative.help "Print each node's help text on an indented line (\"  # ...\") below its path."
                )
    planP =
        QueryPlan
            <$> selectionP
            <*> switch
                ( long "embed-directive"
                    <> Options.Applicative.help "Embed the directive itself in the plan, so `query extract-directive` can recover it later without the original directive on hand."
                )
    extractP =
        QueryExtractDirective
            <$> Options.Applicative.strArgument (Options.Applicative.metavar "PLAN-FILE")

instance (FromJSON seed) => FromJSON (Command seed)
instance (ToJSON seed) => ToJSON (Command seed)

instance FromJSON BaseCommand
instance ToJSON BaseCommand

{- | Function to combine a configuration system (based on a seed).
todo: consider adding some non-det when the graph depends not just on a seed but also on reading a variable in the directive
- either at the configure step: then the seed must contain enough to build the ops
- either in the expand phase from the directive
-}
execCommandOrSeed ::
    forall directive seed.
    (ToJSON directive, FromJSON directive, ParseRecord seed) =>
    Reporter (UpDown.Report Extension) ->
    Configure IO seed directive ->
    Track' directive ->
    Command seed ->
    IO ()
execCommandOrSeed = execCommandOrSeedWith Serve.reportText

{- | 'execCommandOrSeed' with a say in how @run serve@ reports its own
loop-level events (as opposed to the per-node events, which go to the same
reporter every other command uses).
-}
execCommandOrSeedWith ::
    forall directive seed.
    (ToJSON directive, FromJSON directive, ParseRecord seed) =>
    Reporter Serve.Report ->
    Reporter (UpDown.Report Extension) ->
    Configure IO seed directive ->
    Track' directive ->
    Command seed ->
    IO ()
execCommandOrSeedWith serveR r = execCommandOrSeedWithRewrites serveR r []

{- | 'execCommandOrSeedWith' with "Salmon.Op.Rewrite" phases registered.

This is how an application asks for something like
'Salmon.Builtin.Nodes.Debian.Package.batchPackages' — a collection of many
small nodes into one bulk invocation — instead of applying an @Op -> Op@ pass
by hand inside its own 'Track''. The difference is not stylistic: a phase runs
after the fold, so it sees every declaration and which way each node is
wanted, neither of which a @directive -> Op@ can see. See "Salmon.Op.Rewrite".

The phases apply to @run up@, @run down@, @run serve@ — the commands that
execute something — and, as of (R4), @run tree@\/@run dag@: both now print
the /computed/ 'Salmon.Op.Dag.Dag' through 'Salmon.Actions.Help.printDagTree'
\/'Salmon.Actions.Dot.printDagCograph' rather than the declared @Cofree
Graph@, so a batched node shows up once, the way it will actually run.
@query@ is the one holdout still printing the /declared/ graph: it resolves
@--select@\/@--exclude@ as path globs (see 'Salmon.Actions.Query.resolveSelectors'),
and a rewritten 'Salmon.Op.Dag.Dag' has refs and edges but no paths for a
pattern to match against — fixing that needs either a path-free renderer with
its own selection language, or resolving a pattern against the declared graph
and translating the result through 'Salmon.Op.Rewrite.membersOf', neither of
which is worth doing speculatively.
-}
execCommandOrSeedWithRewrites ::
    forall directive seed.
    (ToJSON directive, FromJSON directive, ParseRecord seed) =>
    Reporter Serve.Report ->
    Reporter (UpDown.Report Extension) ->
    [Rewrite Extension] ->
    Configure IO seed directive ->
    Track' directive ->
    Command seed ->
    IO ()
execCommandOrSeedWithRewrites serveR r rewrites0 genBase traceBase cmd = do
    case cmd of
        (Run (RunUp Nothing _ fmt wins override phase)) -> do
            result <- listening fmt $ withGraph (runUp (updownFor fmt) (windowsFor wins override) phase Set.empty)
            when (result == Just False) exitFailure
        (Run (RunUp (Just planPath) forceStale fmt wins override phase)) -> do
            result <- listening fmt $ withGraphAndBytes $ \dirBytes op -> do
                planBytes <- LBysteString.readFile planPath
                case eitherDecode planBytes of
                    Left err -> do
                        putStrLn ("failed to json-parse plan " <> planPath <> ": " <> err)
                        exitFailure
                    Right plan -> do
                        let actual = Query.digestBytes dirBytes
                        let expected = Query.planDirectiveDigest plan
                        if actual == expected
                            then runUp (updownFor fmt) (windowsFor wins override) phase (Set.fromList (Query.planExcludedRefs plan)) op
                            else
                                if forceStale
                                    then do
                                        putStrLn $
                                            "warning: plan digest mismatch (plan expects "
                                                <> Text.unpack expected
                                                <> ", this directive hashes to "
                                                <> Text.unpack actual
                                                <> "); proceeding due to --force-stale-plan"
                                        runUp (updownFor fmt) (windowsFor wins override) phase (Set.fromList (Query.planExcludedRefs plan)) op
                                    else do
                                        putStrLn $
                                            "refusing to run stale plan: plan expects digest "
                                                <> Text.unpack expected
                                                <> ", but this directive hashes to "
                                                <> Text.unpack actual
                                        exitFailure
            when (result == Just False) exitFailure
        (Run (RunDown fmt)) -> do
            result <- listening fmt $ withGraph (runDown (updownFor fmt))
            when (result == Just False) exitFailure
        (Run RunTree) -> do
            -- (R4): the computed 'Dag' is what @run up@ would actually walk
            -- once any "Salmon.Op.Rewrite" phases are registered; with none
            -- registered `computed` is the declared graph, still collapsed
            -- to one line per 'Ref' rather than one per path.
            void $ withGraph (\op -> computedTreeDag op >>= Help.printDagTree)
        (Run RunDAG) -> do
            void $ withGraph (\op -> computedTreeDag (injectRemoteSubgraphs 0 op) >>= Dot.printDagCograph)
        (Run (RunServe maxConcurrency startup noAutoConverge fmt followDir labels followOptions listen http eventsRing sinkOptions tcpOptions httpReadOnly wins overrideWins once linger)) -> do
            reportByLine
            -- a directive named on the command line is looked for before
            -- anything is bound, rather than reported as a bad seed by a
            -- loop that then waits for input nobody is going to type
            forM_ startup.startupDirectives $ \path -> do
                there <- doesFileExist path
                when (not there) $ do
                    hPutStrLn stderr ("--up-directive " <> path <> ": no such file")
                    exitFailure
            -- like --http-read-only without a listener: a flag that would do
            -- nothing is refused rather than ignored
            forM_ linger $ \secs -> do
                when (not once) $ do
                    hPutStrLn stderr "--linger needs --once: it is how long a one-pass session stays up after its pass"
                    exitFailure
                when (secs < 0) $ do
                    hPutStrLn stderr "--linger takes a number of seconds, 0 or more"
                    exitFailure
            -- the first pass's 'Serve.ConvergeStop', which is what --once
            -- exits on; later ones (there are none under --once) are ignored
            verdict <- newIORef Nothing
            let verdictR = ReporterM $ \rep -> case Serve.attributed rep of
                    Serve.ConvergeStop ok remaining -> modifyIORef' verdict (<|> Just (ok, remaining))
                    _ -> pure ()
            limit <- traverse Concurrency.newConcurrencyLimit maxConcurrency
            jsonLog <- traverse openJsonLog (jsonLogPath fmt)
            let own = maybe id (\h t -> reportBoth t (Tagged.reportJSONLines h)) jsonLog (taggedFor fmt)
            -- a build without the HTTP server says so before anything is
            -- bound or read, rather than after the socket listener is up
            when (not httpServerAvailable && (isJust http || isJust tcpOptions.tcpBind)) $ do
                hPutStrLn stderr httpServerUnavailable
                exitFailure
            -- the network listener is refused before anything is bound or
            -- read: the flags as a whole, then the token file itself
            tcp <- case validateTcpOptions tcpOptions of
                Left err -> do
                    hPutStrLn stderr (Text.unpack err)
                    exitFailure
                Right t -> pure t
            -- and so is a socket path no unix address can hold, which
            -- would otherwise surface as network's own crash from bind
            forM_ [(flag, path) | (flag, Just path) <- [("--listen", listen), ("--http", http)]] $ \(flag, path) ->
                when (length path >= Socket.unixPathMax) $ do
                    hPutStrLn stderr (flag <> " " <> path <> " is " <> show (length path) <> " characters; a unix socket path holds at most " <> show (Socket.unixPathMax - 1) <> " (pick a shorter one, e.g. under /run or /tmp)")
                    exitFailure
            tlsBinds <- forM (maybe [] pure tcp) $ \t -> do
                token <- Http.readTokenFile t.tcpTokenPath
                case token of
                    Left (Http.TokenFileReadable path) -> do
                        hPutStrLn stderr ("--token-file " <> path <> " is readable by others; a token anyone on the box can read is not one (chmod 600 it)")
                        exitFailure
                    Left (Http.TokenFileEmpty path) -> do
                        hPutStrLn stderr ("--token-file " <> path <> " is empty")
                        exitFailure
                    Right tok -> pure (Http.BindTls (Http.TlsBind t.tcpHost t.tcpPort t.tcpCertFile t.tcpKeyFile tok t.tcpSessionPolicy))
            let binds = [Http.BindUnix path | Just path <- [http]] ++ tlsBinds
            -- a flag that would restrict nothing is refused, like the TLS
            -- files without --http-tcp, rather than silently a no-op
            when (httpReadOnly && null binds) $ do
                hPutStrLn stderr "--http-read-only needs --http PATH or --http-tcp HOST:PORT to apply to; there is no HTTP listener without one"
                exitFailure
            let access = if httpReadOnly then Http.ReadOnly else Http.ReadWrite
            follow <- case (followDir, traverse Follow.mkLabel labels) of
                (Nothing, _) | not (null followOptions.followKeys) -> do
                    hPutStrLn stderr "--follow-key needs a --follow REGISTRY whose documents it verifies"
                    exitFailure
                (Nothing, Right []) -> pure Nothing
                (Nothing, _) -> do
                    hPutStrLn stderr "--label needs a --follow REGISTRY to fetch from"
                    exitFailure
                (Just _, Right []) -> do
                    hPutStrLn stderr "--follow needs at least one --label to fetch"
                    exitFailure
                (Just _, Left err) -> do
                    hPutStrLn stderr (Text.unpack err)
                    exitFailure
                (Just addr, Right lbls) -> do
                    -- the backend is chosen by the shape of the address; see
                    -- "Salmon.Actions.Follow.Registry"
                    address <- case Registry.parseAddress (Text.pack addr) of
                        Left err -> hPutStrLn stderr (Text.unpack err) >> exitFailure
                        Right a -> pure a
                    -- a key that cannot be loaded must not start a loop that
                    -- would then refuse everything, or accept everything
                    keys <- forM followOptions.followKeys $ \spec -> do
                        (scope, path) <- case Signature.parseKeySpec (Text.pack spec) of
                            Left err -> hPutStrLn stderr ("--follow-key " <> spec <> ": " <> Text.unpack err) >> exitFailure
                            Right parsed -> pure parsed
                        loaded <- Signature.readPublicKeyFile path
                        case loaded of
                            Left err -> hPutStrLn stderr ("--follow-key " <> path <> ": " <> Text.unpack err) >> exitFailure
                            Right k -> pure (maybe (Signature.trustsAnyLabel k) (\l -> Signature.trustsOnly l k) scope)
                    let legacy = if followOptions.followAcceptUnlabelled then Signature.AcceptUnlabelled else Signature.RefuseUnlabelled
                        verifier = if null keys then Follow.noVerifier else Signature.signedVerifier legacy keys
                    registry <-
                        Registry.open
                            Registry.defaultOptions
                                { Registry.optHttp = Registry.Http.Options{Registry.Http.optTimeout = seconds followOptions.followTimeout}
                                , Registry.optWorkdir = followOptions.followWorkdir
                                , Registry.optCacheDir = followOptions.followCacheDir
                                , Registry.optBucketEndpoint = followOptions.followBucketEndpoint
                                }
                            address
                    pure $
                        Just
                            Follow.Follow
                                { Follow.followRegistry = registry
                                , Follow.followLabels = lbls
                                , Follow.followSchedule = followSchedule followOptions
                                , Follow.followCache = followOptions.followCacheDir
                                , Follow.followRefuseOlder = followOptions.followRefuseOlder
                                , Follow.followVerify = verifier
                                }
            -- the fetcher's first round is in the inbox before standard
            -- input is even read, so the first convergence is what the
            -- registry says, deterministically; after that both interleave
            -- at line granularity.
            gate <- newEmptyMVar
            -- and before that, the start-up lines: every producer waits
            -- for them to be in the inbox (there are none by default)
            startupGate <- newEmptyMVar
            -- what `fetch` pokes: the fetcher's clock wakes on it; and
            -- what `status` reads: the fetcher's mode
            pk <- Scheduler.newPoke
            modeVar <- Follow.newMode
            appliedVar <- Follow.newApplied
            host <- maybe StatusSink.hostName pure sinkOptions.sinkHost
            let onFetch = Follow.followed pk modeVar appliedVar <$ follow
                sinkConfig path =
                    StatusSink.Config
                        { StatusSink.configPath = path
                        , StatusSink.configInterval = max 1 sinkOptions.sinkInterval * 1000000
                        , StatusSink.configHost = host
                        }
            -- the status sink watches the loop's stream for its triggers,
            -- so what everything below reports through is the loop's own
            -- reporter with the sink beside it; the sink's own complaints
            -- go to the loop's own alone.
            withMaybe sinkOptions.sinkPath (\path -> StatusSink.withSink (sinkConfig path) onFetch own) $ \msink -> do
              let tagged = maybe own (reportBoth own . StatusSink.sinkReporter) msink
                  -- with a socket to talk to, the process must outlive
                  -- whatever started it (`< /dev/null &` is the ordinary way
                  -- to run it), so standard input is read as a named source
                  -- rather than as the loop's 'Serve.Stdin': its end of input
                  -- is a hang-up like any client's and only `quit` — from
                  -- stdin or from a client — ends the loop.
                  stdinP = case (listen, binds) of
                      (Nothing, []) -> Serve.stdinProducer stdin
                      _ -> Serve.handleProducer (Serve.Origin "stdin") stdin
                  producersWith followR more =
                      startupFirst $ case follow of
                          Nothing -> stdinP : more
                          Just f -> Follow.follower followR pk modeVar appliedVar f (putMVar gate ()) : Follow.gated gate stdinP : more
                  startupFirst ps = case startupLines startup of
                      [] -> ps
                      ls -> Serve.linesProducer (Serve.Origin "startup") ls (putMVar startupGate ()) : map (Follow.gated startupGate) ps
              -- the listener's reporters answer each socket client on its own
              -- connection, the HTTP server's answer each request with its
              -- own reports, and both hand everything on to the loop's own,
              -- which stays exactly as `fmt` says.
              withMaybe listen Socket.withUnixListener $ \mlistener ->
                withMaybe (nonEmptyList binds) (\bs -> Http.withHttpServerAccess Events.defaultConfig{Events.configRing = eventsRing} access bs (seedHelpText (parseRecord :: Parser seed)) (maybe (pure Serve.Interactive) Serve.followedMode onFetch)) $ \mserver -> do
                    -- exactly one line, once the listener is up, saying what
                    -- is now reachable from the network and on what terms
                    forM_ tcp $ \t ->
                        hPutStrLn stderr ("serve: exposing HTTP on " <> t.tcpHost <> ":" <> show t.tcpPort <> " with TLS, token from " <> t.tcpTokenPath)
                    let (serveR0, r0) = reportersOver tagged
                        base = case mlistener of
                            Nothing -> (contramap Serve.attributed serveR0, contramap Serve.attributed r0)
                            Just listener -> Socket.listenerReporters listener tagged
                        (serveR', r') = maybe base (`Http.serverReporters` base) mserver
                        observe acc = do
                            traverse_ (`Http.serverObserver` acc) mserver
                            traverse_ (`StatusSink.sinkObserver` acc) msink
                        more = foldMap (pure . Socket.listenerProducer) mlistener <> foldMap (pure . Http.serverProducer) mserver
                        -- what a node says while it works: to the loop's own
                        -- reporter (text or JSON, as `fmt` says), with a
                        -- server to /events on the `output` stream, and with
                        -- a socket to the client whose command it belongs to
                        nodeLogR = maybe id (\srv nr -> reportBoth nr (Http.serverNodeLogReporter srv)) mserver (Tagged.nodeLogStream tagged)
                        saidR = maybe id (\listener sr -> reportBoth sr (Socket.listenerNodeLogReporter listener)) mlistener (contramap Serve.attributed nodeLogR)
                    void $
                        Serve.serveSayingEnding
                            (if once then Serve.AfterFirstPass else Serve.UntilQuit)
                            (if overrideWins then Window.noWindows else Window.windowsAt (rights (map Window.parseWindow wins)))
                            saidR
                            observe
                            rewrites
                            limit
                            (not noAutoConverge)
                            (if once then reportBoth serveR' verdictR else serveR')
                            r'
                            parseSeedArgs
                            genBase
                            traceBase
                            onFetch
                            -- the fetcher's reports also go to /events, as its own stream
                            (producersWith (maybe id (\srv r -> reportBoth r (Http.serverFollowReporter srv)) mserver (Tagged.followStream tagged)) more)
                    -- the loop is over and its producers are gone, so the
                    -- listeners answer reads from the final world and
                    -- refuse commands; they close when these brackets end
                    forM_ linger $ \secs -> when (once && secs > 0) $ do
                        hPutStrLn stderr ("serve: lingering " <> show secs <> "s before exiting")
                        threadDelay (secs * 1000000)
            when once $ do
                outcome <- readIORef verdict
                case outcome of
                    Just (True, 0) -> hPutStrLn stderr "serve: --once: converged"
                    Just (ok, remaining) -> do
                        hPutStrLn stderr ("serve: --once: did not converge (" <> show remaining <> " node(s) left" <> (if ok then ")" else ", including a failure)"))
                        exitFailure
                    Nothing -> do
                        hPutStrLn stderr "serve: --once: the session ended before any convergence pass ran"
                        exitFailure
        (Query (QueryShow (QuerySelection sel exc) dedupe showDescriptions)) -> do
            void $ withGraph $ \op -> do
                let cograph = runIdentity (expand op)
                computed <- computedRewritten op
                let (selected, excluded) = Query.resolveRewrittenSelectors cograph computed sel exc
                Query.printAnnotated cograph selected excluded dedupe showDescriptions
        (Query (QueryPlan (QuerySelection sel exc) embedDirective)) -> do
            void $ withGraphAndBytes $ \dirBytes op -> do
                let cograph = runIdentity (expand op)
                computed <- computedRewritten op
                let (_, excluded) = Query.resolveRewrittenSelectors cograph computed sel exc
                let embedded = if embedDirective then Just (Text.decodeUtf8 (LBysteString.toStrict dirBytes)) else Nothing
                let plan = Query.Plan (Query.digestBytes dirBytes) (Set.toList excluded) exc embedded
                LBysteString.putStr (encode plan)
        (Query (QueryExtractDirective planPath)) -> do
            planBytes <- LBysteString.readFile planPath
            case eitherDecode planBytes of
                Left err -> do
                    putStrLn ("failed to json-parse plan " <> planPath <> ": " <> err)
                    exitFailure
                Right plan ->
                    case Query.planDirective plan of
                        Nothing -> do
                            putStrLn ("plan " <> planPath <> " has no embedded directive (was it created with `query plan --embed-directive`?)")
                            exitFailure
                        Just dirText ->
                            LBysteString.putStr (LBysteString.fromStrict (Text.encodeUtf8 dirText))
        Config seed -> do
            dir <- gen genBase seed
            LBysteString.putStr $ encode dir
  where
    nat = pure . runIdentity

    {- | The one 'Tagged.Tagged' reporter a @run up@\/@run down@\/@run
    serve@ speaks through, by 'ReportFormat': for 'ReportText' it dispatches
    back to the reporters the binary passed in (so nothing about the text
    output changes), for 'ReportJson' it is "Salmon.Reporter.Tagged"'s line
    writer on stdout in their place. The tending loop's own stream never
    reaches here on its own — @serve@ forwards what it keeps of it as
    'Serve.Tended', which the encoding nests — so its slot is 'silent'. The
    fetcher's stream ("Salmon.Actions.Follow") goes through here too, so
    that @--json@ covers it and a status sink can watch it. -}
    taggedFor :: ReportFormat -> Reporter Tagged.Tagged
    taggedFor fmt = case fmt of
        ReportText -> Tagged.reportTexts serveR r silent Follow.reportText (Tagged.reportNodeLogText stdout)
        ReportJson -> Tagged.reportJSONLines stdout
        -- the JSON half needs a file opened, which @serve@ adds beside this
        ReportTextJsonLog _ -> taggedFor ReportText

    jsonLogPath :: ReportFormat -> Maybe FilePath
    jsonLogPath (ReportTextJsonLog path) = Just path
    jsonLogPath _ = Nothing

    {- | @--json-log FILE@, opened for appending before anything is bound or
    read, so a path that cannot be written stops the loop before it starts.
    The handle stays open for the life of the process: every report is
    flushed as it is written ('Tagged.reportJSONLines'), so nothing is lost
    to its not being closed. -}
    openJsonLog :: FilePath -> IO Handle
    openJsonLog path = do
        opened <- try (openFile path AppendMode)
        case opened of
            Left err -> do
                hPutStrLn stderr ("--json-log " <> path <> ": " <> show (err :: IOException))
                exitFailure
            Right h -> pure h

    -- | The tagged reporter split contravariantly into the two the drivers take.
    reportersOver :: Reporter Tagged.Tagged -> (Reporter Serve.Report, Reporter (UpDown.Report Extension))
    reportersOver tagged = (Tagged.serveStream tagged, Tagged.updownStream tagged)

    updownFor :: ReportFormat -> Reporter (UpDown.Report Extension)
    updownFor = snd . reportersOver . taggedFor

    {- | Standard output flushed at every line whatever it is attached to. A
    terminal already is; a file or a pipe is block-buffered by default, so
    the text reports of a pass redirected to a log arrived in 8k chunks,
    long after the nodes they are about. -}
    reportByLine :: IO ()
    reportByLine = hSetBuffering stdout LineBuffering

    {- | A one-shot pass with a listener for what its nodes say while they
    work ("Salmon.Builtin.NodeLog"), printed or JSON-encoded like every
    other report of the pass. -}
    listening :: ReportFormat -> IO a -> IO a
    listening fmt act = do
        reportByLine
        NodeLog.withSink (Tagged.nodeLogStream (taggedFor fmt)) act

    {- | @run up@: everything in this one directive's graph is wanted up, so
    that is the rewrites' 'phaseDesired'. @excluded@ (a plan's skipped
    refs) is what they must not collect: batching a node the operator asked
    to skip would run it anyway, under another node's name.

    Exclusion is a 'UpDown.Gate' rather than 'Query.forceSkip' precisely so it
    composes with collections — a batch is worth running iff some member of
    it is, which is the same 'Rewrite.membersOf' translation @serve@'s gate
    does. The report stream is identical either way: both produce a 'Skip'. -}
    runUp :: Reporter (UpDown.Report Extension) -> UpDown.Gate Extension -> Maybe PassPhase -> Set Ref -> Op -> IO Bool
    runUp r' windowGate phase excluded op = do
        dag <- UpDown.expandDag r' nat op
        let computed = Rewrite.rewrite rewrites (Phase (Set.fromList (Dag.dagOrder dag)) excluded) dag
        let gate = bothGates windowGate (excluding computed excluded)
        case phase of
            Nothing -> UpDown.upDag gate r' (Rewrite.computedDag computed)
            -- only --phase build parses ('passPhaseOnly')
            Just _ -> case buildPhaseGate (Rewrite.computedDag computed) of
                Left err -> do
                    hPutStrLn stderr ("refusing --phase build: " <> err)
                    pure False
                Right (selected, phaseGate) -> do
                    hPutStrLn stderr $
                        "--phase build: running "
                            <> show (Set.size selected)
                            <> " of "
                            <> show (length (Dag.dagOrder (Rewrite.computedDag computed)))
                            <> " nodes, skipping the rest"
                    UpDown.upDag (bothGates phaseGate gate) r' (Rewrite.computedDag computed)

    {- | The registered rewrites, then the two-phase barrier
    ("Salmon.Op.PassPhase"), last so that it reads the tags of the nodes
    the other rewrites left in place. The identity on a graph with no
    node marked build or deploy. -}
    rewrites :: [Rewrite Extension]
    rewrites = rewrites0 <> [PassPhase.barrier]

    {- | @run down@: nothing is wanted up, which is what makes a
    direction-aware rewrite emit a teardown batch here and an install batch
    under @run up@, from the same registered phase. -}
    runDown :: Reporter (UpDown.Report Extension) -> Op -> IO Bool
    runDown r' op = do
        dag <- UpDown.expandDag r' nat op
        let computed = Rewrite.rewrite rewrites (Phase Set.empty Set.empty) dag
        UpDown.downDag UpDown.alwaysRequired r' (Rewrite.computedDag computed)

    {- | (R4): the whole-graph 'Rewritten' `run tree`\/`run dag`\/`query`
    all read from — everything in the declared graph is "desired" and
    nothing is "ignored", the same 'Phase' 'Rewrite.wholeGraph' builds for a
    bare @run down@'s rewrite pass, since none of the three is about one
    direction of travel. Kept as the full 'Rewritten' (not just
    'Rewrite.computedDag') because `query` also needs 'Rewrite.membersOf' —
    see 'Query.resolveRewrittenSelectors'.
    -}
    computedRewritten :: Op -> IO (Rewritten Extension)
    computedRewritten op = do
        dag <- UpDown.expandDag r nat op
        pure (Rewrite.rewrite rewrites (Rewrite.wholeGraph dag) dag)

    computedTreeDag :: Op -> IO (Dag.Dag Extension)
    computedTreeDag op = Rewrite.computedDag <$> computedRewritten op

    -- | Held by the maintenance windows (reported on stderr) or excluded: skipped.
    windowsFor :: [Text] -> Bool -> UpDown.Gate Extension
    windowsFor wins override
        | override = UpDown.alwaysRequired
        | otherwise = Window.windowGate (rights (map Window.parseWindow wins)) $ \act next ->
            hPutStrLn stderr $
                "held by maintenance window until "
                    <> maybe "(never)" show next
                    <> ": "
                    <> show act.shorthand

    bothGates :: UpDown.Gate Extension -> UpDown.Gate Extension -> UpDown.Gate Extension
    bothGates g1 g2 act = do
        a <- g1 act
        case a of
            UpDown.Skippable -> pure UpDown.Skippable
            _ -> g2 act

    excluding :: Rewritten Extension -> Set Ref -> UpDown.Gate Extension
    excluding computed excluded
        | Set.null excluded = UpDown.alwaysRequired
        | otherwise = \act ->
            pure $
                if any (`Set.notMember` excluded) (Set.toList (Rewrite.membersOf computed act.extension.ref))
                    then UpDown.Required
                    else UpDown.Skippable

    -- | 'Nothing' iff the incoming JSON graph failed to parse (in which case @cont@ never ran).
    withGraph :: (Op -> IO a) -> IO (Maybe a)
    withGraph cont = withGraphAndBytes (const cont)

    -- | Like 'withGraph', but also hands the continuation the exact raw
    -- bytes read off stdin — needed to digest the directive itself (see
    -- 'Query.digestBytes'), since re-'encode'ing the decoded value gives no
    -- guarantee of hashing to the same bytes.
    withGraphAndBytes :: (LBysteString.ByteString -> Op -> IO a) -> IO (Maybe a)
    withGraphAndBytes cont = do
        jsonbody <- LBysteString.getContents
        case eitherDecode jsonbody of
            Left err -> do
                putStrLn ("failed to json-parse graph: " <> err)
                pure Nothing
            Right a -> do
                Just <$> cont jsonbody (run traceBase a)

{- | What @run up --phase build@ runs ("Salmon.Op.PassPhase"): the build
nodes and their dependencies, and a 'UpDown.Gate' skipping every other node.
A skipped node releases its dependants, so nothing outside the selection
blocks the build phase; the barrier keeps every deploy node behind the build
nodes anyway. 'Left' (the message, naming nodes by shorthand) when no node is
marked build or a deploy node is among the build nodes' dependencies, in
which case nothing should run.
-}
buildPhaseGate :: Dag.Dag Extension -> Either String (Set Ref, UpDown.Gate Extension)
buildPhaseGate dag =
    case PassPhase.buildSelection dag of
        Left PassPhase.NoBuildNode -> Left "no node is marked build (inBuildPhase)"
        Left (PassPhase.DeployUnderBuild refs) ->
            Left $
                "these nodes are marked deploy but a build node depends on them: "
                    <> Text.unpack (Text.intercalate ", " (fmap nameOf refs))
        Right selected -> Right (selected, gateFor selected)
  where
    nameOf aref = maybe (Text.pack (show aref)) (\act -> act.shorthand) (Dag.representativeOf dag aref)
    gateFor :: Set Ref -> UpDown.Gate Extension
    gateFor selected act =
        pure $
            if Set.member act.extension.ref selected
                then UpDown.Required
                else UpDown.Skippable

-- | Bracket over an optional resource: the continuation gets 'Nothing'
-- when there was nothing to acquire.
withMaybe :: Maybe x -> (x -> (y -> IO r) -> IO r) -> (Maybe y -> IO r) -> IO r
withMaybe Nothing _ k = k Nothing
withMaybe (Just x) with k = with x (k . Just)

-- | 'Nothing' for an empty list, for 'withMaybe' over a list of resources.
nonEmptyList :: [x] -> Maybe [x]
nonEmptyList [] = Nothing
nonEmptyList xs = Just xs

{- | A seed parser's own @--help@ text, as @config --help@ prints it: what
@GET \/help\/seed@ answers, the one non-generic surface the server has.
-}
seedHelpText :: Parser seed -> Text
seedHelpText p =
    case execParserPure defaultPrefs (info (p <**> helper) briefDesc) ["--help"] of
        Failure failure -> Text.pack (fst (renderFailure failure "config"))
        Success _ -> ""
        CompletionInvoked _ -> ""

{- | Runs a seed's own command-line parser over the arguments of one @run
serve@ declaration — i.e. the same words that would follow @config@ on an
actual command line.
-}
parseSeedArgs :: forall seed. (ParseRecord seed) => [String] -> Either Text seed
parseSeedArgs args =
    case execParserPure defaultPrefs (info parseRecord briefDesc) args of
        Success seed -> Right seed
        Failure failure -> Left (Text.pack $ fst $ renderFailure failure "config")
        CompletionInvoked _ -> Left "unexpected shell-completion request"

updownOnReport ::
    Reporter (UpDown.Report Extension) ->
    Reporter Op
updownOnReport r =
    ReporterM $ \op -> void $ UpDown.upTree r nat op
  where
    nat = pure . runIdentity

injectRemoteSubgraphs :: Int -> Op -> Op
injectRemoteSubgraphs lvl orig =
    orig `overlaid` flattenAllRemoteCalls orig

-- | A record for dynamic remote-op.
data RemoteOp = RemoteOp {unRemote :: Op}

flattenAllRemoteCalls :: Op -> Op
flattenAllRemoteCalls root =
    op "remote-call-details" (deps remoteCalls) id
  where
    remoteCalls = concatMap adapt $ collectDynamics root
    adapt :: (Op, [RemoteOp]) -> [Op]
    adapt (orig, remotes) = [unRemote r | r <- remotes]
