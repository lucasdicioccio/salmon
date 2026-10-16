{- | Waiting for a service to answer.

@podman run -d@ (or @systemctl start@, or a detached process) returns long
before the service inside accepts anything: Postgres takes a few seconds to
open its socket, an HTTP server longer to warm up. A dependant applied in
that window fails for no reason of its own. 'waitUntil' is the node that sits
between the two: it does nothing but ask, and its dependants run once the
answer is yes.

The question is a 'Probe' (a command that exits 0, or a TCP port that
accepts) or any @IO Bool@ ('untilIO'), so the same node serves a database in
a container ('pgIsReady', 'podmanExec'), a published port ('ProbeTcp') and an
HTTP health endpoint the caller asks in its own way.

* @check@ asks once. Yes is 'Success' (the node is skipped); no is 'Unknown',
  not 'Failure': the node cannot make the service answer, only wait for it,
  so a "no" is a service on its way up (or down), which is what 'Unknown' is
  for. Under @run serve@ it restarts nothing; the node that runs the service
  is the one whose own check should notice it gone.
* @up@ asks again every 'waitEvery' milliseconds until yes or until
  'waitDeadline' seconds have passed, and then throws 'NotReadyInTime', so
  the node is @Failed@ and its dependants @Blocked@.
* @down@ does nothing: there is no effect to take back.

Each attempt is cut at five seconds (or at what is left of the deadline,
whichever is shorter), and an attempt that throws counts as "no".

The probe is run on the machine salmon runs on, as whoever runs it. Its
command and arguments are rendered in @help@ and @notes@, which are public:
a probe must not carry a secret on its command line.
-}
module Salmon.Builtin.Nodes.Readiness (
    -- * probes
    Probe (..),
    describeProbe,
    runProbe,
    pgIsReady,
    podmanExec,

    -- * the node
    WaitUntil (..),
    untilProbe,
    untilIO,
    waitUntil,
    NotReadyInTime (..),

    -- * the wait, apart from the node
    Clock (..),
    systemClock,
    pollUntil,
    askOnce,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (..), SomeException, bracket, throwIO, try)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import qualified Network.Socket as Net
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Op.Ref (mkRef)

-------------------------------------------------------------------------------

{- | A question asked from the machine salmon runs on, as whoever runs it,
whose answer "yes" means the service does its job.
-}
data Probe
    = -- | a TCP connection to this host and port is accepted (a published port)
      ProbeTcp Text Int
    | -- | this command exits 0. It is run on the /host/; one asking inside
      -- the container is @ProbeCommand "podman" ["exec", NAME, ...]@
      -- (see 'podmanExec')
      ProbeCommand FilePath [Text]
    deriving (Eq, Ord, Show)

describeProbe :: Probe -> Text
describeProbe (ProbeTcp host port) = "tcp " <> host <> ":" <> Text.pack (show port) <> " accepts"
describeProbe (ProbeCommand cmd args) = "`" <> Text.unwords (Text.pack cmd : args) <> "` exits 0"

{- | One attempt, cut at five seconds; anything thrown is "no".

The command's stdin is a pipe closed at once (never the caller's, which
under @run serve@ is the command channel), and its output is discarded.
-}
runProbe :: Probe -> IO Bool
runProbe probe = do
    answer <- timeout 5000000 (try attempt) :: IO (Maybe (Either SomeException Bool))
    pure $ case answer of
        Just (Right ok) -> ok
        _ -> False
  where
    attempt :: IO Bool
    attempt = case probe of
        ProbeCommand cmd args -> do
            (code, _out, _err) <- readCreateProcessWithExitCode (proc cmd (map Text.unpack args)) ""
            pure (code == ExitSuccess)
        ProbeTcp host port -> do
            let hints = Net.defaultHints{Net.addrSocketType = Net.Stream}
            addrs <- Net.getAddrInfo (Just hints) (Just (Text.unpack host)) (Just (show port))
            case addrs of
                [] -> pure False
                addr : _ ->
                    bracket (Net.openSocket addr) Net.close $ \sock ->
                        True <$ Net.connect sock (Net.addrAddress addr)

{- | The host's @pg_isready@ against this host and port: exit 0 only once the
server accepts connections (1 while it is starting, 2 with no answer). It
needs no password, so nothing secret is on the command line.
-}
pgIsReady :: Text -> Int -> Probe
pgIsReady host port =
    ProbeCommand "pg_isready" ["--host", host, "--port", Text.pack (show port), "--quiet"]

{- | This command, run inside the named container with @podman exec@, exits 0:
@podmanExec "db" "pg_isready" ["--quiet"]@ asks the container's own
@pg_isready@, for a database whose port is not published.
-}
podmanExec :: Text -> FilePath -> [Text] -> Probe
podmanExec container cmd args = ProbeCommand "podman" ("exec" : container : Text.pack cmd : args)

-------------------------------------------------------------------------------

-- | What 'waitUntil' waits for, and how long.
data WaitUntil
    = WaitUntil
    { waitKey :: Text
    -- ^ the node's 'Salmon.Op.Ref.Ref' key: two waits with one key are one
    -- node (last writer wins), so name the service, e.g. @"db-pg"@
    , waitAsking :: Text
    -- ^ what is asked, for @help@ and @notes@ (public: no secrets)
    , waitProbe :: IO Bool
    -- ^ one attempt; an exception counts as "no"
    , waitDeadline :: Int
    -- ^ seconds @up@ waits for a yes before it throws. Below one, @up@ asks
    -- exactly once.
    , waitEvery :: Int
    -- ^ milliseconds between two attempts
    }

-- | Wait for this probe, for up to this many seconds, asking four times a second.
untilProbe :: Text -> Probe -> Int -> WaitUntil
untilProbe key probe seconds =
    WaitUntil
        { waitKey = key
        , waitAsking = describeProbe probe
        , waitProbe = runProbe probe
        , waitDeadline = seconds
        , waitEvery = 250
        }

{- | Wait for any @IO Bool@ (an HTTP health endpoint, a file appearing), said
in words for @help@ and @notes@, for up to this many seconds, asking four
times a second.
-}
untilIO :: Text -> Text -> IO Bool -> Int -> WaitUntil
untilIO key asking probe seconds =
    WaitUntil
        { waitKey = key
        , waitAsking = asking
        , waitProbe = probe
        , waitDeadline = seconds
        , waitEvery = 250
        }

-- | The service never answered yes within the deadline.
data NotReadyInTime = NotReadyInTime
    { notReadyKey :: Text
    , notReadyAsking :: Text
    , notReadySeconds :: Int
    }
    deriving (Show)

instance Exception NotReadyInTime where
    displayException e =
        Text.unpack $
            "not ready after "
                <> Text.pack (show e.notReadySeconds)
                <> "s ("
                <> e.notReadyKey
                <> "): "
                <> e.notReadyAsking
                <> " never held"

{- | A node that is satisfied once the service answers; see the module
header for what each of @check@, @up@ and @down@ does. Put it between the
node that starts the service and the ones that use it:

@
waitUntil (untilProbe "db-pg" (pgIsReady "127.0.0.1" 5432) 60)
    \`inject\` runDatabaseContainer
@
-}
waitUntil :: WaitUntil -> Op
waitUntil w =
    op "wait-until" nodeps $ \actions ->
        actions
            { help = "waits until " <> w.waitAsking <> " (up to " <> seconds <> ")"
            , notes =
                [ "asks: " <> w.waitAsking
                , "every " <> Text.pack (show w.waitEvery) <> "ms, for up to " <> seconds <> ", then fails"
                , "check asks once: no answer is Unknown, not Failure"
                , "down does nothing"
                ]
            , ref = mkRef "wait-until" w.waitKey
            , check = do
                ok <- askOnce 5000 w.waitProbe
                pure (if ok then Success else Unknown)
            , up = do
                answered <- pollUntil systemClock (w.waitDeadline * 1000) w.waitEvery w.waitProbe
                if answered
                    then pure ()
                    else throwIO (NotReadyInTime w.waitKey w.waitAsking w.waitDeadline)
            }
  where
    seconds :: Text
    seconds = Text.pack (show w.waitDeadline) <> "s"

-------------------------------------------------------------------------------

-- | Time, in milliseconds, so that 'pollUntil' can be driven without sleeping.
data Clock = Clock
    { clockNow :: IO Int
    -- ^ monotonic milliseconds
    , clockSleep :: Int -> IO ()
    -- ^ sleep this many milliseconds
    }

systemClock :: Clock
systemClock =
    Clock
        { clockNow = (\ns -> fromIntegral (ns `div` 1000000)) <$> getMonotonicTimeNSec
        , clockSleep = \ms -> threadDelay (ms * 1000)
        }

-- | One attempt, cut at this many milliseconds; anything thrown is "no".
askOnce :: Int -> IO Bool -> IO Bool
askOnce ms probe = do
    answer <- timeout (max 1 ms * 1000) (try probe) :: IO (Maybe (Either SomeException Bool))
    pure $ case answer of
        Just (Right ok) -> ok
        _ -> False

{- | Ask until yes (True) or until @budget@ milliseconds have passed since
the first attempt (False), sleeping @every@ milliseconds in between. Always
asks at least once, and every attempt gets at least 100ms, so the last one
may end that much past the budget.
-}
pollUntil :: Clock -> Int -> Int -> IO Bool -> IO Bool
pollUntil clock budget every probe = do
    start <- clock.clockNow
    let go = do
            now <- clock.clockNow
            let remaining = budget - (now - start)
            ok <- askOnce (min 5000 (max 100 remaining)) probe
            if ok
                then pure True
                else do
                    after <- clock.clockNow
                    let left = budget - (after - start)
                    if left <= 0
                        then pure False
                        else clock.clockSleep (max 1 (min every left)) >> go
    go
