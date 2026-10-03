{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A Postgres pair, a pgbouncer in front of it, and a client that keeps
writing while the primary moves — all on qemu guests this binary makes for
itself.

It is the demo of @specs\/pg-switchover.md@, and the same thing
"Test.PgPairDemoSpec" asserts, in a form a person can watch:

> t=$(cabal list-bin salmon-toy-qemu-pg-ha)
> sudo $t config prereqs   | sudo $t run up        # once: three rootfses
> $t config up --primary A --seed B | $t run up    # once: the pair, B built from A
> $t config client --seconds 120    | $t run up &  # a client, writing
> $t config up --primary B | $t run up             # the demo

The client keeps writing across that last line, and says at the end how many
inserts it was told had happened, how many came back an error, and how many
went missing. Here, on the machine this was written on:

> PauseBouncers -> StopMember A -> Promote B -> RepointBouncers B -> Rejoin A -> Done
>
>   inserts acknowledged through the bouncer: 460
>   inserts that came back an error:          0
>   acknowledged rows missing afterwards:     0

@--seed@ is only needed the first time, when B has no cluster to be a copy
of A's: it is safe to leave on (the clone does nothing once the two sides
share a system identifier) and safe to leave off (a pair that is already a
pair does not need it). @--may-discard B@ is the other flag worth trying,
with machine B's guest paused or killed: it is what turns a refusal into a
failover.

= The same thing, kept up and driven live

Each line above is a process that boots nothing twice but still starts from
nothing: it asks every node again, does its one thing and exits. Under
@run serve@ the guests are booted once and stay, and what is wanted of them
is changed a line at a time, by a person or by a program holding the socket:

> $t run serve --http $XDG_RUNTIME_DIR/salmon-toy.http < /dev/null &
> post() { curl -s --unix-socket $XDG_RUNTIME_DIR/salmon-toy.http -X POST --data-binary "$1" http://x/command; }
> post 'up guests'                       # the bridge and three guests, answering ssh
> post 'up up --primary A --seed B'      # the pair on top of them
> post 'up writer'                       # a client that never stops; its lines are /events?stream=output
> post 'up up --primary B'               # the demo ...
> post 'down up --primary A --seed B'    # ... and the declaration it replaced
> post 'up frozen --machine b'           # a fault: B's guest stops mid-sentence
> post 'down frozen --machine b'         # and carries on

@salmon-apps\/scripts\/qemu-pg-ha-serve.sh@ is those lines with names, and
@resources\/postgres-pair.md@ ("Driving it live") says what to expect of each.
Four seeds exist for this and mean little to a one-shot @run up@:

* @guests@ is the machines without the pair, so that the pair can be
  retired and declared again without a boot in between.
* @writer@ is the client as a process the server /holds/ ('managed'), which
  is the only kind that can keep writing across somebody else's command.
* @frozen@ and @partition@ are faults as declarations: @up@ injects one,
  @down@ heals it, and the loop's own state says which are in force.

= What this needs of the host

Two capabilities, granted once, and no root after @prereqs@:

> sudo setcap cap_net_admin+eip $(command -v capsh)
> sudo setcap cap_dac_override,cap_chown,cap_fowner+eip $(command -v qemu-system-x86_64)

plus membership of the @kvm@ group. @specs\/qemu-test-vms-progress.md@ has
the reasoning, including why the first one is on @capsh@ and not on @ip@.

= Why two seeds

@prereqs@ is the only part that needs root: debootstrapping a root
filesystem, regenerating its initrd through a chroot, and handing
@\/etc\/ssh@ to whoever will run the rest. Everything after it — the bridge,
the guests, the pair — is an unprivileged user with two capabilities granted
once (see "Salmon.Builtin.Nodes.Qemu" and @specs\/qemu-test-vms.md@).

Splitting them is not only about privilege. @prereqs@ is slow and rarely
changes; @up@ is the one you re-run, and the whole demo is that re-running it
with one word changed moves a primary under a live client.

= What it is not

A deployment. The secrets here are constants in the source, because the
point is to be able to read the whole thing: a pair in earnest is handed
@.pgpass@ files that somebody else provisioned, which is why
"SreBox.PostgresPair" takes paths and never passwords.

The toy therefore plays both parts, and keeps them apart. 'inventSecrets' is
the somebody else: it leaves files in @\/etc\/salmon-toy-secrets@ on each
guest and does nothing more. Everything a deployment would also have to do
with such files -- install them where the pair reads them, restart pgbouncer
when its auth file changed, give the application a role, a database and a
@pg_hba.conf@ line -- is "SreBox.PostgresPairPrereqs", exactly as a
deployment would use it.
-}
module QemuPgHaToy (
    main,
    Seed (..),
    Side (..),
    Spec (..),
    Host (..),
    Machine (..),
    program,
    thePair,
    machines,
    machineName,
    machineAddr,
    blackholeScript,
    healScript,
    WriterTally (..),
    emptyTally,
    tallyLine,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (toLower)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Options.Applicative (auto, command, execParser, fullDesc, header, helper, info, long, option, optional, progDesc, strOption, subparser, value, (<**>))
import qualified Options.Applicative as Opt
import Options.Generic (ParseRecord (..))
import System.Exit (ExitCode (..))
import Data.Time.Clock (NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory (XdgConfig))
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Posix.User (getEffectiveUserName)
import System.Environment (getEnvironment, lookupEnv)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode, readProcessWithExitCode)

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Daemon as Daemon
import qualified Salmon.Builtin.Nodes.Debian.Debootstrap as Debootstrap
import Salmon.Builtin.Nodes.Debian.Package (Package (..))
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Keys as Keys
import qualified Salmon.Builtin.Nodes.LinuxBridge as LinuxBridge
import qualified Salmon.Builtin.Nodes.Qemu as Qemu
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Reporter (reportPrint, silent)

import qualified SreBox.PostgresPair as Pair
import qualified SreBox.PostgresPairPrereqs as Prereqs

main :: IO ()
main = do
    let desc =
            fullDesc
                <> progDesc "A Postgres pair on qemu guests, with a client that keeps writing while the primary moves"
                <> header "salmon-toy-qemu-pg-ha"
    cmd <- execParser (info parseRecord desc)
    CLI.execCommandOrSeed reportPrint configure program cmd

-------------------------------------------------------------------------------
-- Seed

data Seed
    = SeedPrereqs {seedRoot :: FilePath}
    | SeedUp
        { seedRoot :: FilePath
        , seedPrimary :: Side
        , seedClone :: Maybe Side
        , seedDiscard :: Maybe Side
        }
    | SeedClient {seedRoot :: FilePath, seedSeconds :: Int}
    | -- | the machines alone: what `up` stands on, declared by itself
      SeedGuests {seedRoot :: FilePath}
    | -- | the client as a process `run serve` holds
      SeedWriter {seedRoot :: FilePath}
    | -- | a guest whose CPUs are stopped, for as long as this is declared
      SeedFrozen {seedRoot :: FilePath, seedMachine :: Machine}
    | -- | a guest that cannot reach another, for as long as this is declared
      SeedPartition {seedRoot :: FilePath, seedMachine :: Machine, seedFrom :: Machine}

-- | @--primary A@ is what it looks like.
newtype Side = Side {unSide :: Pair.Side}

instance Read Side where
    readsPrec _ s = case span (`notElem` (" \t" :: String)) s of
        ("A", r) -> [(Side Pair.A, r)]
        ("a", r) -> [(Side Pair.A, r)]
        ("B", r) -> [(Side Pair.B, r)]
        ("b", r) -> [(Side Pair.B, r)]
        _ -> []

instance ParseRecord Seed where
    parseRecord = combo <**> helper
      where
        combo =
            subparser $
                mconcat
                    [ command "prereqs" (info (SeedPrereqs <$> rootOpt) (progDesc "root filesystems for the guests -- needs root, and is the only part that does"))
                    , command
                        "up"
                        ( info
                            (SeedUp <$> rootOpt <*> primaryOpt <*> cloneOpt <*> discardOpt)
                            (progDesc "the bridge, the three guests, and the pair -- re-run with a different --primary to move it")
                        )
                    , command "client" (info (SeedClient <$> rootOpt <*> secondsOpt) (progDesc "write through the bouncer until interrupted, then say what happened"))
                    , command "guests" (info (SeedGuests <$> rootOpt) (progDesc "the bridge and the three guests, without the pair -- under `run serve`, what keeps them booted while the pair is retired and declared again"))
                    , command "writer" (info (SeedWriter <$> rootOpt) (progDesc "(run serve only) a client the server holds: writes through the bouncer for as long as it is declared, and reports on the output stream"))
                    , command
                        "frozen"
                        ( info
                            (SeedFrozen <$> rootOpt <*> machineOpt "machine" "the guest to pause: a, b or bouncer")
                            (progDesc "a fault: this guest's CPUs are stopped (qemu's monitor) -- `down` resumes it")
                        )
                    , command
                        "partition"
                        ( info
                            (SeedPartition <$> rootOpt <*> machineOpt "machine" "the guest that stops answering: a, b or bouncer" <*> machineOpt "from" "the guest it stops answering")
                            (progDesc "a fault: --machine drops everything it would send to --from, which cuts both directions -- `down` heals it")
                        )
                    ]
        machineOpt name what = option (Opt.maybeReader readMachine) (long name <> Opt.help what)
        rootOpt = strOption (long "root" <> Opt.help "where the guests' root filesystems live" <> value "/var/lib/salmon-toy-pg-ha")
        primaryOpt = option auto (long "primary" <> Opt.help "which guest should be the primary: A or B")
        cloneOpt = optional (option auto (long "seed" <> Opt.help "build this side's cluster from the other one (the first time, and after a standby falls too far behind)"))
        discardOpt =
            optional
                ( option
                    auto
                    ( long "may-discard"
                        <> Opt.help "accept losing the writes on this side that the other does not have -- what makes a failover possible, and it must name the side that is not --primary"
                    )
                )
        secondsOpt = option auto (long "seconds" <> Opt.help "how long to keep writing" <> value 120)

-------------------------------------------------------------------------------
-- Spec
--
-- The seed says what the operator wants; the spec says it in terms that
-- need no further looking around. Everything resolved here -- who is
-- running this, where systemd keeps user units, which kernel each rootfs
-- has -- is a fact about /this/ machine, which is what `configure` is for.

data Spec
    = Prereqs
        { specRoot :: FilePath
        , specOwner :: Text
        -- ^ who will run @up@ afterwards, and therefore who must own the
        -- @\/etc\/ssh@ that @up@ writes into.
        }
    | Up
        { specRoot :: FilePath
        , specPair :: Pair.Pair
        , specUser :: Text
        , specUnitDir :: FilePath
        , specRuntimeDir :: FilePath
        -- ^ where qemu's monitor sockets go. Not under @--root@: a unix
        -- socket path may be 107 bytes, and a directory deep enough to
        -- exceed that produces a guest that will not start, with the reason
        -- a long way from the flag that caused it.
        , specBoot :: [(Text, FilePath, FilePath)]
        -- ^ machine name, kernel, initrd: a debootstrapped rootfs names its
        -- kernel after a version nobody can predict.
        }
    | RunClient
        { specPair :: Pair.Pair
        , specSeconds :: Int
        }
    | Guests {specHost :: Host}
    | Writer {specPair :: Pair.Pair}
    | Frozen {specHost :: Host, specMachine :: Machine}
    | Partition {specHost :: Host, specMachine :: Machine, specFrom :: Machine}
    deriving (Generic)

instance FromJSON Spec
instance ToJSON Spec

{- | What booting a guest needs to know about /this/ machine: the same facts
'Up' carries, as one value for the seeds that boot guests and declare no
pair.
-}
data Host = Host
    { hostRoot :: FilePath
    , hostUser :: Text
    , hostUnitDir :: FilePath
    , hostRuntimeDir :: FilePath
    , hostBoot :: [(Text, FilePath, FilePath)]
    }
    deriving (Generic)

instance FromJSON Host
instance ToJSON Host

configure :: Configure IO Seed Spec
configure = Configure $ \seed -> case seed of
    SeedPrereqs root -> Prereqs root <$> unprivilegedUser
    SeedClient root secs -> pure (RunClient (thePair root) secs)
    SeedUp root primary clone discard -> do
        host <- hostFacts root
        pure
            ( Up
                { specRoot = root
                , specPair =
                    (thePair root)
                        { Pair.pair_primary = unSide primary
                        , Pair.pair_seed = fmap unSide clone
                        , Pair.pair_may_discard = fmap unSide discard
                        }
                , specUser = host.hostUser
                , specUnitDir = host.hostUnitDir
                , specRuntimeDir = host.hostRuntimeDir
                , specBoot = host.hostBoot
                }
            )
    SeedGuests root -> Guests <$> hostFacts root
    SeedWriter root -> pure (Writer (thePair root))
    SeedFrozen root m -> (\h -> Frozen h m) <$> hostFacts root
    SeedPartition root m from -> do
        when (m == from) $
            fail "a partition is between two machines: --machine and --from name the same one"
        (\h -> Partition h m from) <$> hostFacts root
  where
    hostFacts root = do
        user <- Text.pack <$> getEffectiveUserName
        unitDir <- getXdgDirectory XdgConfig "systemd/user"
        runtimeDir <- maybe "/tmp" id <$> lookupEnv "XDG_RUNTIME_DIR"
        boots <- traverse (resolveBoot root) machines
        pure (Host root user unitDir runtimeDir boots)

    resolveBoot root m = do
        there <- doesFileExist (rootfsOf root m </> "etc/issue")
        unless there $
            fail (rootfsOf root m <> " does not exist yet: run `prereqs` first, as root")
        (kernel, initrd) <- Qemu.resolveKernelInitrd (rootfsOf root m)
        pure (machineName m, kernel, initrd)

{- | Whoever will run everything after @prereqs@.

@prereqs@ is run with sudo, so the interesting answer is the user who typed
it rather than the root it became -- and if this is real root rather than
sudo, there is nobody to hand the rootfs to and saying so beats guessing.
-}
unprivilegedUser :: IO Text
unprivilegedUser = do
    sudoUser <- lookupEnv "SUDO_USER"
    case sudoUser of
        Just u | not (null u) -> pure (Text.pack u)
        _ -> do
            me <- getEffectiveUserName
            when (me == "root") $
                fail "run `prereqs` with sudo, or pass SUDO_USER: somebody unprivileged has to own the rootfs afterwards"
            pure (Text.pack me)

program :: Track' Spec
program = Track $ \spec -> case spec of
    Prereqs root owner -> prereqs root owner
    Up root pair user unitDir runtimeDir boots -> demo (Host root user unitDir runtimeDir boots) pair
    RunClient pair secs -> clientOp pair secs
    Guests host -> guests host
    Writer pair -> writerOp pair
    Frozen host m -> frozen host m
    Partition host m from -> partition host m from

-------------------------------------------------------------------------------
-- What the toy is made of: three guests on one bridge.

data Machine = MachineA | MachineB | MachineBouncer
    deriving (Eq, Show, Generic)

instance FromJSON Machine
instance ToJSON Machine

-- | @--machine a@, by the name the guest is known by everywhere else.
readMachine :: String -> Maybe Machine
readMachine s = case [m | m <- machines, Text.unpack (machineName m) == map toLower s] of
    (m : _) -> Just m
    [] -> Nothing

machines :: [Machine]
machines = [MachineA, MachineB, MachineBouncer]

machineName :: Machine -> Text
machineName MachineA = "a"
machineName MachineB = "b"
machineName MachineBouncer = "bouncer"

{- | A bridge of its own (@10.98.0.0\/24@), so that this toy and the test
harness's guests (@10.99.0.0\/24@) can be up at the same time without two
machines answering to one address.
-}
machineAddr :: Machine -> Text
machineAddr MachineA = "10.98.0.2"
machineAddr MachineB = "10.98.0.3"
machineAddr MachineBouncer = "10.98.0.4"

-- | Postgres on the two members, pgbouncer on the third. Baked into the
-- rootfs, because these guests have no route to the internet after boot.
machinePackages :: Machine -> Debootstrap.Includes
machinePackages MachineBouncer = [Package "pgbouncer", Package "postgresql-client"]
machinePackages _ = [Package "postgresql", Package "sudo"]

rootfsOf :: FilePath -> Machine -> FilePath
rootfsOf root m = root </> Text.unpack (machineName m) </> "root"

bridgeName :: Text
bridgeName = "salmontoy0"

bridgeCidr :: LinuxBridge.Cidr
bridgeCidr = LinuxBridge.Cidr "10.98.0.1" 24

-- | One CA for all three guests, and one key signed by it.
caKey :: FilePath -> Keys.SSHKeyPair
caKey root = Keys.SSHKeyPair Keys.ED25519 (root </> "keys") "toy-ca"

clientKey :: FilePath -> Keys.SSHKeyPair
clientKey root = Keys.SSHKeyPair Keys.ED25519 (root </> "keys") "toy-client"

-------------------------------------------------------------------------------
-- Demo passwords. Real ones live in files somebody else provisioned, which
-- is why SreBox.PostgresPair takes paths and never passwords.

replPassword, rewindPassword, appPassword, consolePassword :: Text
replPassword = "toy-replication-password"
rewindPassword = "toy-rewind-password"
appPassword = "toy-app-password"
consolePassword = "toy-console-password"

thePair :: FilePath -> Pair.Pair
thePair root =
    Pair.Pair
        { Pair.pair_name = "toy"
        , Pair.pair_a = member MachineA
        , Pair.pair_b = member MachineB
        , Pair.pair_primary = Pair.A
        , Pair.pair_repl_role = "replicator"
        , Pair.pair_repl_passfile = "/etc/postgresql/toy-replication.pgpass"
        , Pair.pair_rewind_role = "rewinder"
        , Pair.pair_rewind_passfile = "/etc/postgresql/toy-rewind.pgpass"
        , -- the guests are rebuilt at fixed addresses, so remembering host
          -- keys across runs would only ever be wrong
          Pair.pair_ssh_known_hosts = Just "/dev/null"
        , Pair.pair_catch_up_seconds = 60
        , Pair.pair_seed = Nothing
        , Pair.pair_may_discard = Nothing
        , Pair.pair_reseed = Nothing
        , Pair.pair_conn_security = Nothing
        , Pair.pair_bouncers = [theBouncer root]
        }
  where
    member m =
        Pair.Member
            { Pair.member_ssh_user = "root"
            , Pair.member_host = machineAddr m
            , Pair.member_cluster = "main"
            , Pair.member_port = 5432
            , Pair.member_ssh_identity = Just (Keys.privateKeyPath (clientKey root))
            , Pair.member_ssh_host = Nothing
            }

theBouncer :: FilePath -> Pair.Bouncer
theBouncer root =
    Pair.Bouncer
        { Pair.bouncer_name = "toy-bouncer"
        , Pair.bouncer_ssh_user = "root"
        , Pair.bouncer_ssh_host = machineAddr MachineBouncer
        , Pair.bouncer_ssh_identity = Just (Keys.privateKeyPath (clientKey root))
        , Pair.bouncer_console_user = "router"
        , Pair.bouncer_console_port = 6432
        , Pair.bouncer_console_passfile = "/etc/pgbouncer/console.pgpass"
        , Pair.bouncer_alias = "app"
        , Pair.bouncer_dbname = "app"
        , Pair.bouncer_more_databases = Nothing
        , Pair.bouncer_routing_path = "/etc/pgbouncer/routing.ini"
        , Pair.bouncer_config_dir = "/etc/pgbouncer"
        , Pair.bouncer_listen_port = 6432
        }

-------------------------------------------------------------------------------
-- prereqs: the only part that needs root.

{- | A root filesystem per guest, able to boot over 9p, with @\/etc\/ssh@
handed to whoever runs the rest.

All three are 'Debootstrap.rootTree' plus 'Debootstrap.ensureVm9pBoot',
which is the pair of nodes @specs\/qemu-test-vms-progress.md@ is about: the
stock initrd cannot mount a 9p root, because the modules for it are modules
and nothing loads them.

The ownership node is the third thing that used to be done by hand. Whatever
boots these guests writes an SSH CA into @\/etc\/ssh@ before qemu starts,
and does that on the host as an ordinary user -- qemu's own capabilities
cover what the /guest/ reaches, not what is written beforehand.
-}
prereqs :: FilePath -> Text -> Op
prereqs root owner =
    op "toy-prereqs" (deps (map rootfsFor machines <> map handOver workingDirs)) $ \actions ->
        actions
            { help = "root filesystems for the toy's guests"
            , notes = ["owned afterwards by " <> owner]
            , ref = mkRef "toy-prereqs" root
            }
  where
    rootfsFor m =
        let tree = Debootstrap.RootTree Debootstrap.Stable (rootfsOf root m) (Debootstrap.vmEssentials <> machinePackages m)
            bootable = Debootstrap.ensureVm9pBoot reportPrint bashTrack tree `inject` Debootstrap.rootTree reportPrint debootstrapTrack tree
         in sshOwnership m `inject` bootable

    -- ownedFile is about a path, not only a file; a directory is what needs
    -- handing over here.
    sshOwnership m =
        FS.ownedFile (FS.FileOwnership (rootfsOf root m </> "etc/ssh") (Just owner) Nothing 0o755)

    {- Everything under this tree that the /unprivileged/ half then writes:
    the keys it mints, the systemd units and monitor sockets each guest
    needs. Not the root filesystems themselves, whose files belong to the
    users inside the guest and are mapped back out by 9p -- chowning those
    would tell the guest that root's files are somebody else's. -}
    workingDirs = (root </> "keys") : [root </> Text.unpack (machineName m) | m <- machines]

    handOver path =
        FS.ownedFile (FS.FileOwnership path (Just owner) Nothing 0o755)
            `inject` FS.dir (FS.Directory path)

-------------------------------------------------------------------------------
-- up: a bridge, three guests, and the pair on top of them.

demo :: Host -> Pair.Pair -> Op
demo host pair =
    canary root pair `inject` foldl inject (Prereqs.pairWithPrereqs reportPrint pair toyPrereqs) provisioned
  where
    root = host.hostRoot

    {- Each machine's prerequisites wait for that machine's secrets, which
    wait for the machine. Said by naming the recipe's node again with one
    more dependency: it is the same node, so the walk merges the two. -}
    provisioned =
        [ Prereqs.memberPrereqs reportPrint pair toyPrereqs Pair.A `inject` secretsOn MachineA
        , Prereqs.memberPrereqs reportPrint pair toyPrereqs Pair.B `inject` secretsOn MachineB
        ]
            <> [ Prereqs.bouncerPrereqs reportPrint pair toyPrereqs b `inject` secretsOn MachineBouncer
               | b <- pair.pair_bouncers
               ]

    secretsOn m = inventSecrets root m `inject` guestUp host m

{- | The machines, and nothing on them: what 'demo' stands on, as a
declaration of its own.

Every node here is one 'demo' declares too, described the same way, so the
two merge rather than collide. What it buys under @run serve@ is a second
holder: with @guests@ declared, retiring the pair takes the pair down and
leaves three machines booted for the next one.
-}
guests :: Host -> Op
guests host =
    op "toy-guests" (deps [guestUp host m | m <- machines]) $ \actions ->
        actions
            { help = "the toy's three guests, booted and answering"
            , ref = mkRef "toy-guests" host.hostRoot
            }

-- | A guest that answers ssh, on top of 'vmUnit'.
guestUp :: Host -> Machine -> Op
guestUp host m = awaitSsh host m `inject` vmUnit host m

-- | The guest as a unit of the user's systemd: its tap, its bridge, and an sshd that trusts the toy's CA.
vmUnit :: Host -> Machine -> Op
vmUnit host m =
    Qemu.setup reportPrint silent systemctlTrack qemuTrack ipTrack (vmConfig host m)
        `inject` trustsTheCa host.hostRoot m
        `inject` LinuxBridge.bridgeAddr silent ipTrack (LinuxBridge.Bridge bridgeName) bridgeCidr

vmConfig :: Host -> Machine -> Qemu.VmConfig
vmConfig (Host root user unitDir runtimeDir boots) m =
    Qemu.VmConfig
        { Qemu.vm_name = "salmon-toy-" <> machineName m
        , Qemu.vm_memory_mb = 512
        , Qemu.vm_smp = 1
        , Qemu.vm_rootfs = rootfsOf root m
        , Qemu.vm_kernel = kernel
        , Qemu.vm_initrd = initrd
        , Qemu.vm_extra_kernel_args =
            ["ip=" <> machineAddr m <> "::" <> bridgeCidr.cidrAddr <> ":255.255.255.0::eth0:off"]
        , Qemu.vm_tap = LinuxBridge.Tap ("toytap-" <> machineName m) (LinuxBridge.Bridge bridgeName) (Just user)
        , Qemu.vm_mac = macOf m
        , Qemu.vm_monitor_socket = runtimeDir </> ("salmon-toy-" <> Text.unpack (machineName m) <> ".sock")
        , Qemu.vm_enable_kvm = True
        , Qemu.vm_user = user
        , Qemu.vm_group = user
        , Qemu.vm_working_dir = root </> Text.unpack (machineName m)
        , Qemu.vm_systemd_scope = Systemd.User
        , Qemu.vm_unit_dir = unitDir
        }
  where
    (kernel, initrd) = case [(k, i) | (n, k, i) <- boots, n == machineName m] of
        ((k, i) : _) -> (k, i)
        [] -> error ("no kernel resolved for " <> Text.unpack (machineName m))

-- | Fixed, because the guests are: three machines, three addresses.
macOf :: Machine -> Text
macOf MachineA = "52:54:00:70:a1:01"
macOf MachineB = "52:54:00:70:a1:02"
macOf MachineBouncer = "52:54:00:70:a1:03"

{- | Makes the guest's sshd trust this toy's CA -- /one/ CA for all three, so
a single key reaches every machine, which is what a deployment looks like.

The CA's public half is read when this runs rather than when the graph is
declared, because the node that generates it is a dependency of this one:
declaring a file's contents from a key that does not exist yet reads an
empty string and writes it, and an empty @TrustedUserCAKeys@ locks everybody
out of a guest that otherwise looks fine.
-}
trustsTheCa :: FilePath -> Machine -> Op
trustsTheCa root m =
    op "toy-ssh-trust" (deps [signed]) $ \actions ->
        actions
            { help = "sshd on " <> machineName m <> " trusts the toy CA"
            , ref = mkRef "toy-ssh-trust" (machineName m)
            , up = do
                pub <- readFile (Keys.publicKeyPath (caKey root))
                createDirectoryIfMissing True (rootfsOf root m </> "etc/ssh/sshd_config.d")
                writeFile (rootfsOf root m </> "etc/ssh/ca.pub") pub
                writeFile
                    (rootfsOf root m </> "etc/ssh/sshd_config.d/99-salmon-toy.conf")
                    (unlines ["TrustedUserCAKeys /etc/ssh/ca.pub", "PasswordAuthentication no"])
            }
  where
    signed =
        Keys.signKey silent keygenTrack (Keys.SSHCertificateAuthority (caKey root)) (Keys.KeyIdentifier "salmon-toy") [Keys.Principal "root"] (clientKey root)
            `inject` Keys.sshKey silent keygenTrack (clientKey root)
            `inject` Keys.sshKey silent keygenTrack (caKey root)

{- | A guest is not up when qemu is running; it is up when it answers.

A guest that is /paused/ ('frozen') will not answer however long anyone
waits, and waiting is not free under @run serve@: every command stands the
tending machines down first, and standing down waits for an @up@ in flight.
So a paused guest fails this at once, saying why, rather than holding the
line that would resume it behind two minutes of polling.
-}
awaitSsh :: Host -> Machine -> Op
awaitSsh host m =
    op "toy-guest-up" nodeps $ \actions ->
        actions
            { help = machineName m <> " answers ssh"
            , ref = mkRef "toy-guest-up" (machineName m)
            , check = do
                paused <- isPaused
                if paused
                    then pure (Failure (machineName m <> " is paused"))
                    else do
                        (code, _, _) <- sshToGuest root m "true"
                        pure (if code == ExitSuccess then Success else Failure (machineName m <> " is not answering yet"))
            , up = poll (60 :: Int)
            }
  where
    root = host.hostRoot
    isPaused = (== Just Qemu.Paused) <$> Qemu.runState (vmConfig host m).vm_monitor_socket
    poll 0 = fail (Text.unpack (machineName m) <> " never answered ssh")
    poll n = do
        paused <- isPaused
        when paused $
            fail (Text.unpack (machineName m) <> " is paused (see `frozen`): it will not answer until it is resumed")
        (code, _, _) <- sshToGuest root m "true"
        unless (code == ExitSuccess) (threadDelay 2000000 >> poll (n - 1))

-------------------------------------------------------------------------------
-- Faults, as declarations: `up` injects one, `down` heals it.

{- | This guest's CPUs are stopped, through qemu's monitor.

The machine its peers see is one that went silent without closing anything:
no FIN, no RST, a replication connection that is simply never written to
again. Resumed, it carries on from the instruction it stopped at -- which,
for a primary whose standby was promoted in the meantime, is the only way
this toy has of producing two primaries.

It depends on the unit and not on the guest answering: the monitor is
qemu's, and a guest that is about to be paused does not need to have booted.
-}
frozen :: Host -> Machine -> Op
frozen host m =
    op "toy-frozen" (deps [vmUnit host m]) $ \actions ->
        actions
            { help = machineName m <> " is paused"
            , notes = ["a fault, for as long as it is declared: retiring it resumes the guest"]
            , ref = mkRef "toy-frozen" (machineName m)
            , check = do
                st <- Qemu.runState sock
                pure $ case st of
                    Just Qemu.Paused -> Success
                    Just _ -> Failure (machineName m <> " is running")
                    Nothing -> Unknown
            , up = do
                ok <- Qemu.pause sock
                unless ok (fail ("could not reach the monitor of " <> Text.unpack (machineName m) <> " at " <> sock))
            , -- a monitor nobody answers is a guest that is not running, and
              -- a guest that is not running is not paused: nothing to undo,
              -- and failing here would hold up the unit's own teardown.
              down = void (Qemu.resume sock)
            }
  where
    sock = (vmConfig host m).vm_monitor_socket

{- | @machine@ sends nothing to @from@: a blackhole route for that one
address, which cuts both directions since no answer leaves either.

A route rather than a firewall rule because @ip@ is in every rootfs and
@nft@ is not, and because @ip route replace@ is a set: applying it twice is
applying it once. It is not persistent, so a guest that reboots comes back
healed -- and this node's check is what says so.

The host is not a thing a guest can be cut off from here: the command that
would heal it has to travel the path it cut.
-}
partition :: Host -> Machine -> Machine -> Op
partition host m from =
    op "toy-partition" (deps [guestUp host m]) $ \actions ->
        actions
            { help = machineName m <> " cannot reach " <> machineName from
            , notes = ["a fault, for as long as it is declared: retiring it removes the route"]
            , ref = mkRef "toy-partition" (machineName m, machineName from)
            , check = do
                (code, _, _) <- sshToGuest root m (blackholeProbe from)
                pure $ case code of
                    ExitSuccess -> Success
                    ExitFailure 255 -> Unknown
                    ExitFailure _ -> Failure (machineName m <> " can reach " <> machineName from)
            , up = do
                (code, out, err) <- sshToGuest root m (blackholeScript from)
                unless (code == ExitSuccess) $
                    fail ("cutting " <> Text.unpack (machineName m) <> " off: " <> out <> err)
            , down = do
                (code, out, err) <- sshToGuest root m (healScript from)
                -- 255 is ssh not connecting: a machine that is gone has no
                -- routes left to remove, and must not block its own teardown.
                unless (code == ExitSuccess || code == ExitFailure 255) $
                    fail ("healing " <> Text.unpack (machineName m) <> ": " <> out <> err)
            }
  where
    root = host.hostRoot

blackholeScript, healScript, blackholeProbe :: Machine -> String
blackholeScript from = "ip route replace blackhole " <> Text.unpack (machineAddr from) <> "/32"
healScript from = "ip route del blackhole " <> Text.unpack (machineAddr from) <> "/32 2>/dev/null || true"
blackholeProbe from = "ip -o route show type blackhole | grep -qw " <> Text.unpack (machineAddr from)

-------------------------------------------------------------------------------
-- The secrets a deployment would have provisioned, and this toy invents.

-- | Where the toy's "somebody else" leaves secrets on a guest.
secretsDir :: FilePath
secretsDir = "/etc/salmon-toy-secrets"

{- | What the pair needs first, in a deployment's own terms: every secret is
a file in 'secretsDir' on the machine it is for, and the recipe puts it
where the pair reads it. The packages are baked into the rootfs (these
guests have no route to a mirror), which the recipe finds out for itself and
so never reaches for @apt-get@.
-}
toyPrereqs :: Prereqs.Prereqs
toyPrereqs =
    Prereqs.defaultPrereqs
        { Prereqs.prereq_repl_passfile = Prereqs.postgresOwned (secretsDir </> "replication.pgpass")
        , Prereqs.prereq_rewind_passfile = Prereqs.postgresOwned (secretsDir </> "rewind.pgpass")
        , Prereqs.prereq_console_passfile = Prereqs.postgresOwned (secretsDir </> "console.pgpass")
        , Prereqs.prereq_userlist = (Prereqs.postgresOwned (secretsDir </> "userlist.txt")){Prereqs.secret_mode = "0644"}
        , Prereqs.prereq_applications =
            [ -- the application's own access, which the pair knows nothing
              -- about: it routes a database, it does not own one.
              Prereqs.Application
                { Prereqs.app_role = "app"
                , Prereqs.app_database = "app"
                , Prereqs.app_passfile = secretsDir </> "app.pgpass"
                , Prereqs.app_clients = [machineAddr MachineBouncer]
                , Prereqs.app_hba_method = "md5"
                }
            ]
        }

{- | The toy as the somebody else: files in 'secretsDir', and nothing done
with them. This is the only node that knows a password.
-}
inventSecrets :: FilePath -> Machine -> Op
inventSecrets root m =
    op "toy-secret" nodeps $ \actions ->
        actions
            { help = "passwords on " <> machineName m
            , notes = ["constants in this binary's source, which is the difference between a toy and a deployment"]
            , ref = mkRef "toy-secret" (machineName m)
            , up = do
                (code, out, err) <- sshToGuest root m (secretScript m)
                unless (code == ExitSuccess) $
                    fail ("provisioning " <> Text.unpack (machineName m) <> ": " <> out <> err)
            }

secretScript :: Machine -> String
secretScript m =
    unlines $
        [ "set -e"
        , "umask 077"
        , "mkdir -p " <> secretsDir
        ]
            <> case m of
                MachineBouncer ->
                    [ -- postgres's own scheme: md5, then the hex digest of
                      -- password and user run together.
                      "md5() { printf 'md5%s' \"$(printf '%s%s' \"$2\" \"$1\" | md5sum | cut -d' ' -f1)\"; }"
                    , "{"
                    , "  printf '\"router\" \"%s\"\\n' \"$(md5 router " <> Text.unpack consolePassword <> ")\""
                    , "  printf '\"app\" \"%s\"\\n' \"$(md5 app " <> Text.unpack appPassword <> ")\""
                    , "} > " <> secretsDir </> "userlist.txt"
                    , pgpass "console.pgpass" "router" consolePassword
                    ]
                _ ->
                    [ pgpass "replication.pgpass" "replicator" replPassword
                    , pgpass "rewind.pgpass" "rewinder" rewindPassword
                    , pgpass "app.pgpass" "app" appPassword
                    ]
  where
    pgpass file role pwd =
        "printf '*:*:*:" <> role <> ":" <> Text.unpack pwd <> "\\n' > " <> secretsDir </> file

{- | The table the client writes to. The toy's own, and not a prerequisite
of anything: it is made last, on whichever member is the primary by then.
-}
canary :: FilePath -> Pair.Pair -> Op
canary root pair =
    op "toy-canary" nodeps $ \actions ->
        actions
            { help = "the table the client writes to"
            , ref = mkRef "toy-canary" pair.pair_name
            , up = forM_ [MachineA, MachineB] $ \m -> do
                (code, out, err) <- sshToGuest root m canaryScript
                unless (code == ExitSuccess) $
                    fail ("the canary table on " <> Text.unpack (machineName m) <> ": " <> out <> err)
            }

canaryScript :: String
canaryScript =
    unlines
        [ "set -e"
        , "export LANG=C LC_ALL=C"
        , "if [ \"$(sudo -u postgres psql -tAXc 'SELECT pg_is_in_recovery()')\" = f ]; then"
        , "  sudo -u postgres psql -d app -tAXc 'CREATE TABLE IF NOT EXISTS canary (n int primary key, at timestamptz default now())'"
        , "  sudo -u postgres psql -d app -tAXc 'GRANT ALL ON canary TO app'"
        , "fi"
        ]

-------------------------------------------------------------------------------
-- The client: on the host, through the bouncer, like anybody else's.

{- | Writes one row a fifth of a second until the time runs out, then says
what happened.

It runs here rather than on a guest because that is where a client is: the
bouncer is reachable over the toy's bridge, so this is an ordinary libpq
connection from an ordinary machine. The three numbers at the end are the
whole claim -- most of all the middle one, which is what a bouncer paused
and reloaded instead of restarted buys.
-}
clientOp :: Pair.Pair -> Int -> Op
clientOp pair seconds =
    op "toy-client" nodeps $ \actions ->
        actions
            { help = Text.pack ("writes through the bouncer for " <> show seconds <> "s")
            , ref = mkRef "toy-client" ("toy" :: Text)
            , up = runClient pair seconds
            }

runClient :: Pair.Pair -> Int -> IO ()
runClient pair seconds = do
    -- the canary table outlives a run, and its key is the row number, so
    -- this one starts where the last one stopped rather than colliding with
    -- it and calling that an outage.
    start <- highestSoFar bouncer
    deadline <- addUTCTime (fromIntegral seconds) <$> getCurrentTime
    putStrLn ("writing through " <> Text.unpack host <> ":" <> show port <> " for " <> show seconds <> "s; move the primary while this runs")
    (ok, failed, firstError) <- go deadline start [] [] Nothing
    putStrLn ""
    present <- rowsPresent pair ok
    putStrLn ("  inserts acknowledged through the bouncer: " <> show (length ok))
    putStrLn ("  inserts that came back an error:          " <> show (length failed))
    putStrLn ("  acknowledged rows missing afterwards:     " <> show (length ok - present))
    forM_ firstError $ \e -> putStrLn ("  the first error was: " <> takeWhile (/= '\n') e)
  where
    bouncer = case pair.pair_bouncers of
        (b : _) -> b
        [] -> error "the toy always declares a bouncer"
    host = bouncer.bouncer_ssh_host
    port = bouncer.bouncer_listen_port

    go deadline n ok failed firstError = do
        now <- getCurrentTime
        if now >= deadline
            then pure (reverse ok, reverse failed, firstError)
            else do
                let i = n + 1
                (acked, err) <- insertOne bouncer i
                putStr (if acked then "." else "!")
                hFlush stdout
                threadDelay 200000
                go
                    deadline
                    i
                    (if acked then i : ok else ok)
                    (if acked then failed else i : failed)
                    (if acked then firstError else maybe (Just err) Just firstError)

{- | The client as something @run serve@ holds: it writes for as long as it
is declared, and says how it is going on the node's output.

'clientOp' cannot be this. Its @up@ returns when its time is up, and a pass
waits for it -- so under @serve@ the line that would move the primary sits
in the inbox until the client it was meant to move it under has finished. A
'managed' action is the other kind: the loop keeps it running across
commands and across passes, and every line it writes is an @output@ event
(@GET \/events?stream=output@) beside the node's own ring in @\/dag@.

It says something every 'tallyEvery' seconds, and at once when inserts
start failing or stop failing, since those two lines are what a switchover
looks like from a client. There is no final count, because there is no end.
-}
writerOp :: Pair.Pair -> Op
writerOp pair =
    op "toy-writer" nodeps $ \actions ->
        actions
            { help = "keeps writing through the bouncer, and says how it is going"
            , notes = ["held by `run serve`; its lines are the output stream"]
            , ref = mkRef "toy-writer" pair.pair_name
            , managed = Just (runWriter pair)
            , up = throwIO (Daemon.NeedsSupervisor "the toy's writer")
            , -- nothing holds it once its machine is gone
              down = pure ()
            }

-- | What the writer has seen so far.
data WriterTally = WriterTally
    { tallyAcked :: !Int
    , tallyFailed :: !Int
    , tallyMissing :: !(Maybe Int)
    -- ^ acknowledged rows not there at the last audit; 'Nothing' when the audit could not be run
    , tallyLast :: !Int
    -- ^ the last row number tried
    }
    deriving (Eq, Show)

emptyTally :: WriterTally
emptyTally = WriterTally 0 0 (Just 0) 0

-- | The three numbers the one-shot client prints at the end, as one line.
tallyLine :: WriterTally -> Text
tallyLine t =
    Text.pack $
        "acknowledged "
            <> show t.tallyAcked
            <> ", errors "
            <> show t.tallyFailed
            <> ", acknowledged rows missing "
            <> maybe "unknown (the audit could not read the table)" show t.tallyMissing
            <> " (last row "
            <> show t.tallyLast
            <> ")"

-- | Seconds between two tally lines. By the clock and not by the insert: a failing insert can take seconds to fail.
tallyEvery :: NominalDiffTime
tallyEvery = 5

runWriter :: Pair.Pair -> Output -> IO ExitCode
runWriter pair out = do
    start <- highestSoFar bouncer
    out (Text.pack ("writing through " <> Text.unpack bouncer.bouncer_ssh_host <> ":" <> show bouncer.bouncer_listen_port <> ", from row " <> show (start + 1)))
    now <- getCurrentTime
    go start Set.empty emptyTally{tallyLast = start} True now
  where
    bouncer = case pair.pair_bouncers of
        (b : _) -> b
        [] -> error "the toy always declares a bouncer"

    go n acked tally healthy lastLine = do
        let i = n + 1
        (ok, err) <- insertOne bouncer i
        let acked' = if ok then Set.insert i acked else acked
            tally' =
                tally
                    { tallyAcked = tally.tallyAcked + (if ok then 1 else 0)
                    , tallyFailed = tally.tallyFailed + (if ok then 0 else 1)
                    , tallyLast = i
                    }
        when (healthy && not ok) $
            out (Text.pack ("insert " <> show i <> " failed: " <> takeWhile (/= '\n') err))
        when (not healthy && ok) $
            out (Text.pack ("insert " <> show i <> " acknowledged: writing again"))
        -- a table that came back with rows this run never wrote (an earlier
        -- run's, behind a bouncer that was not there when this one started)
        -- would otherwise be one duplicate-key error per such row.
        next <- if ok then pure i else max i <$> highestSoFar bouncer
        now <- getCurrentTime
        (tally'', lastLine') <-
            if diffUTCTime now lastLine >= tallyEvery
                then do
                    audited <- audit acked' tally'
                    out (tallyLine audited)
                    pure (audited, now)
                else pure (tally', lastLine)
        threadDelay 200000
        go next acked' tally'' ok lastLine'

    audit acked tally = do
        present <- rowsAmong bouncer
        pure tally{tallyMissing = fmap (\there -> Set.size (Set.difference acked there)) present}

-- | The rows that are there, or 'Nothing' if nobody could be asked.
rowsAmong :: Pair.Bouncer -> IO (Maybe (Set.Set Int))
rowsAmong b = do
    (code, out, _) <- psqlThroughBouncer b ["-tAXc", "SELECT n FROM canary"]
    pure $
        if code /= ExitSuccess
            then Nothing
            else Just (Set.fromList [n | w <- words out, [(n, _)] <- [reads w]])

insertOne :: Pair.Bouncer -> Int -> IO (Bool, String)
insertOne b i = do
    (code, _, err) <-
        psqlThroughBouncer
            b
            [ "-v"
            , "ON_ERROR_STOP=1"
            , "-tAXc"
            , "INSERT INTO canary (n) VALUES (" <> show i <> ")"
            ]
    pure (code == ExitSuccess, err)

{- | @psql@ against the bouncer, as the application.

The password is in this process's environment rather than in a file only
because the whole toy's passwords are in its source; a client in earnest
reads a @.pgpass@, which is also what the recipe's own nodes take.
-}
psqlThroughBouncer :: Pair.Bouncer -> [String] -> IO (ExitCode, String, String)
psqlThroughBouncer b args = do
    environment <- getEnvironment
    let cp =
            (proc "psql" (connArgs <> args))
                { env =
                    Just
                        ( ("PGPASSWORD", Text.unpack appPassword)
                            -- a bouncer whose machine is paused or gone answers
                            -- nothing at all, and libpq would wait out the
                            -- kernel's two minutes of SYN retries per insert.
                            : ("PGCONNECT_TIMEOUT", "3")
                            : filter ((`notElem` ["PGPASSWORD", "PGCONNECT_TIMEOUT"]) . fst) environment
                        )
                }
    readCreateProcessWithExitCode cp ""
  where
    connArgs =
            [ "-h"
            , Text.unpack b.bouncer_ssh_host
            , "-p"
            , show b.bouncer_listen_port
            , "-U"
            , "app"
            , "-d"
            , Text.unpack b.bouncer_alias
            ]

-- | The highest row already there, so a second run does not collide with a first.
highestSoFar :: Pair.Bouncer -> IO Int
highestSoFar b = do
    (code, out, _) <- psqlThroughBouncer b ["-tAXc", "SELECT coalesce(max(n), 0) FROM canary"]
    pure $ case (code, reads (takeWhile (/= '\n') out)) of
        (ExitSuccess, [(n, _)]) -> n
        _ -> 0

-- | How many of the acknowledged inserts are actually there afterwards.
rowsPresent :: Pair.Pair -> [Int] -> IO Int
rowsPresent pair ok = do
    (code, out, _) <- psqlThroughBouncer bouncer ["-tAXc", "SELECT n FROM canary"]
    pure $
        if code /= ExitSuccess
            then 0
            else length (filter (`elem` map show ok) (words out))
  where
    bouncer = case pair.pair_bouncers of
        (b : _) -> b
        [] -> error "the toy always declares a bouncer"

-------------------------------------------------------------------------------

sshToGuest :: FilePath -> Machine -> String -> IO (ExitCode, String, String)
sshToGuest root m script =
    readProcessWithExitCode
        "ssh"
        [ "-o"
        , "BatchMode=yes"
        , "-o"
        , "StrictHostKeyChecking=no"
        , "-o"
        , "UserKnownHostsFile=/dev/null"
        , "-o"
        , "IdentitiesOnly=yes"
        , "-o"
        , "ConnectTimeout=5"
        , "-i"
        , Keys.privateKeyPath (clientKey root)
        , "root@" <> Text.unpack (machineAddr m)
        , "bash"
        , "-c"
        , shQuote script
        ]
        ""
  where
    shQuote x = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) x <> "'"

-- | The binaries this toy assumes are installed; it provisions none of them.
bashTrack :: Track' (Binary.Binary "bash")
bashTrack = ignoreTrack

debootstrapTrack :: Track' (Binary.Binary "debootstrap")
debootstrapTrack = ignoreTrack

systemctlTrack :: Track' (Binary.Binary "systemctl")
systemctlTrack = ignoreTrack

qemuTrack :: Track' (Binary.Binary "qemu-system-x86_64")
qemuTrack = ignoreTrack

ipTrack :: Track' (Binary.Binary "ip")
ipTrack = ignoreTrack

keygenTrack :: Track' (Binary.Binary "ssh-keygen")
keygenTrack = ignoreTrack
