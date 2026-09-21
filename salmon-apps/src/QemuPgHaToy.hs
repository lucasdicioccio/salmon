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
-}
module QemuPgHaToy (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Options.Applicative (auto, command, execParser, fullDesc, header, helper, info, long, option, optional, progDesc, strOption, subparser, value, (<**>))
import qualified Options.Applicative as Opt
import Options.Generic (ParseRecord (..))
import System.Exit (ExitCode (..))
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory (XdgConfig))
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Posix.User (getEffectiveUserName)
import System.Environment (getEnvironment, lookupEnv)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode, readProcessWithExitCode)

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Binary as Binary
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
                    ]
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
    deriving (Generic)

instance FromJSON Spec
instance ToJSON Spec

configure :: Configure IO Seed Spec
configure = Configure $ \seed -> case seed of
    SeedPrereqs root -> Prereqs root <$> unprivilegedUser
    SeedClient root secs -> pure (RunClient (thePair root) secs)
    SeedUp root primary clone discard -> do
        user <- Text.pack <$> getEffectiveUserName
        unitDir <- getXdgDirectory XdgConfig "systemd/user"
        runtimeDir <- maybe "/tmp" id <$> lookupEnv "XDG_RUNTIME_DIR"
        boots <- traverse (resolveBoot root) machines
        pure
            ( Up
                { specRoot = root
                , specPair =
                    (thePair root)
                        { Pair.pair_primary = unSide primary
                        , Pair.pair_seed = fmap unSide clone
                        , Pair.pair_may_discard = fmap unSide discard
                        }
                , specUser = user
                , specUnitDir = unitDir
                , specRuntimeDir = runtimeDir
                , specBoot = boots
                }
            )
  where
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
    Up root pair user unitDir runtimeDir boots -> demo root pair user unitDir runtimeDir boots
    RunClient pair secs -> clientOp pair secs

-------------------------------------------------------------------------------
-- What the toy is made of: three guests on one bridge.

data Machine = MachineA | MachineB | MachineBouncer
    deriving (Eq, Show)

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

demo :: FilePath -> Pair.Pair -> Text -> FilePath -> FilePath -> [(Text, FilePath, FilePath)] -> Op
demo root pair user unitDir runtimeDir boots =
    Pair.pairOp reportPrint pair `inject` secrets
  where
    secrets =
        op "toy-secrets" (deps (map secretsOn machines)) $ \actions ->
            actions
                { help = "the passwords a deployment would have provisioned"
                , ref = mkRef "toy-secrets" root
                }

    secretsOn m = provisionSecrets root m `inject` reachable root m

    reachable r m = guestUp r m

    guestUp r m =
        awaitSsh r m
            `inject` ( Qemu.setup reportPrint silent systemctlTrack qemuTrack ipTrack (vmConfig r m user unitDir runtimeDir boots)
                        `inject` trustsTheCa r m
                        `inject` LinuxBridge.bridgeAddr silent ipTrack (LinuxBridge.Bridge bridgeName) bridgeCidr
                     )

vmConfig :: FilePath -> Machine -> Text -> FilePath -> FilePath -> [(Text, FilePath, FilePath)] -> Qemu.VmConfig
vmConfig root m user unitDir runtimeDir boots =
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

-- | A guest is not up when qemu is running; it is up when it answers.
awaitSsh :: FilePath -> Machine -> Op
awaitSsh root m =
    op "toy-guest-up" nodeps $ \actions ->
        actions
            { help = machineName m <> " answers ssh"
            , ref = mkRef "toy-guest-up" (machineName m)
            , check = do
                (code, _, _) <- sshToGuest root m "true"
                pure (if code == ExitSuccess then Success else Failure (machineName m <> " is not answering yet"))
            , up = poll (60 :: Int)
            }
  where
    poll 0 = fail (Text.unpack (machineName m) <> " never answered ssh")
    poll n = do
        (code, _, _) <- sshToGuest root m "true"
        unless (code == ExitSuccess) (threadDelay 2000000 >> poll (n - 1))

-------------------------------------------------------------------------------
-- The secrets a deployment would have provisioned, and this toy invents.

provisionSecrets :: FilePath -> Machine -> Op
provisionSecrets root m =
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
secretScript MachineBouncer =
    unlines
        [ "set -e"
        , "mkdir -p /etc/pgbouncer"
        , -- postgres's own scheme: md5, then the hex digest of password and
          -- user run together.
          "md5() { printf 'md5%s' \"$(printf '%s%s' \"$2\" \"$1\" | md5sum | cut -d' ' -f1)\"; }"
        , "{"
        , "  printf '\"router\" \"%s\"\\n' \"$(md5 router " <> Text.unpack consolePassword <> ")\""
        , "  printf '\"app\" \"%s\"\\n' \"$(md5 app " <> Text.unpack appPassword <> ")\""
        , "} > /tmp/toy-userlist.txt"
        , "printf '*:*:*:router:" <> Text.unpack consolePassword <> "\\n' > /etc/pgbouncer/console.pgpass"
        , "chmod 0600 /etc/pgbouncer/console.pgpass"
        , "chown postgres:postgres /etc/pgbouncer/console.pgpass"
        , {- pgbouncer reads its auth file when it starts and not again, so a
          userlist written under a running process is a password that does
          not work yet -- and the symptom is an authentication failure with
          a correct password in a correct file, which is a bad afternoon.
          Restarting is fine here and only here: this runs before there are
          clients, and only when the file actually changed, because on every
          later pass a restart would drop the very clients the bouncer is in
          the way to protect. -}
          "if ! cmp -s /tmp/toy-userlist.txt /etc/pgbouncer/userlist.txt; then"
        , "  install -m 0644 -o postgres -g postgres /tmp/toy-userlist.txt /etc/pgbouncer/userlist.txt"
        , "  systemctl restart pgbouncer"
        , "fi"
        , "rm -f /tmp/toy-userlist.txt"
        ]
secretScript _ =
    unlines $
        [ "set -e"
        , "export LANG=C LC_ALL=C"
        , "version=$(pg_lsclusters --no-header | awk '{print $1}' | head -n1)"
        , "hba=/etc/postgresql/$version/main/pg_hba.conf"
        ]
            <> [ "printf '*:*:*:" <> role <> ":" <> pwd <> "\\n' > " <> path <> "; chown postgres:postgres " <> path <> "; chmod 0600 " <> path
               | (path, role, pwd) <-
                    [ ("/etc/postgresql/toy-replication.pgpass", "replicator", Text.unpack replPassword)
                    , ("/etc/postgresql/toy-rewind.pgpass", "rewinder", Text.unpack rewindPassword)
                    ]
               ]
            <> [ -- the application's own access, which the pair knows nothing
                 -- about: it routes a database, it does not own one.
                 "line='host all app " <> Text.unpack (machineAddr MachineBouncer) <> "/32 md5'"
               , "grep -qxF \"$line\" \"$hba\" || echo \"$line\" >> \"$hba\""
               , "pg_ctlcluster \"$version\" main reload"
               , "if [ \"$(sudo -u postgres psql -tAXc 'SELECT pg_is_in_recovery()')\" = f ]; then"
               , "  sudo -u postgres psql -tAX -d postgres >/dev/null <<TOY_SQL"
               , "DO \\$do\\$ BEGIN CREATE ROLE app LOGIN; EXCEPTION WHEN duplicate_object THEN NULL; END \\$do\\$;"
               , "ALTER ROLE app LOGIN PASSWORD '" <> Text.unpack appPassword <> "';"
               , "TOY_SQL"
               , "  sudo -u postgres psql -tAXc \"SELECT 1 FROM pg_database WHERE datname='app'\" | grep -q 1 ||"
               , "    sudo -u postgres psql -tAXc 'CREATE DATABASE app OWNER app'"
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
                { env = Just (("PGPASSWORD", Text.unpack appPassword) : filter ((/= "PGPASSWORD") . fst) environment)
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
