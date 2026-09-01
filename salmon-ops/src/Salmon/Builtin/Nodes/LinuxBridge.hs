{- | Linux bridge and tap-device primitives, for giving qemu VMs (or anything
else) a real local L2 network to sit on — the @-- TODO: Ip, Ip6, Arp, Bridge,
NetDev@ "Salmon.Builtin.Nodes.Netfilter" never got to.

Neither @ip link add ... type bridge@ nor @ip tuntap add@ is idempotent on
its own (both fail with "File exists" on a second run) — same shape as
"Salmon.Builtin.Nodes.Netfilter"'s @nft add rule@ problem, so this uses the
same fix already established as this project's convention: check whether
the link already exists (@ip link show@) and report 'Skippable' via
'prelim' instead of trying to force the @ip@ invocation itself to be
idempotent (see 'skipIfLinkExists', mirroring
"Salmon.Builtin.Nodes.Podman".'Salmon.Builtin.Nodes.Podman.skipIfNetworkExists').
-}
module Salmon.Builtin.Nodes.LinuxBridge where

import Salmon.Actions.UpDown (Requirement (..), upTree)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.Identity (runIdentity)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

-------------------------------------------------------------------------------
data Report
    = RunIpLink !IpLinkCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

type DevName = Text

-- | A local Linux bridge, identified by its device name (e.g. @salmontest0@).
newtype Bridge = Bridge {bridgeName :: DevName}
    deriving (Eq, Ord, Show)

{- | A tap device attached to a 'Bridge' — what a qemu VM's @-netdev tap@
plugs into. 'tapOwner', if given, is the unprivileged user allowed to open
the resulting @\/dev\/tapN@ without root (matches @ip tuntap add ... user
\<name\>@).
-}
data Tap
    = Tap
    { tapName :: DevName
    , tapBridge :: Bridge
    , tapOwner :: Maybe Text
    }
    deriving (Eq, Ord, Show)

-- | An IPv4 address in CIDR notation, e.g. @Cidr "10.99.0.1" 24@ for @10.99.0.1/24@.
data Cidr = Cidr {cidrAddr :: Text, cidrPrefix :: Int}
    deriving (Eq, Ord, Show)

cidrText :: Cidr -> Text
cidrText c = c.cidrAddr <> "/" <> Text.pack (show c.cidrPrefix)

-------------------------------------------------------------------------------

-- | Creates a bridge device and brings it up.
bridge :: Reporter Report -> Track' (Binary "ip") -> Bridge -> Op
bridge r ip br =
    withBinary ip ipLinkCommand (AddBridge br) $ \add ->
        op "linux-bridge" nodeps $ \actions ->
            actions
                { help = "creates a Linux bridge device " <> br.bridgeName
                , ref = mkRef "linux-bridge" br.bridgeName
                , prelim = skipIfLinkExists br.bridgeName
                , up = add r' >> Binary.untrackedExec ipLinkCommand (SetUp br.bridgeName) "" r'
                , down = Binary.untrackedExec ipLinkCommand (DeleteLink br.bridgeName) "" r'
                }
  where
    r' = contramap (RunIpLink (AddBridge br)) r

{- | Creates a tap device, attaches it to its bridge, and brings both the tap
and the bridge up.

Deliberately does *not* declare the bridge as a graph dependency (@deps
[bridge ...]@) — a persistent, shared bridge (the documented, intended
lifecycle here: many taps/VMs come and go, the bridge outlives all of
them) must never be reachable as *this tap's own* predecessor, or
'Salmon.Actions.UpDown.downTree''s "release a predecessor once its last
dependent is torn down" rule (correct in general — see the directory/
two-files example in CLAUDE.md) would delete the bridge out from under
every *other* still-running tap the moment any single one of them tears
down, since each tap's own 'Salmon.Op.OpGraph.OpGraph' traversal has no
visibility into sibling taps' graphs (different process, different
'downTree' call). Hand-observed 2026-09-08: several real qemu VMs' taps
left dangling (@NO-CARRIER@) after the bridge vanished this way mid test
session — see @specs/qemu-test-vms-progress.md@.

Instead, 'up' ensures the bridge exists via a *nested* 'upTree' run
(same accepted pattern as
"Salmon.Builtin.Nodes.PostgresMigrations".@remoteMigrateOpaqueSetup@ —
check the returned 'Bool', 'throwIO' if it's 'False', since that's the
only way a nested traversal's failure becomes visible to the outer one)
rather than a plain graph dependency, so the bridge is brought up as a
precondition without ever becoming *this* op's own teardown-reachable
predecessor. Whoever wants the bridge gone does so explicitly (e.g.
'bridge'\/'bridgeAddr' torn down directly) — never implicitly as a side
effect of one tap going down.
-}
tap :: Reporter Report -> Track' (Binary "ip") -> Tap -> Op
tap r ip t =
    withBinary ip ipLinkCommand (AddTap t) $ \add ->
        op "linux-tap" nodeps $ \actions ->
            actions
                { help = "creates tap device " <> t.tapName <> " on bridge " <> t.tapBridge.bridgeName
                , ref = mkRef "linux-tap" (t.tapBridge.bridgeName, t.tapName)
                , prelim = skipIfLinkExists t.tapName
                , up = ensureBridge >> add r' >> attach r' >> Binary.untrackedExec ipLinkCommand (SetUp t.tapName) "" r'
                , down = Binary.untrackedExec ipLinkCommand (DeleteLink t.tapName) "" r'
                }
  where
    r' = contramap (RunIpLink (AddTap t)) r
    attach r'' = Binary.untrackedExec ipLinkCommand (SetMaster t.tapName t.tapBridge) "" r''
    ensureBridge = do
        ok <- upTree silent (pure . runIdentity) (bridge r ip t.tapBridge)
        unless ok (throwIO (userError ("linux-tap: failed to bring up bridge " <> Text.unpack t.tapBridge.bridgeName)))

{- | Assigns an IPv4 address to an already-existing 'Bridge' (e.g. so the host
side of a test network has something to route SSH traffic through to a
guest sitting on the same bridge). Depends on the bridge already existing.
-}
bridgeAddr :: Reporter Report -> Track' (Binary "ip") -> Bridge -> Cidr -> Op
bridgeAddr r ip br cidr =
    withBinary ip ipLinkCommand (AddAddr br.bridgeName cidr) $ \add ->
        op "linux-addr" (deps [bridge r ip br]) $ \actions ->
            actions
                { help = "assigns " <> cidrText cidr <> " to " <> br.bridgeName
                , ref = mkRef "linux-addr" (br.bridgeName, cidrText cidr)
                , prelim = skipIfAddrExists br.bridgeName cidr
                , up = add r'
                , down = Binary.untrackedExec ipLinkCommand (DelAddr br.bridgeName cidr) "" r'
                }
  where
    r' = contramap (RunIpLink (AddAddr br.bridgeName cidr)) r

{- | @ip addr show dev \<name\>@ succeeds and lists every address currently
assigned — 'Skippable' iff the wanted CIDR text is already one of them, same
"does the effect already exist" shape as 'skipIfLinkExists'.
-}
skipIfAddrExists :: DevName -> Cidr -> IO Requirement
skipIfAddrExists name cidr = do
    (code, out, _err) <- readCreateProcessWithExitCode (proc "ip" ["addr", "show", "dev", Text.unpack name]) ""
    pure $ case code of
        ExitSuccess | cidrText cidr `Text.isInfixOf` Text.decodeUtf8With Text.lenientDecode out -> Skippable
        _ -> Required

{- | @ip link show \<name\>@ succeeds (exit 0) iff a link by that name already
exists — the same "does the effect already exist" shape as
'Salmon.Builtin.Nodes.Podman.skipIfNetworkExists' \/
'Salmon.Builtin.Nodes.Netfilter.skipIfNftRuleExists'.
-}
skipIfLinkExists :: DevName -> IO Requirement
skipIfLinkExists name = do
    (code, _, _) <- readCreateProcessWithExitCode (proc "ip" ["link", "show", Text.unpack name]) ""
    pure $ case code of
        ExitSuccess -> Skippable
        _ -> Required

-------------------------------------------------------------------------------
data IpLinkCommand
    = AddBridge Bridge
    | AddTap Tap
    | SetMaster DevName Bridge
    | SetUp DevName
    | DeleteLink DevName
    | AddAddr DevName Cidr
    | DelAddr DevName Cidr
    deriving (Show)

{- | Every @ip@ invocation here goes through @capsh@ instead of calling
@ip@ directly — hand-validated 2026-09-08 against a real unprivileged run
(see @specs/qemu-test-vms-progress.md@): granting @ip@ itself
@cap_net_admin@ via plain @setcap@ does not work. @strace@ on a failing
@ip link add@ showed @ip@ unconditionally calling
@capset({...}, {effective=0, permitted=0, inheritable=0})@ at startup —
iproute2 drops its *entire* capability set on exec and only trusts the
*ambient* set to re-populate what it needs, which a plain file-capability
grant can never populate (the kernel zeroes ambient for any exec of a
"privileged" file, by design — see @capabilities(7)@). The fix is a
launcher that already holds the capability (via file caps, granted on
@capsh@ itself — see 'Salmon.Builtin.Nodes.Capabilities.grantCapabilities')
raising it into its *own* ambient set (which requires it in both the
permitted and — separately, since exec does not carry a file's inheritable
bit into the new process's own inheritable set — the inheritable set
first) before exec'ing the real, uncapped @ip@; ambient capabilities do
propagate across exec and are what iproute2 actually honors. Works
identically whether the caller is real root (whose permitted set is
already full, so @--inh=@/@--addamb=@ trivially succeed with no file
capability needed at all) or an unprivileged user with the capability
granted on @capsh@ — so this wrapping is unconditional, not privilege-mode
-specific.
-}
ipLinkCommand :: Command "ip" IpLinkCommand
ipLinkCommand = Command $ \cmd -> capshAmbient (rawIpArgs cmd)

rawIpArgs :: IpLinkCommand -> [String]
rawIpArgs cmd = case cmd of
    (AddBridge br) ->
        [ "link"
        , "add"
        , "name"
        , Text.unpack br.bridgeName
        , "type"
        , "bridge"
        ]
    (AddTap t) ->
        mconcat
            [ ["tuntap", "add", "dev", Text.unpack t.tapName, "mode", "tap"]
            , maybe [] (\owner -> ["user", Text.unpack owner]) t.tapOwner
            ]
    (SetMaster name br) ->
        [ "link"
        , "set"
        , Text.unpack name
        , "master"
        , Text.unpack br.bridgeName
        ]
    (SetUp name) ->
        [ "link"
        , "set"
        , Text.unpack name
        , "up"
        ]
    (DeleteLink name) ->
        [ "link"
        , "delete"
        , Text.unpack name
        ]
    (AddAddr name cidr) ->
        [ "addr"
        , "add"
        , Text.unpack (cidrText cidr)
        , "dev"
        , Text.unpack name
        ]
    (DelAddr name cidr) ->
        [ "addr"
        , "del"
        , Text.unpack (cidrText cidr)
        , "dev"
        , Text.unpack name
        ]

{- | Runs @ip \<args\>@ via @capsh --inh=cap_net_admin --addamb=cap_net_admin
-- -c "ip ...quoted args..."@ — see 'ipLinkCommand's haddock for why. The
inner @-c@ string is re-split by a shell, so each arg is individually
single-quoted first (same "one Haskell string, one remote/shell token"
concern as "Test.Harness".'Test.Harness.quoteForRemoteShell', just for a
local shell instead of ssh's).
-}
capshAmbient :: [String] -> CreateProcess
capshAmbient args =
    proc
        "capsh"
        [ "--inh=cap_net_admin"
        , "--addamb=cap_net_admin"
        , "--"
        , "-c"
        , unwords (map shellQuote ("ip" : args))
        ]

shellQuote :: String -> String
shellQuote s = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) s <> "'"
