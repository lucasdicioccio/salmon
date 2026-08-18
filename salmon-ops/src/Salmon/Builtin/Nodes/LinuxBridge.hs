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

import Salmon.Actions.UpDown (Requirement (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import GHC.IO.Exception (ExitCode (..))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

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
and the bridge up. Depends on the bridge already existing.
-}
tap :: Reporter Report -> Track' (Binary "ip") -> Tap -> Op
tap r ip t =
    withBinary ip ipLinkCommand (AddTap t) $ \add ->
        op "linux-tap" (deps [bridge r ip t.tapBridge]) $ \actions ->
            actions
                { help = "creates tap device " <> t.tapName <> " on bridge " <> t.tapBridge.bridgeName
                , ref = mkRef "linux-tap" (t.tapBridge.bridgeName, t.tapName)
                , prelim = skipIfLinkExists t.tapName
                , up = add r' >> attach r' >> Binary.untrackedExec ipLinkCommand (SetUp t.tapName) "" r'
                , down = Binary.untrackedExec ipLinkCommand (DeleteLink t.tapName) "" r'
                }
  where
    r' = contramap (RunIpLink (AddTap t)) r
    attach r'' = Binary.untrackedExec ipLinkCommand (SetMaster t.tapName t.tapBridge) "" r''

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

ipLinkCommand :: Command "ip" IpLinkCommand
ipLinkCommand = Command $ \cmd -> case cmd of
    (AddBridge br) ->
        proc
            "ip"
            [ "link"
            , "add"
            , "name"
            , Text.unpack br.bridgeName
            , "type"
            , "bridge"
            ]
    (AddTap t) ->
        proc "ip" $
            mconcat
                [ ["tuntap", "add", "dev", Text.unpack t.tapName, "mode", "tap"]
                , maybe [] (\owner -> ["user", Text.unpack owner]) t.tapOwner
                ]
    (SetMaster name br) ->
        proc
            "ip"
            [ "link"
            , "set"
            , Text.unpack name
            , "master"
            , Text.unpack br.bridgeName
            ]
    (SetUp name) ->
        proc
            "ip"
            [ "link"
            , "set"
            , Text.unpack name
            , "up"
            ]
    (DeleteLink name) ->
        proc
            "ip"
            [ "link"
            , "delete"
            , Text.unpack name
            ]
    (AddAddr name cidr) ->
        proc
            "ip"
            [ "addr"
            , "add"
            , Text.unpack (cidrText cidr)
            , "dev"
            , Text.unpack name
            ]
    (DelAddr name cidr) ->
        proc
            "ip"
            [ "addr"
            , "del"
            , Text.unpack (cidrText cidr)
            , "dev"
            , Text.unpack name
            ]
