{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A WireGuard mesh among hosts that have stable endpoints, declared once and
unfolded per host. See @specs\/wireguard-mesh.md@ for the whole design.

= Shape

The /seed/ ('MeshSeed') is the one declaration: peers (a declared address, a
public key collected out of band, an optional endpoint, groups), policies
(group to group, protocol, ports) and routers (a peer that also carries a
network, or the default route). 'genHost' is a __pure__ fan-out of that seed
to one host's view ('HostSpec'): its interface, its peers with their
allowed-ips, its routes and its nft rules. 'hostOp' turns a 'HostSpec' into
an 'Op'. Nothing in between touches the network or the filesystem, so the
table of seeds to host specs is a Layer 0 test.

= Delivery

'hostDocument' wraps a host's 'HostSpec' as a pull-mode document (one inline
directive), to be signed and published under a label named for the host and
followed with @run serve --follow ... --label HOST@. A peer added to or
removed from the seed is then a small diff of nodes the ledger brings up and
down, which is why "Salmon.Builtin.Nodes.WireGuard"'s @peer@ has a @down@.

= What this recipe does not do

* It ships no secret. Each host generates its own private key where it is
  (@WG.privateKey@, kept, never sent); a public key is not a secret and is
  carried inline in the seed.
* A peer with no endpoint dials out and sets a persistent keepalive; it is
  reached only through peers that have one. Two peers that both lack an
  endpoint have no link. There is no NAT traversal and no roaming.
* An endpoint given as a host name is resolved by WireGuard when the peer is
  configured. Following a changed address is left to the peer node's
  @check@ and the tending loop, not done here.
* Policies are enforced on the input of the mesh interface (traffic addressed
  to the host). Traffic a router forwards between the mesh and elsewhere is
  governed by the router's forward chain (mesh-interface traffic only), not
  by policies.
* IPv4 only. An exit router's endpoint must be an IPv4 literal, because the
  route pinning it off the tunnel needs an address, not a name.
-}
module SreBox.WireGuardMesh (
    -- * The declaration
    MeshSeed (..),
    PeerDecl (..),
    Policy (..),
    Proto (..),
    PortRange (..),
    RouterDecl (..),

    -- * The per-host view
    HostSpec (..),
    PeerLink (..),
    NftRuleSpec (..),
    MeshError (..),
    renderError,
    validate,
    genHost,
    genAll,

    -- * Delivery
    hostDocument,

    -- * The node
    hostOp,
    Report,
    Binaries (..),

    -- * Exposed for tests
    parseIpv4,
    parseCidr,
    inSubnet,
) where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Bits (complement, shiftL, (.&.), (.|.))
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Salmon.Actions.Follow (Document (..), Entry (..))
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Netfilter as Nft
import qualified Salmon.Builtin.Nodes.Routes as IpRoute
import qualified Salmon.Builtin.Nodes.Sysctl as Sysctl
import qualified Salmon.Builtin.Nodes.WireGuard as WG
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track
import Salmon.Reporter

import SreBox.WireGuardVpn (Binaries (..), Report (..), pinHostRoute)

-------------------------------------------------------------------------------
-- the declaration

data MeshSeed = MeshSeed
    { mesh_subnet :: Text
    -- ^ the mesh's network, e.g. @10.66.0.0/24@
    , mesh_iface :: Text
    , mesh_port :: Int
    -- ^ the UDP port every host listens on
    , mesh_privkey_path :: FilePath
    -- ^ where each host keeps (and, if absent, generates) its private key
    , mesh_peers :: [PeerDecl]
    , mesh_policies :: [Policy]
    , mesh_routers :: [RouterDecl]
    }
    deriving (Eq, Show, Generic)

instance FromJSON MeshSeed
instance ToJSON MeshSeed

data PeerDecl = PeerDecl
    { peer_name :: Text
    , peer_addr :: Text
    -- ^ a bare IPv4 address inside the subnet, declared and never derived from
    -- position, so adding or removing a peer renumbers nobody
    , peer_pubkey :: Text
    -- ^ base64, collected out of band
    , peer_endpoint :: Maybe Text
    -- ^ @host:port@, host a literal or a name
    , peer_groups :: [Text]
    }
    deriving (Eq, Show, Generic)

instance FromJSON PeerDecl
instance ToJSON PeerDecl

data Proto = Tcp | Udp | Icmp | AnyProto
    deriving (Eq, Show, Generic)

instance FromJSON Proto
instance ToJSON Proto

data PortRange = PortRange {port_from :: Int, port_to :: Int}
    deriving (Eq, Show, Generic)

instance FromJSON PortRange
instance ToJSON PortRange

data Policy = Policy
    { policy_from :: [Text]
    -- ^ groups
    , policy_to :: [Text]
    -- ^ groups
    , policy_protocol :: Proto
    , policy_ports :: [PortRange]
    -- ^ only for 'Tcp' and 'Udp'; none means every port
    }
    deriving (Eq, Show, Generic)

instance FromJSON Policy
instance ToJSON Policy

{- | A peer that also reaches a network. @0.0.0.0/0@ makes it the mesh's exit
node; at most one router may be that.
-}
data RouterDecl = RouterDecl
    { router_peer :: Text
    , router_network :: Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON RouterDecl
instance ToJSON RouterDecl

-------------------------------------------------------------------------------
-- the per-host view

-- | One peer as a host configures it.
data PeerLink = PeerLink
    { link_name :: Text
    , link_pubkey :: Text
    , link_endpoint :: Maybe Text
    , link_allowed_ips :: [Text]
    , link_keepalive :: Maybe Int
    }
    deriving (Eq, Show, Generic)

instance FromJSON PeerLink
instance ToJSON PeerLink

-- | A rule in the host's mesh input chain, as the words handed to @nft@.
newtype NftRuleSpec = NftRuleSpec {rule_words :: [Text]}
    deriving (Eq, Show, Generic)

instance FromJSON NftRuleSpec
instance ToJSON NftRuleSpec

data HostSpec = HostSpec
    { host_name :: Text
    , host_iface :: Text
    , host_addr :: Text
    -- ^ CIDR, the host's address with the mesh's prefix length
    , host_port :: Int
    , host_privkey_path :: FilePath
    , host_peers :: [PeerLink]
    , host_routes :: [Text]
    -- ^ networks to send over the interface (besides the mesh itself)
    , host_exit_pins :: [Text]
    -- ^ addresses kept off the tunnel (the endpoints of exit routers)
    , host_forwards :: Maybe Text
    -- ^ for a router, the mesh subnet it masquerades when it leaves elsewhere
    , host_input_rules :: [NftRuleSpec]
    -- ^ accepted traffic on the mesh interface, in order, after
    -- established/related and before the final drop
    }
    deriving (Eq, Show, Generic)

instance FromJSON HostSpec
instance ToJSON HostSpec

data MeshError
    = BadSubnet Text
    | BadAddress Text Text
    | AddressOutsideSubnet Text Text
    | DuplicatePeerName Text
    | DuplicateAddress Text [Text]
    | DuplicatePubkey [Text]
    | BadEndpoint Text Text
    | UnknownGroup Int Text
    | BadPorts Int Text
    | UnknownRouterPeer Text
    | BadNetwork Text Text
    | DuplicateRouterNetwork Text
    | SeveralExitRouters [Text]
    | ExitEndpointNotLiteral Text
    | UnknownHost Text
    deriving (Eq, Show)

renderError :: MeshError -> Text
renderError e = case e of
    BadSubnet s -> "mesh subnet is not an IPv4 CIDR: " <> s
    BadAddress p a -> "peer " <> p <> " has an address that is not IPv4: " <> a
    AddressOutsideSubnet p a -> "peer " <> p <> " has address " <> a <> " outside the mesh subnet"
    DuplicatePeerName p -> "two peers are named " <> p
    DuplicateAddress a ps -> "address " <> a <> " is declared by several peers: " <> Text.intercalate ", " ps
    DuplicatePubkey ps -> "peers share one public key: " <> Text.intercalate ", " ps
    BadEndpoint p ep -> "peer " <> p <> " has an endpoint that is not host:port: " <> ep
    UnknownGroup i g -> "policy #" <> tshow i <> " names a group no peer belongs to: " <> g
    BadPorts i why -> "policy #" <> tshow i <> ": " <> why
    UnknownRouterPeer p -> "router names an unknown peer: " <> p
    BadNetwork p n -> "router " <> p <> " has a network that is not an IPv4 CIDR: " <> n
    DuplicateRouterNetwork n -> "network " <> n <> " is carried by several routers"
    SeveralExitRouters ps -> "several peers are the exit node: " <> Text.intercalate ", " ps
    ExitEndpointNotLiteral p -> "exit router " <> p <> " needs an endpoint that is an IPv4 literal (its route is pinned by address)"
    UnknownHost h -> "no such peer in the seed: " <> h

tshow :: Show a => a -> Text
tshow = Text.pack . show

-------------------------------------------------------------------------------
-- pure IPv4

parseIpv4 :: Text -> Maybe Word32
parseIpv4 t = case traverse readOctet (Text.splitOn "." t) of
    Just [a, b, c, d] -> Just ((a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d)
    _ -> Nothing
  where
    readOctet o
        | Text.null o || Text.any (`notElem` ['0' .. '9']) o || Text.length o > 3 = Nothing
        | otherwise = case readMaybe (Text.unpack o) :: Maybe Word32 of
            Just n | n <= 255 -> Just n
            _ -> Nothing

-- | An address and a prefix length.
parseCidr :: Text -> Maybe (Word32, Int)
parseCidr t = case Text.splitOn "/" t of
    [a, p] -> do
        addr <- parseIpv4 a
        n <- if Text.all (`elem` ['0' .. '9']) p && not (Text.null p) then readMaybe (Text.unpack p) else Nothing
        if n >= 0 && n <= 32 then Just (addr, n) else Nothing
    _ -> Nothing

mask :: Int -> Word32
mask 0 = 0
mask n = complement 0 `shiftL` (32 - n)

-- | Is the address inside the (address, prefix) network?
inSubnet :: (Word32, Int) -> Word32 -> Bool
inSubnet (net, n) a = (a .&. mask n) == (net .&. mask n)

-- | @host:port@ with a port in range.
parseEndpoint :: Text -> Maybe (Text, Int)
parseEndpoint t =
    let (h, rest) = Text.breakOnEnd ":" t
        host = Text.dropEnd 1 h
     in do
            port <- if Text.all (`elem` ['0' .. '9']) rest && not (Text.null rest) then readMaybe (Text.unpack rest) else Nothing
            if not (Text.null host) && port >= 1 && port <= 65535 then Just (host, port) else Nothing

-------------------------------------------------------------------------------
-- validation

duplicates :: Ord a => [a] -> [a]
duplicates xs = Map.keys (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(x, 1) | x <- xs]))

-- | Every refusal at once; empty when the seed is sound.
validate :: MeshSeed -> [MeshError]
validate s = subnetErrs <> nameErrs <> addrErrs <> keyErrs <> endpointErrs <> policyErrs <> routerErrs
  where
    peers = s.mesh_peers
    msubnet = parseCidr s.mesh_subnet
    subnetErrs = [BadSubnet s.mesh_subnet | Nothing <- [msubnet]]
    nameErrs = DuplicatePeerName <$> duplicates (peer_name <$> peers)
    addrErrs =
        concat
            [ case parseIpv4 p.peer_addr of
                Nothing -> [BadAddress p.peer_name p.peer_addr]
                Just a -> [AddressOutsideSubnet p.peer_name p.peer_addr | Just net <- [msubnet], not (inSubnet net a)]
            | p <- peers
            ]
            <> [DuplicateAddress a [p.peer_name | p <- peers, p.peer_addr == a] | a <- duplicates (peer_addr <$> peers)]
    keyErrs = [DuplicatePubkey [p.peer_name | p <- peers, p.peer_pubkey == k] | k <- duplicates (peer_pubkey <$> peers)]
    endpointErrs = [BadEndpoint p.peer_name ep | p <- peers, Just ep <- [p.peer_endpoint], Nothing <- [parseEndpoint ep]]
    groups = nub (concatMap peer_groups peers)
    policyErrs =
        concat
            [ [UnknownGroup i g | g <- pol.policy_from <> pol.policy_to, g `notElem` groups]
                <> portErrs i pol
            | (i, pol) <- zip [1 ..] s.mesh_policies
            ]
    portErrs i pol =
        [BadPorts i "ports given for a protocol without ports" | pol.policy_protocol `elem` [Icmp, AnyProto], not (null pol.policy_ports)]
            <> [ BadPorts i ("bad port range " <> tshow lo <> "-" <> tshow hi)
               | PortRange lo hi <- pol.policy_ports
               , lo < 1 || hi > 65535 || lo > hi
               ]
            <> [BadPorts i "a policy needs at least one group on each side" | null pol.policy_from || null pol.policy_to]
    routerErrs =
        concat
            [ [UnknownRouterPeer r.router_peer | r.router_peer `notElem` (peer_name <$> peers)]
                <> [BadNetwork r.router_peer r.router_network | Nothing <- [parseCidr r.router_network]]
            | r <- s.mesh_routers
            ]
            <> (DuplicateRouterNetwork <$> duplicates (router_network <$> s.mesh_routers))
            <> [SeveralExitRouters exits | let exits = [r.router_peer | r <- s.mesh_routers, isExit r.router_network], length exits > 1]
            <> [ ExitEndpointNotLiteral r.router_peer
               | r <- s.mesh_routers
               , isExit r.router_network
               , Just p <- [lookupPeer r.router_peer]
               , not (maybe False (isLiteral . fst) (p.peer_endpoint >>= parseEndpoint))
               ]
    lookupPeer n = case [p | p <- peers, p.peer_name == n] of (p : _) -> Just p; [] -> Nothing
    isLiteral h = parseIpv4 h /= Nothing

isExit :: Text -> Bool
isExit n = n == "0.0.0.0/0"

-------------------------------------------------------------------------------
-- fan-out

-- | The view of every host, or every refusal.
genAll :: MeshSeed -> Either [MeshError] [HostSpec]
genAll s = case validate s of
    [] -> Right [genChecked s p | p <- s.mesh_peers]
    errs -> Left errs

-- | One host's view, or every refusal (an unknown host included).
genHost :: MeshSeed -> Text -> Either [MeshError] HostSpec
genHost s name = case validate s of
    [] -> case [p | p <- s.mesh_peers, p.peer_name == name] of
        (p : _) -> Right (genChecked s p)
        [] -> Left [UnknownHost name]
    errs -> Left errs

-- | Fan-out for a seed already validated.
genChecked :: MeshSeed -> PeerDecl -> HostSpec
genChecked s me =
    HostSpec
        { host_name = me.peer_name
        , host_iface = s.mesh_iface
        , host_addr = me.peer_addr <> "/" <> prefixLen
        , host_port = s.mesh_port
        , host_privkey_path = s.mesh_privkey_path
        , host_peers = links
        , host_routes = routes
        , host_exit_pins = pins
        , host_forwards = if myRouters /= [] then Just s.mesh_subnet else Nothing
        , host_input_rules = inputRules
        }
  where
    prefixLen = maybe "32" (tshow . snd) (parseCidr s.mesh_subnet)
    others = [p | p <- s.mesh_peers, p.peer_name /= me.peer_name]
    routerNets p = [r.router_network | r <- s.mesh_routers, r.router_peer == p.peer_name]
    myRouters = routerNets me

    -- a link exists when at least one side has an endpoint to dial
    linked p = me.peer_endpoint /= Nothing || p.peer_endpoint /= Nothing
    links =
        [ PeerLink
            { link_name = p.peer_name
            , link_pubkey = p.peer_pubkey
            , link_endpoint = p.peer_endpoint
            , link_allowed_ips = (p.peer_addr <> "/32") : routerNets p
            , -- whoever has no endpoint is the one dialling, and keeps the path open
              link_keepalive = if me.peer_endpoint == Nothing then Just 25 else Nothing
            }
        | p <- others
        , linked p
        ]

    reachedRouters = [(p, n) | l <- links, Just p <- [byName l.link_name], n <- routerNets p]
    byName n = case [p | p <- s.mesh_peers, p.peer_name == n] of (p : _) -> Just p; [] -> Nothing
    routes =
        concat
            [ if isExit n then ["0.0.0.0/1", "128.0.0.0/1"] else [n]
            | (_, n) <- reachedRouters
            ]
    pins = [h | (p, n) <- reachedRouters, isExit n, Just (h, _) <- [p.peer_endpoint >>= parseEndpoint]]

    -- policies whose destination includes this host
    inputRules =
        [ NftRuleSpec (policyWords srcs pol)
        | pol <- s.mesh_policies
        , any (`elem` me.peer_groups) pol.policy_to
        , let srcs = sort (nub [p.peer_addr | p <- s.mesh_peers, any (`elem` p.peer_groups) pol.policy_from])
        ]

    ifaceQuoted = "\"" <> s.mesh_iface <> "\""
    policyWords srcs pol =
        ["iifname", ifaceQuoted, "ip", "saddr", set srcs]
            <> protoWords pol

    protoWords pol = case pol.policy_protocol of
        AnyProto -> ["accept"]
        Icmp -> ["ip", "protocol", "icmp", "accept"]
        Tcp -> portWords "tcp" pol.policy_ports
        Udp -> portWords "udp" pol.policy_ports
    portWords proto [] = ["meta", "l4proto", proto, "accept"]
    portWords proto ranges = [proto, "dport", set (renderRange <$> ranges), "accept"]
    renderRange (PortRange lo hi) = if lo == hi then tshow lo else tshow lo <> "-" <> tshow hi

    -- nft prints a one-element set without braces, and the rule's check
    -- matches the rendered text, so render it the way nft lists it
    set [x] = x
    set xs = "{ " <> Text.intercalate ", " xs <> " }"

-------------------------------------------------------------------------------
-- delivery

{- | A host's pull-mode document: one inline directive. @docid@ is the
publisher's name for this revision (it is what @history@ records).
-}
hostDocument :: Text -> HostSpec -> Document
hostDocument docid h = Document{docId = docid, docSeeds = [SeedDirective (toJSON h)], docPublished = Nothing}

-------------------------------------------------------------------------------
-- the node

{- | The Op for one host: the interface with its private key (generated here
if absent, never sent anywhere), the peers, the routes, and the forwarding
and nft rules the 'HostSpec' asks for.
-}
hostOp :: Reporter Report -> Binaries -> HostSpec -> Op
hostOp r bins h =
    op "wireguard-mesh-host" (deps [routing, forwarding, filtering]) $ \actions ->
        actions{help = "wireguard mesh host " <> h.host_name}
  where
    wg_r = contramap RunWireGuard r
    nft_r = contramap RunNft r
    sysctl_r = contramap RunSysctl r
    ip_r = contramap RunIpRoute r

    (_, plen) = fromMaybe (0, 32) (parseCidr h.host_addr)
    netCidr = WG.Ipv4Cidr (Text.takeWhile (/= '/') h.host_addr) plen

    ifaceTrack :: Track' WG.WgName
    ifaceTrack = Track $ \name -> WG.iface wg_r bins.binIp name netCidr

    privkeyTrack :: Track' FilePath
    privkeyTrack = Track $ WG.privateKey bins.binWg

    serverIface :: Op
    serverIface = WG.server wg_r bins.binWg privkeyTrack ifaceTrack h.host_iface h.host_privkey_path h.host_port

    peerOp :: PeerLink -> Op
    peerOp l =
        WG.peerWith
            wg_r
            bins.binWg
            ifaceTrack
            ignoreTrack
            h.host_iface
            (WG.PeerKeyValue l.link_pubkey)
            l.link_endpoint
            (Text.intercalate "," l.link_allowed_ips)
            l.link_keepalive

    peers :: Op
    peers = op "wireguard-mesh-peers" (deps (peerOp <$> h.host_peers)) id `inject` serverIface

    pinOps = [pinHostRoute ip_r bins a `inject` peers | a <- h.host_exit_pins]

    -- every pin precedes every route, or the tunnel's own traffic loops
    routing :: Op
    routing =
        op "wireguard-mesh-routes" (deps routeOps) id
            `inject` peers
      where
        routeOps =
            [ foldl inject (IpRoute.route ip_r bins.binIp (IpRoute.Route (IpRoute.RawNetwork n) h.host_iface Nothing)) (peers : pinOps)
            | n <- h.host_routes
            ]

    table :: Nft.Table
    table = Nft.Table "filter" Nft.Inet

    forwarding :: Op
    forwarding = case h.host_forwards of
        Nothing -> noop "no forwarding"
        Just subnet ->
            Sysctl.set sysctl_r bins.binSysctl (Sysctl.Setting "net.ipv4.ip_forward" "1")
                `inject` forwardChain
                `inject` natRule subnet

    ifaceQuoted = "\"" <> h.host_iface <> "\""

    forwardChain :: Op
    forwardChain =
        Nft.rule nft_r bins.binNft fchain (Nft.RawRule ["ct", "state", "established,related", "accept"])
            `inject` Nft.rule nft_r bins.binNft fchain (Nft.RawRule ["iifname", ifaceQuoted, "accept"])
      where
        fchain = Nft.baseChain "forward" table (Nft.BaseChainSpec Nft.FilterChain Nft.Forward 0 Nft.Drop)

    natRule subnet =
        Nft.rule
            nft_r
            bins.binNft
            (Nft.baseChain "postrouting" (Nft.Table "nat" Nft.Inet) (Nft.BaseChainSpec Nft.NatChain Nft.PostRouting 100 Nft.Accept))
            (Nft.RawRule ["ip", "saddr", subnet, "oifname", "!=", ifaceQuoted, "masquerade"])

    -- an input chain of its own (policy accept: only the mesh interface is
    -- restricted, ssh and everything else is untouched): established, then one
    -- accept per policy, then drop whatever else arrives over the mesh
    filtering :: Op
    filtering = case h.host_input_rules of
        [] | null h.host_peers -> noop "no mesh filter"
        rules ->
            let rs =
                    [Nft.RawRule ["iifname", ifaceQuoted, "ct", "state", "established,related", "accept"]]
                        <> [Nft.RawRule w | NftRuleSpec w <- rules]
                        <> [Nft.RawRule ["iifname", ifaceQuoted, "drop"]]
                -- `x inject y` puts y before x, so each rule injects its predecessor
                ops = Nft.rule nft_r bins.binNft inchain <$> rs
             in foldl1 (\prev next -> next `inject` prev) ops
      where
        inchain = Nft.baseChain ("mesh-" <> h.host_iface) (Nft.Table ("mesh_" <> h.host_iface) Nft.Inet) (Nft.BaseChainSpec Nft.FilterChain Nft.Input 0 Nft.Accept)

