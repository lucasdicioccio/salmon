{- | A point-to-point WireGuard VPN between a statically-addressed server and a
client that may sit behind a dynamic-IP eyeball connection.

Design:

* the server owns the VPN subnet (an RFC1918 /24 carved out of 10.0.0.0/8),
  keeps to routing only VPN-subnet traffic on the WireGuard interface (a
  dedicated filter-forward base chain with a drop policy only opens up
  traffic in/out of that interface), and NATs (masquerades) VPN-subnet
  traffic that leaves via any other interface so it can act as the client's
  internet exit node;
* the client keeps its normal default route and LAN routes untouched (more
  specific routes always win over the routes we add here) and additionally
  routes all other traffic over the VPN by installing the classic
  @0.0.0.0\/1@ + @128.0.0.0\/1@ split-default routes, plus a pinned host
  route to the server's own IP via the original default gateway so the
  tunnel's own traffic doesn't try to route over itself; the client also
  sets a persistent-keepalive on its peer entry so the NAT/dynamic-IP side
  of the tunnel stays punched through.
-}
module SreBox.WireGuardVpn where

import Data.Text (Text)

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, justInstall)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Netfilter as Nft
import qualified Salmon.Builtin.Nodes.Routes as IpRoute
import qualified Salmon.Builtin.Nodes.Sysctl as Sysctl
import qualified Salmon.Builtin.Nodes.WireGuard as WG
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------
data Report
    = RunWireGuard !WG.Report
    | RunNft !Nft.Report
    | RunSysctl !Sysctl.Report
    | RunIpRoute !IpRoute.Report
    deriving (Show)

-------------------------------------------------------------------------------

{- | Binaries needed on either side of the VPN. Both server and client need
@wg@/@ip@; only the server needs @nft@ and @sysctl@ (for NAT/forwarding).
-}
data Binaries
    = Binaries
    { binWg :: Track' (Binary "wg")
    , binIp :: Track' (Binary "ip")
    , binNft :: Track' (Binary "nft")
    , binSysctl :: Track' (Binary "sysctl")
    }

-------------------------------------------------------------------------------

-- | One client of the VPN, as seen from the server side.
data ClientPeer
    = ClientPeer
    { peerVpnAddr :: WG.IpNet
    -- ^ the client's address inside the VPN subnet, e.g. 10.0.3.2/24
    , peerPublicKeyPath :: FilePath
    -- ^ path (on the server) to the client's already-provisioned public key
    }

data ServerVpnSpec
    = ServerVpnSpec
    { server_wg_iface :: WG.WgName
    , server_wg_port :: WG.PortNum
    , server_wg_privkey_path :: FilePath
    , server_vpn_net :: WG.IpNet
    -- ^ the VPN subnet as configured on the server's WireGuard interface, e.g. 10.0.3.1/24
    , server_clients :: [ClientPeer]
    }

{- | Sets up the server side: WireGuard interface + peers, IPv4 forwarding,
NAT masquerade for VPN-subnet traffic leaving any non-VPN interface, and a
forward chain restricted to VPN-interface traffic only.
-}
serverVpn :: Reporter Report -> Binaries -> ServerVpnSpec -> Op
serverVpn r bins spec =
    op "wireguard-vpn-server" (deps [wgSetup, forwarding, nat]) id
  where
    wg_r = contramap RunWireGuard r
    nft_r = contramap RunNft r
    sysctl_r = contramap RunSysctl r

    ifaceTrack :: Track' WG.WgName
    ifaceTrack = Track $ \name -> WG.iface wg_r bins.binIp name spec.server_vpn_net

    privkeyTrack :: Track' FilePath
    privkeyTrack = Track $ WG.privateKey bins.binWg

    wgSetup :: Op
    wgSetup =
        op "wireguard-vpn-server-peers" (deps $ fmap addPeer spec.server_clients) id
            `inject` serverIface

    serverIface :: Op
    serverIface =
        WG.server wg_r bins.binWg privkeyTrack ifaceTrack spec.server_wg_iface spec.server_wg_privkey_path spec.server_wg_port

    addPeer :: ClientPeer -> Op
    addPeer p =
        WG.peer
            wg_r
            bins.binWg
            ignoreTrack -- the client's public key is provisioned out of band (e.g. rsync'd in)
            ifaceTrack
            ignoreTrack
            spec.server_wg_iface
            p.peerPublicKeyPath
            Nothing -- server never dials out: the client (dynamic IP) always initiates
            (WG.iptxt p.peerVpnAddr <> "/32")

    -- ip_forward is required for the server to route between the VPN subnet and the internet
    forwarding :: Op
    forwarding =
        Sysctl.set sysctl_r bins.binSysctl (Sysctl.Setting "net.ipv4.ip_forward" "1")
            `inject` forwardChain

    filterTable :: Nft.Table
    filterTable = Nft.Table "filter" Nft.Inet

    -- restrict all forwarding to VPN-interface traffic: nothing else gets routed through this box
    forwardChain :: Op
    forwardChain =
        Nft.rule nft_r bins.binNft chain (Nft.RawRule ["ct", "state", "established,related", "accept"])
            `inject` Nft.rule nft_r bins.binNft chain (Nft.RawRule ["iifname", ifaceQuoted, "accept"])
      where
        chain = Nft.baseChain "forward" filterTable (Nft.BaseChainSpec Nft.FilterChain Nft.Forward 0 Nft.Drop)

    natTable :: Nft.Table
    natTable = Nft.Table "nat" Nft.Inet

    -- masquerade VPN-subnet traffic as it leaves any interface other than the VPN one
    nat :: Op
    nat =
        Nft.rule
            nft_r
            bins.binNft
            (Nft.baseChain "postrouting" natTable (Nft.BaseChainSpec Nft.NatChain Nft.PostRouting 100 Nft.Accept))
            (Nft.RawRule ["ip", "saddr", WG.nettxt spec.server_vpn_net, "oifname", "!=", ifaceQuoted, "masquerade"])

    ifaceQuoted :: Text
    ifaceQuoted = "\"" <> spec.server_wg_iface <> "\""

-------------------------------------------------------------------------------

data ClientVpnSpec
    = ClientVpnSpec
    { client_wg_iface :: WG.WgName
    , client_wg_privkey_path :: FilePath
    , client_vpn_addr :: WG.IpNet
    -- ^ the client's own address inside the VPN subnet, e.g. 10.0.3.2/24
    , client_server_endpoint :: WG.Endpoint
    -- ^ "host:port" of the server (its static IP)
    , client_server_ip :: Text
    -- ^ bare IP of the server (used to pin a host route via the original gateway)
    , client_server_pubkey_path :: FilePath
    -- ^ path (on the client) to the server's already-provisioned public key
    , client_keepalive_seconds :: Int
    -- ^ persistent-keepalive so the NAT/dynamic-IP side stays punched through; 25s is the usual default
    }

{- | Sets up the client side: WireGuard interface + a single peer (the
server) routing all traffic (@0.0.0.0\/0@ split in two \/1s, the classic
wg-quick trick) over the VPN, plus a pinned host route to the server's own
IP so the tunnel doesn't try to route over itself. Existing LAN/default
routes are left untouched, since more specific routes always take priority
over what we add here.
-}
clientVpn :: Reporter Report -> Binaries -> ClientVpnSpec -> Op
clientVpn r bins spec =
    op "wireguard-vpn-client" (deps [routeAllTraffic]) id
  where
    wg_r = contramap RunWireGuard r
    ip_r = contramap RunIpRoute r

    ifaceTrack :: Track' WG.WgName
    ifaceTrack = Track $ \name -> WG.iface wg_r bins.binIp name spec.client_vpn_addr

    privkeyTrack :: Track' FilePath
    privkeyTrack = Track $ WG.privateKey bins.binWg

    clientIface :: Op
    clientIface =
        WG.client wg_r bins.binWg privkeyTrack ifaceTrack spec.client_wg_iface spec.client_wg_privkey_path

    serverPeer :: Op
    serverPeer =
        WG.peerKeepalive
            wg_r
            bins.binWg
            ignoreTrack -- the server's public key is provisioned out of band
            ifaceTrack
            ignoreTrack -- the endpoint is just a "host:port" string, nothing to provision
            spec.client_wg_iface
            spec.client_server_pubkey_path
            (Just spec.client_server_endpoint)
            "0.0.0.0/0"
            (Just spec.client_keepalive_seconds)
            `inject` clientIface

    -- pin a host route to the server's own IP via whatever the kernel currently
    -- uses (the physical uplink), so the tunnel's own traffic doesn't loop over itself
    pinServerRoute :: Op
    pinServerRoute =
        ( op "wireguard-vpn-client-pin-server-route" (deps [justInstall bins.binIp]) $ \actions ->
            actions
                { help = "keeps the route to the VPN server itself off the tunnel"
                , ref = mkRef "wg-vpn-pin-server-route" spec.client_server_ip
                , up = do
                    (via, dev) <- IpRoute.discoverGatewayFor spec.client_server_ip
                    let cmd = IpRoute.ReplaceRoute (IpRoute.Route (IpRoute.RawNetwork (spec.client_server_ip <> "/32")) dev via)
                    Binary.untrackedExec IpRoute.ipcommand cmd "" (contramap (IpRoute.RunIp cmd) ip_r)
                }
        )
            `inject` serverPeer

    -- classic wg-quick "route all traffic" trick: two more-specific-than-default
    -- routes over the VPN interface, leaving the real default route (and every
    -- other more-specific route, e.g. the LAN) untouched
    routeAllTraffic :: Op
    routeAllTraffic =
        IpRoute.route ip_r bins.binIp (IpRoute.Route (IpRoute.RawNetwork "128.0.0.0/1") spec.client_wg_iface Nothing)
            `inject` IpRoute.route ip_r bins.binIp (IpRoute.Route (IpRoute.RawNetwork "0.0.0.0/1") spec.client_wg_iface Nothing)
            `inject` pinServerRoute
