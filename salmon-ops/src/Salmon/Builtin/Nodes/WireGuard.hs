{-# LANGUAGE OverloadedStrings #-}

module Salmon.Builtin.Nodes.WireGuard where

import Salmon.Actions.UpDown (CheckResult (..), skipIfFileExists)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), CommandIO (..), checkExitCode, justInstall, untrackedExec, withBinary, withBinaryIO)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import System.IO (IOMode (ReadMode, WriteMode), withFile)

import Control.Monad (unless)
import qualified Data.ByteString as ByteString
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.IO.Handle (Handle)

import System.FilePath (takeDirectory, (</>))
import GHC.IO.Exception (ExitCode (..))
import System.Process (StdStream (UseHandle), waitForProcess)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------
data Report
    = RunWg !WgCommand !Binary.Report
    | RunIp !IpCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------
newtype PrivateKeyForWriting = PrivateKeyForWriting {getWritePkHandle :: Handle}

newtype PublicKeyForWriting = PublicKeyForWriting {getWritePubPkHandle :: Handle}

newtype PrivateKeyForReading = PrivateKeyForReading {getReadPkHandle :: Handle}

privateKey ::
    Track' (Binary "wg") ->
    FilePath ->
    Op
privateKey wg path =
    withBinaryIO wg genkeycommand GenKey $ \writePK ->
        op "wg-private-key" (deps [enclosingdir]) $ \actions ->
            actions
                { help = "privkey at " <> Text.pack path
                , ref = mkRef "wg-write-pk" path
                , check = skipIfFileExists path
                , up = withFile path WriteMode $ \h -> do
                    (_, _, _, ph) <- writePK (PrivateKeyForWriting h)
                    waitForProcess ph >>= checkExitCode "wg genkey"
                }
  where
    enclosingdir :: Op
    enclosingdir = FS.dir (FS.Directory $ takeDirectory path)

publicKey ::
    Track' (Binary "wg") ->
    Track' FilePath ->
    FilePath ->
    FilePath ->
    Op
publicKey wg mkprivate private path =
    withBinaryIO wg pubkeycommand PubKey $ \writePK ->
        op "wg-public-key" (deps [run mkprivate private, enclosingdir]) $ \actions ->
            actions
                { help = "pubkey at " <> Text.pack path
                , ref = mkRef "wg-write-public-pk" path
                , check = skipIfFileExists path
                , up =
                    withFile private ReadMode $ \hIn ->
                        withFile path WriteMode $ \hOut -> do
                            (_, _, _, ph) <- writePK ((PrivateKeyForReading hIn), (PublicKeyForWriting hOut))
                            waitForProcess ph >>= checkExitCode "wg pubkey"
                }
  where
    enclosingdir :: Op
    enclosingdir = FS.dir (FS.Directory $ takeDirectory path)

data GenKeyCommand
    = GenKey

data PubKeyCommand
    = PubKey

genkeycommand :: CommandIO "wg" GenKeyCommand PrivateKeyForWriting
genkeycommand = CommandIO $ \cmd -> case cmd of
    GenKey -> \h -> do
        pure ((proc "wg" ["genkey"]){std_out = UseHandle (getWritePkHandle h)})

pubkeycommand :: CommandIO "wg" PubKeyCommand (PrivateKeyForReading, PublicKeyForWriting)
pubkeycommand = CommandIO $ \cmd -> case cmd of
    PubKey -> \(hin, hout) -> do
        pure ((proc "wg" ["pubkey"]){std_in = UseHandle (getReadPkHandle hin), std_out = UseHandle (getWritePubPkHandle hout)})

-------------------------------------------------------------------------------
type Ipv4 = Text
type Ipv4PrefixSize = Int

data IpNet
    = Ipv4Cidr Ipv4 Ipv4PrefixSize
    deriving (Show)

nettxt :: IpNet -> Text
nettxt (Ipv4Cidr ipv4 cidr) = ipv4 <> "/" <> Text.pack (show cidr)

iptxt :: IpNet -> Text
iptxt (Ipv4Cidr ipv4 _) = ipv4

data RFC1918
    = Ten8
    | OneSevenTwo12
    | OneNineTwoOneSixEight16

type NetworkNum = Int
type MachineNum = Int

rfc1918_slash24 :: RFC1918 -> NetworkNum -> MachineNum -> IpNet
rfc1918_slash24 rfc n m =
    let
        n', m' :: Text
        n' = Text.pack (show n)
        m' = Text.pack (show m)
     in
        case rfc of
            Ten8 ->
                Ipv4Cidr (mconcat ["10.0.", n', ".", m']) 24
            OneSevenTwo12 ->
                Ipv4Cidr (mconcat ["172.16.", n', ".", m']) 24
            OneNineTwoOneSixEight16 ->
                Ipv4Cidr (mconcat ["192.168.", n', ".", m']) 24

type WgName = Text

{- | A WireGuard interface with an address, up.

'up' tolerates an interface that is already there: @ip link add@ fails on a
second run, so the link is only added when @ip link show@ does not know it,
and the address is set with @ip address replace@ (a set, not an insert). The
'check' answers whether the link exists, is @UP@ and carries the address, so
a second @run up@ skips it and, under @run serve@, a link deleted behind
salmon's back is put back.

__The interface does not survive a reboot__ and nothing here makes it: under
@run serve@ the tending loop re-creates it from the 'check', and a host that
wants it before salmon is running is a @systemd-networkd@ netdev, which is a
different node.
-}
iface ::
    Reporter Report ->
    Track' (Binary "ip") ->
    WgName ->
    IpNet ->
    Op
iface r ip wg net =
    withCommand (AddWg wg) $ \addwg ->
        withCommand (SetWgAddr wg net) $ \setAddr ->
            withCommand (UpWg wg) $ \activate ->
                op "wireguard-iface" nodeps $ \actions ->
                    actions
                        { help = "wireguard interface " <> wg <> " at " <> nettxt net
                        , ref = mkRef "wg-iface" wg
                        , check = checkIface wg net
                        , up = do
                            exists <- linkExists wg
                            unless exists addwg
                            setAddr
                            activate
                        }
  where
    r' cmd = contramap (RunIp cmd) r
    withCommand :: IpCommand -> (IO () -> Op) -> Op
    withCommand cmd f =
        let
            g :: (Reporter Binary.Report -> IO ()) -> Op
            g callbin = f (callbin (r' cmd))
         in
            withBinary ip ipcommand cmd g

-- | Does @ip link show dev NAME@ know the link?
linkExists :: WgName -> IO Bool
linkExists wg = do
    (code, _, _) <- readCreateProcessWithExitCode (proc "ip" ["link", "show", "dev", Text.unpack wg]) ""
    pure (code == ExitSuccess)

checkIface :: WgName -> IpNet -> IO CheckResult
checkIface wg net = do
    (lcode, lout, _) <- readCreateProcessWithExitCode (proc "ip" ["-o", "link", "show", "dev", Text.unpack wg]) ""
    (acode, aout, _) <- readCreateProcessWithExitCode (proc "ip" ["-o", "-4", "address", "show", "dev", Text.unpack wg]) ""
    pure $ case (lcode, acode) of
        (ExitSuccess, ExitSuccess) -> interpretIface wg net (Text.decodeUtf8 lout) (Text.decodeUtf8 aout)
        (ExitFailure _, _) -> Failure ("no such link: " <> wg)
        _ -> Unknown

{- | The verdict from @ip -o link show dev NAME@ and @ip -o -4 address show dev
NAME@ output (split out so it can be tested without a network namespace): the
link must carry the @UP@ flag and the address must be listed.
-}
interpretIface :: WgName -> IpNet -> Text -> Text -> CheckResult
interpretIface wg net link addrs
    | not ("UP" `elem` flags) = Failure ("link not up: " <> wg)
    | nettxt net `notElem` concatMap Text.words (Text.lines addrs) = Failure ("address missing on " <> wg <> ": " <> nettxt net)
    | otherwise = Success
  where
    -- "5: wg0: <POINTOPOINT,NOARP,UP,LOWER_UP> mtu 1420 ..."
    flags = Text.splitOn "," (Text.takeWhile (/= '>') (Text.drop 1 (Text.dropWhile (/= '<') link)))

data IpCommand
    = AddWg WgName
    | SetWgAddr WgName IpNet
    | UpWg WgName
    deriving (Show)

ipcommand :: Command "ip" IpCommand
ipcommand = Command $ \cmd -> case cmd of
    (AddWg name) ->
        proc
            "ip"
            [ "link"
            , "add"
            , "dev"
            , Text.unpack name
            , "type"
            , "wireguard"
            ]
    (SetWgAddr name ipnet) ->
        proc
            "ip"
            [ "address"
            , "replace"
            , "dev"
            , Text.unpack name
            , Text.unpack $ nettxt ipnet
            ]
    (UpWg name) ->
        proc
            "ip"
            [ "link"
            , "set"
            , "up"
            , Text.unpack name
            ]

-------------------------------------------------------------------------------

type Endpoint = Text

type B64PubKey = Text

type AllowedIps = Text

type PortNum = Int

server ::
    Reporter Report ->
    Track' (Binary "wg") ->
    Track' FilePath ->
    Track' WgName ->
    WgName ->
    FilePath ->
    PortNum ->
    Op
server r wg key iface wgname privateKeyPath port =
    withBinary wg wgcommand cmd $ \config ->
        op "wireguard-server" (deps [run key privateKeyPath, run iface wgname]) $ \actions ->
            actions
                { ref = mkRef "wg-server" wgname
                , up = config r'
                }
  where
    cmd = SetupServer wgname port privateKeyPath
    r' = contramap (RunWg cmd) r

client ::
    Reporter Report ->
    Track' (Binary "wg") ->
    Track' FilePath ->
    Track' WgName ->
    WgName ->
    FilePath ->
    Op
client r wg key iface wgname privatekeyPath =
    op "wireguard-client" (deps [justInstall wg, pk, netdev]) $ \actions ->
        actions
            { ref = mkRef "wg-client" (wgname, privatekeyPath)
            , up = do
                let cmd = SetupClient wgname privatekeyPath
                untrackedExec wgcommand cmd "" (r' cmd)
            }
  where
    r' cmd = contramap (RunWg cmd) r
    pk = run key privatekeyPath
    netdev = run iface wgname

type KeepaliveSeconds = Int

-- | Where a peer's public key comes from.
data PeerKey
    = -- | A file some other node provides (the track provisions it); read when the node runs.
      PeerKeyFile (Track' FilePath) FilePath
    | -- | The base64 key itself, e.g. from a generated document that carries it inline.
      PeerKeyValue B64PubKey

peer ::
    Reporter Report ->
    Track' (Binary "wg") ->
    Track' FilePath ->
    Track' WgName ->
    Track' Endpoint ->
    WgName ->
    FilePath ->
    Maybe Endpoint ->
    AllowedIps ->
    Op
peer r wg key iface endpoint wgname publicKeyPath ep ips =
    peerKeepalive r wg key iface endpoint wgname publicKeyPath ep ips Nothing

{- | Like 'peer', but also sets a persistent-keepalive interval. Needed on the
side behind a NAT/dynamic-IP (typically the client) so the tunnel stays
punched through and the far (static-IP) side can keep sending it traffic.
-}
peerKeepalive ::
    Reporter Report ->
    Track' (Binary "wg") ->
    Track' FilePath ->
    Track' WgName ->
    Track' Endpoint ->
    WgName ->
    FilePath ->
    Maybe Endpoint ->
    AllowedIps ->
    Maybe KeepaliveSeconds ->
    Op
peerKeepalive r wg key iface endpoint wgname publicKeyPath =
    peerWith r wg iface endpoint wgname (PeerKeyFile key publicKeyPath)

{- | A peer, with its key given either way ('PeerKey').

* 'check' reads @wg show IF dump@ and compares the peer's public key,
  allowed-ips, endpoint and persistent-keepalive with what is declared (see
  'interpretWgDump'); a peer already as declared is skipped, and one whose
  allowed-ips changed is re-applied (@wg set@ replaces them).
* 'down' is @wg set IF peer KEY remove@, so a peer dropped from a declaration
  leaves the interface and only that peer does.
-}
peerWith ::
    Reporter Report ->
    Track' (Binary "wg") ->
    Track' WgName ->
    Track' Endpoint ->
    WgName ->
    PeerKey ->
    Maybe Endpoint ->
    AllowedIps ->
    Maybe KeepaliveSeconds ->
    Op
peerWith r wg iface endpoint wgname pkey ep ips keepalive =
    op "wireguard-peer" (deps [justInstall wg, pk, netdev, peersetup]) $ \actions ->
        actions
            { help = "wireguard peer on " <> wgname
            , ref = case pkey of
                PeerKeyFile _ path -> mkRef "wg-peer" (wgname, path)
                PeerKeyValue v -> mkRef "wg-peer-value" (wgname, v)
            , check = do
                k <- resolve
                (code, out, _) <- readCreateProcessWithExitCode (prepare wgcommand (ShowDump wgname)) ""
                pure $ case code of
                    ExitSuccess -> interpretWgDump (PeerSpec k ep ips keepalive) (Text.decodeUtf8 out)
                    ExitFailure _ -> Unknown
            , up = do
                k <- resolve
                let cmd = AddPeer wgname k ep ips keepalive
                untrackedExec wgcommand cmd "" (r' cmd)
            , down = do
                k <- resolve
                let cmd = RemovePeer wgname k
                untrackedExec wgcommand cmd "" (r' cmd)
            }
  where
    r' cmd = contramap (RunWg cmd) r
    pk = case pkey of
        PeerKeyFile key path -> run key path
        PeerKeyValue _ -> realNoop
    resolve :: IO B64PubKey
    resolve = case pkey of
        PeerKeyFile _ path -> Text.strip <$> Text.readFile path
        PeerKeyValue v -> pure (Text.strip v)
    netdev = run iface wgname
    peersetup = maybe realNoop (run endpoint) ep

-- | What a peer node declares, for comparison with @wg show ... dump@.
data PeerSpec = PeerSpec
    { specKey :: B64PubKey
    , specEndpoint :: Maybe Endpoint
    , specAllowedIps :: AllowedIps
    , specKeepalive :: Maybe KeepaliveSeconds
    }

{- | The verdict from @wg show IF dump@. The interface line has 4 tab-separated
fields; a peer line has 8: public key, preshared key, endpoint, allowed-ips,
latest handshake, rx, tx, persistent-keepalive (@off@ or seconds).

Fields the declaration leaves out are not compared: @wg set@ without an
endpoint or a keepalive leaves what is there alone, so declaring none of it
is not a statement that it is unset. An endpoint given as a host name is not
compared either, since @wg@ shows the address it resolved to. Allowed-ips are
compared as sets, a bare address counting as its @/32@ (or @/128@).

The reasons name the field and never quote the dump, whose first line is the
interface's private key.
-}
interpretWgDump :: PeerSpec -> Text -> CheckResult
interpretWgDump spec dump =
    case [fields | fields@[k, _, _, _, _, _, _, _] <- Text.splitOn "\t" <$> Text.lines dump, k == spec.specKey] of
        [] -> Failure "peer not on the interface"
        [_, _, endpoint, allowed, _, _, _, keepalive] : _ ->
            case [reason | (False, reason) <- comparisons endpoint allowed keepalive] of
                [] -> Success
                reasons -> Failure (Text.intercalate "; " reasons)
        _ -> Unknown
  where
    comparisons endpoint allowed keepalive =
        [ (normalizeIps allowed == normalizeIps spec.specAllowedIps, "allowed-ips differ")
        , (maybe True (\e -> not (isIpLiteralEndpoint e) || e == endpoint) spec.specEndpoint, "endpoint differs")
        , (maybe True (\k -> keepalive == Text.pack (show k)) spec.specKeepalive, "persistent-keepalive differs")
        ]

normalizeIps :: Text -> Set.Set Text
normalizeIps = Set.fromList . filter (/= "(none)") . fmap norm . Text.splitOn "," . Text.filter (/= ' ')
  where
    norm ip
        | Text.null ip = ip
        | Text.any (== '/') ip = ip
        | Text.any (== ':') ip = ip <> "/128"
        | otherwise = ip <> "/32"

-- | @1.2.3.4:51820@ or @[::1]:51820@, as opposed to @host.example:51820@.
isIpLiteralEndpoint :: Endpoint -> Bool
isIpLiteralEndpoint e
    | "[" `Text.isPrefixOf` e = True
    | otherwise = not (Text.null host) && Text.all (`elem` ("0123456789." :: String)) host
  where
    host = Text.takeWhile (/= ':') e

data WgCommand
    = SetupServer WgName PortNum FilePath
    | SetupClient WgName FilePath
    | AddPeer WgName B64PubKey (Maybe Endpoint) AllowedIps (Maybe KeepaliveSeconds)
    | RemovePeer WgName B64PubKey
    | ShowDump WgName
    deriving (Show)

wgcommand :: Command "wg" WgCommand
wgcommand = Command $ \cmd -> case cmd of
    (SetupServer name port key) ->
        proc
            "wg"
            [ "set"
            , Text.unpack name
            , "listen-port"
            , show port
            , "private-key"
            , key
            ]
    (SetupClient name key) ->
        proc
            "wg"
            [ "set"
            , Text.unpack name
            , "private-key"
            , key
            ]
    (AddPeer name b64pk ep allowedIps keepalive) ->
        proc "wg" $
            mconcat
                [
                    [ "set"
                    , Text.unpack name
                    , "peer"
                    , Text.unpack b64pk
                    ]
                , maybe [] (\e -> ["endpoint", Text.unpack e]) ep
                , ["allowed-ips", Text.unpack allowedIps]
                , maybe [] (\k -> ["persistent-keepalive", show k]) keepalive
                ]
    (RemovePeer name b64pk) ->
        proc "wg" ["set", Text.unpack name, "peer", Text.unpack b64pk, "remove"]
    (ShowDump name) ->
        proc "wg" ["show", Text.unpack name, "dump"]
