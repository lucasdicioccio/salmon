module Salmon.Builtin.Nodes.WireGuard where

import Salmon.Actions.UpDown (skipIfFileExists)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), CommandIO (..), justInstall, untrackedExec, withBinary, withBinaryIO)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import System.IO (IOMode (ReadMode, WriteMode), withFile)

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.IO.Handle (Handle)

import System.FilePath (takeDirectory, (</>))
import System.Process (StdStream (UseHandle), waitForProcess)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess (..), proc)

-------------------------------------------------------------------------------
data Report
    = RunWg !WgCommand !Binary.Report
    | RunIp !IpCommand !Binary.Report

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
                , ref = dotRef $ "wg:write-pk" <> Text.pack path
                , prelim = skipIfFileExists path
                , up = withFile path WriteMode $ \h -> do
                    (_, _, _, ph) <- writePK (PrivateKeyForWriting h)
                    void $ waitForProcess ph
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
                , ref = dotRef $ "wg:write-public-pk" <> Text.pack path
                , prelim = skipIfFileExists path
                , up =
                    withFile private ReadMode $ \hIn ->
                        withFile path WriteMode $ \hOut -> do
                            (_, _, _, ph) <- writePK ((PrivateKeyForReading hIn), (PublicKeyForWriting hOut))
                            void $ waitForProcess ph
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
                        { ref = dotRef $ "wg-iface" <> wg
                        , up = addwg >> setAddr >> activate
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

data IpCommand
    = AddWg WgName
    | SetWgAddr WgName IpNet
    | UpWg WgName

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
            , "add"
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
                { ref = dotRef $ "wg-server" <> wgname
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
            { ref = dotRef $ "wg-client" <> wgname <> Text.pack privatekeyPath
            , up = do
                let cmd = SetupClient wgname privatekeyPath
                untrackedExec wgcommand cmd "" (r' cmd)
            }
  where
    r' cmd = contramap (RunWg cmd) r
    pk = run key privatekeyPath
    netdev = run iface wgname

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
    op "wireguard-peer" (deps [justInstall wg, pk, netdev, peersetup]) $ \actions ->
        actions
            { ref = dotRef $ "wg-peer" <> wgname <> Text.pack publicKeyPath
            , up = do
                pkey <- Text.strip <$> Text.readFile publicKeyPath
                print (wgname, publicKeyPath, pkey)
                let cmd = AddPeer wgname pkey ep ips
                untrackedExec wgcommand cmd "" (r' cmd)
            }
  where
    r' cmd = contramap (RunWg cmd) r
    pk = run key publicKeyPath
    netdev = run iface wgname
    peersetup = maybe realNoop (run endpoint) ep

data WgCommand
    = SetupServer WgName PortNum FilePath
    | SetupClient WgName FilePath
    | AddPeer WgName B64PubKey (Maybe Endpoint) AllowedIps

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
    (AddPeer name b64pk (Just ep) allowedIps) ->
        proc
            "wg"
            [ "set"
            , Text.unpack name
            , "peer"
            , Text.unpack b64pk
            , "endpoint"
            , Text.unpack ep
            , "allowed-ips"
            , Text.unpack allowedIps
            ]
    (AddPeer name b64pk Nothing allowedIps) ->
        proc
            "wg"
            [ "set"
            , Text.unpack name
            , "peer"
            , Text.unpack b64pk
            , "allowed-ips"
            , Text.unpack allowedIps
            ]
