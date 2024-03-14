module Salmon.Builtin.Nodes.WireGuard where

import Salmon.Actions.UpDown (skipIfFileExists)
import Salmon.Op.Ref
import Salmon.Builtin.Extension
import Salmon.Op.Track
import Salmon.Builtin.Nodes.Binary
import qualified Salmon.Builtin.Nodes.Filesystem as FS

import System.IO (withFile, IOMode(ReadMode,WriteMode))

import GHC.IO.Handle (Handle)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import System.FilePath ((</>), takeDirectory)
import System.Process.ListLike (CreateProcess(..), proc)
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process (StdStream(UseHandle), waitForProcess)

-------------------------------------------------------------------------------
newtype PrivateKeyForWriting = PrivateKeyForWriting { getWritePkHandle :: Handle }

newtype PublicKeyForWriting = PublicKeyForWriting { getWritePubPkHandle :: Handle }

newtype PrivateKeyForReading = PrivateKeyForReading { getReadPkHandle :: Handle }

privateKey
  :: Track' (Binary "wg")
  -> FilePath
  -> Op
privateKey wg path =
  withBinaryIO wg genkeycommand GenKey $ \writePK ->
  op "wg-private-key" (deps [enclosingdir]) $ \actions -> actions {
    help = "privkey at " <> Text.pack path
  , ref = dotRef $ "wg:write-pk" <> Text.pack path
  , prelim = skipIfFileExists path
  , up = withFile path WriteMode $ \h -> do
      (_,_,_,ph) <- writePK (PrivateKeyForWriting h)
      void $ waitForProcess ph
  }
  where
    enclosingdir :: Op
    enclosingdir = FS.dir (FS.Directory $ takeDirectory path)

publicKey
  :: Track' (Binary "wg")
  -> Track' FilePath
  -> FilePath
  -> FilePath
  -> Op
publicKey wg mkprivate private path =
  withBinaryIO wg pubkeycommand PubKey $ \writePK ->
  op "wg-public-key" (deps [run mkprivate private, enclosingdir]) $ \actions -> actions {
    help = "pubkey at " <> Text.pack path
  , ref = dotRef $ "wg:write-public-pk" <> Text.pack path
  , prelim = skipIfFileExists path
  , up =
      withFile private ReadMode $ \hIn ->
      withFile path WriteMode $ \hOut -> do
        (_,_,_,ph) <- writePK ((PrivateKeyForReading hIn), (PublicKeyForWriting hOut))
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
    pure ((proc "wg" [ "genkey" ]) { std_out = UseHandle (getWritePkHandle h) })

pubkeycommand :: CommandIO "wg" PubKeyCommand (PrivateKeyForReading, PublicKeyForWriting)
pubkeycommand = CommandIO $ \cmd -> case cmd of
  PubKey -> \(hin,hout) -> do
    pure ((proc "wg" [ "pubkey" ]) { std_in = UseHandle (getReadPkHandle hin) , std_out = UseHandle (getWritePubPkHandle hout) })

-------------------------------------------------------------------------------
type Ipv4 = Text
type Ipv4PrefixSize = Int

data IpNet
  = Ipv4Cidr Ipv4 Ipv4PrefixSize

iptxt :: IpNet -> Text
iptxt (Ipv4Cidr ipv4 cidr) = ipv4 <> "/" <> Text.pack (show cidr)

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

iface
  :: Track' (Binary "ip")
  -> WgName
  -> IpNet
  -> Op
iface ip wg net =
  withBinary ip ipcommand (AddWg wg) $ \addwg ->
  withBinary ip ipcommand (SetWgAddr wg net) $ \setAddr ->
  withBinary ip ipcommand (UpWg wg) $ \activate ->
  op "wireguard-iface" nodeps $ \actions -> actions {
    ref = dotRef $ "wg-iface" <> wg
  , up = addwg >> setAddr >> activate
  }

data IpCommand
  = AddWg WgName
  | SetWgAddr WgName IpNet
  | UpWg WgName

ipcommand :: Command "ip" IpCommand
ipcommand = Command $ \cmd -> case cmd of
  (AddWg name) ->
    proc "ip"
      [ "link"
      , "add", "dev", Text.unpack name, "type", "wireguard"
      ]
  (SetWgAddr name ipnet) ->
    proc "ip"
      [ "address"
      , "add", "dev", Text.unpack name, Text.unpack $ iptxt ipnet
      ]
  (UpWg name) ->
    proc "ip"
      [ "link"
      , "set", "up", Text.unpack name
      ]

-------------------------------------------------------------------------------

type Endpoint = Text

type B64PubKey = Text

type AllowedIps = Text

type PortNum = Int


server
  :: Track' (Binary "wg")
  -> Track' FilePath
  -> Track' WgName
  -> WgName
  -> FilePath
  -> PortNum
  -> Op
server wg key iface wgname privateKeyPath port =
  withBinary wg wgcommand (SetupServer wgname port privateKeyPath) $ \config ->
  op "wireguard-server" (deps [run key privateKeyPath, run iface wgname]) $ \actions -> actions {
    ref = dotRef $ "wg-server" <> wgname
  , up = config
  }

client
  :: Track' (Binary "wg")
  -> Track' FilePath
  -> Track' WgName
  -> WgName
  -> FilePath
  -> Op
client wg key iface wgname privatekeyPath =
  op "wireguard-client" (deps [justInstall wg, pk, netdev]) $ \actions -> actions {
    ref = dotRef $ "wg-client" <> wgname <> Text.pack privatekeyPath
  , up = do
      untrackedExec wgcommand (SetupClient wgname privatekeyPath) ""
  }
  where
    pk = run key privatekeyPath
    netdev = run iface wgname

peer
  :: Track' (Binary "wg")
  -> Track' FilePath
  -> Track' WgName
  -> Track' Endpoint
  -> WgName
  -> FilePath
  -> Maybe Endpoint
  -> AllowedIps
  -> Op
peer wg key iface endpoint wgname publicKeyPath ep ips =
  op "wireguard-peer" (deps [justInstall wg, pk, netdev, peersetup]) $ \actions -> actions {
    ref = dotRef $ "wg-peer" <> wgname <> Text.pack publicKeyPath
  , up = do
      pkey <- Text.strip <$> Text.readFile publicKeyPath
      print (wgname, publicKeyPath, pkey)
      untrackedExec wgcommand (AddPeer wgname pkey ep ips) ""
  }
  where
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
    proc "wg"
      [ "set", Text.unpack name
      , "listen-port", show port
      , "private-key", key
      ]
  (SetupClient name key) ->
    proc "wg"
      [ "set", Text.unpack name
      , "private-key", key
      ]
  (AddPeer name b64pk (Just ep) allowedIps) ->
    proc "wg"
      [ "set", Text.unpack name
      , "peer", Text.unpack b64pk
      , "endpoint", Text.unpack ep
      , "allowed-ips", Text.unpack allowedIps
      ]
  (AddPeer name b64pk Nothing allowedIps) ->
    proc "wg"
      [ "set", Text.unpack name
      , "peer", Text.unpack b64pk
      , "allowed-ips", Text.unpack allowedIps
      ]
