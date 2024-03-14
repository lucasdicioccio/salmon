module Salmon.Builtin.Nodes.Routes where

import Salmon.Op.Ref
import Salmon.Builtin.Extension
import Salmon.Op.Track
import Salmon.Builtin.Nodes.Binary

import Data.Text (Text)
import qualified Data.Text as Text

import System.Process.ListLike (proc)

-------------------------------------------------------------------------------

type DevName = Text

type GatewayAddr = Text

data Route
  = Route
  { routeDestination :: DestinationNetwork 
  , routeDevice :: DevName
  , routeVia :: GatewayAddr
  }

data DestinationNetwork
  = Default
  | RawNetwork Text

route
  :: Track' (Binary "ip")
  -> Route
  -> Op
route ip r =
  withBinary ip ipcommand (AddRoute r) $ \addroute ->
  op "ip-route" nodeps $ \actions -> actions {
    help = mconcat ["route ", dst , " via " , r.routeDevice , "(", r.routeVia, ")"]
  , ref = dotRef $ "ip-route" <> r.routeDevice <> r.routeVia
  , up = addroute
  }
  where
    dst = case r.routeDestination of Default -> "default" ; RawNetwork dst -> dst

data IpCommand
  = AddRoute Route

ipcommand :: Command "ip" IpCommand
ipcommand = Command $ \cmd -> case cmd of
  (AddRoute (Route Default name gw)) ->
    proc "ip"
      [ "route"
      , "add", "default", "dev", Text.unpack name, "via", Text.unpack gw
      ]
  (AddRoute (Route (RawNetwork dst) name gw)) ->
    proc "ip"
      [ "route"
      , "add", Text.unpack dst, "dev", Text.unpack name, "via", Text.unpack gw
      ]
