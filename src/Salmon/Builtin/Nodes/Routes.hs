module Salmon.Builtin.Nodes.Routes where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Data.Text (Text)
import qualified Data.Text as Text

import System.Process.ListLike (proc)

-------------------------------------------------------------------------------
data Report
    = RunIp !IpCommand !Binary.Report

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

route ::
    Reporter Report ->
    Track' (Binary "ip") ->
    Route ->
    Op
route r ip netroute =
    withBinary ip ipcommand cmd $ \addroute ->
        op "ip-route" nodeps $ \actions ->
            actions
                { help = mconcat ["route ", dst, " via ", netroute.routeDevice, "(", netroute.routeVia, ")"]
                , ref = dotRef $ "ip-route" <> netroute.routeDevice <> netroute.routeVia
                , up = addroute r'
                }
  where
    cmd = AddRoute netroute
    r' = contramap (RunIp cmd) r
    dst = case netroute.routeDestination of Default -> "default"; RawNetwork dst -> dst

data IpCommand
    = AddRoute Route

ipcommand :: Command "ip" IpCommand
ipcommand = Command $ \cmd -> case cmd of
    (AddRoute (Route Default name gw)) ->
        proc
            "ip"
            [ "route"
            , "add"
            , "default"
            , "dev"
            , Text.unpack name
            , "via"
            , Text.unpack gw
            ]
    (AddRoute (Route (RawNetwork dst) name gw)) ->
        proc
            "ip"
            [ "route"
            , "add"
            , Text.unpack dst
            , "dev"
            , Text.unpack name
            , "via"
            , Text.unpack gw
            ]
