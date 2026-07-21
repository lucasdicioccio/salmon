module Salmon.Builtin.Nodes.Routes where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Data.Text (Text)
import qualified Data.Text as Text

import System.Process (readProcess)
import System.Process.ListLike (proc)

-------------------------------------------------------------------------------
data Report
    = RunIp !IpCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

type DevName = Text

type GatewayAddr = Text

data Route
    = Route
    { routeDestination :: DestinationNetwork
    , routeDevice :: DevName
    , routeVia :: Maybe GatewayAddr
    }
    deriving (Show)

data DestinationNetwork
    = Default
    | RawNetwork Text
    deriving (Show)

{- | Idempotently sets a route (uses @ip route replace@, which unlike @ip route add@
does not fail when the route already exists).
-}
route ::
    Reporter Report ->
    Track' (Binary "ip") ->
    Route ->
    Op
route r ip netroute =
    withBinary ip ipcommand cmd $ \addroute ->
        op "ip-route" nodeps $ \actions ->
            actions
                { help = mconcat ["route ", dst, " dev ", netroute.routeDevice, via]
                , ref = mkRef "ip-route" (netroute.routeDevice, netroute.routeVia)
                , up = addroute r'
                }
  where
    cmd = ReplaceRoute netroute
    r' = contramap (RunIp cmd) r
    dst = case netroute.routeDestination of Default -> "default"; RawNetwork d -> d
    via = maybe "" (\gw -> " via " <> gw) netroute.routeVia

data IpCommand
    = ReplaceRoute Route
    deriving (Show)

ipcommand :: Command "ip" IpCommand
ipcommand = Command $ \cmd -> case cmd of
    (ReplaceRoute (Route dstnet name via)) ->
        proc "ip" $
            mconcat
                [ ["route", "replace", dst]
                , ["dev", Text.unpack name]
                , maybe [] (\gw -> ["via", Text.unpack gw]) via
                ]
      where
        dst = case dstnet of Default -> "default"; RawNetwork d -> Text.unpack d

{- | Discovers the (gateway, device) the kernel currently uses to reach a given
host, by shelling out to @ip route get@. Useful to pin a host route to its
current path before overriding the default route (e.g. so VPN client traffic
destined to the VPN server's own endpoint keeps using the physical uplink).
-}
discoverGatewayFor :: Text -> IO (Maybe GatewayAddr, DevName)
discoverGatewayFor host = do
    out <- readProcess "ip" ["route", "get", Text.unpack host] ""
    let ws = Text.words (Text.pack out)
    pure (findAfter "via" ws, maybe (error $ "no `dev` in `ip route get " <> Text.unpack host <> "` output") id (findAfter "dev" ws))
  where
    findAfter tok (a : b : rest)
        | a == tok = Just b
        | otherwise = findAfter tok (b : rest)
    findAfter _ _ = Nothing
