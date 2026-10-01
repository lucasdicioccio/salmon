{-# LANGUAGE OverloadedStrings #-}

{- | Ask the LAN's gateway (UPnP-IGD, through @upnpc@ from @miniupnpc@) to
forward an external port to this host, and keep it that way.

There is no "set" verb, so this is the 'Salmon.Builtin.Nodes.Netfilter.rule'
case: 'check' lists the gateway's mappings and 'up' adds one only if the
listing does not already say it is in place. Routers forget mappings on reboot
and a timed lease expires; the 'check' notices, and under @run serve@ the
tending loop re-asserts it.

Things worth knowing:

* __The mapping is the gateway's, not ours__: it is identified by
  (protocol, external port), which is also this node's 'Ref'. A mapping on that
  port pointing at /another/ host is refused, never taken over.
* __'down' deletes only a mapping carrying our marker__ (@salmon:NAME@ in the
  description); anything else on the router is not ours.
* __No gateway__ (nothing answered, or a device that is not an IGD) is
  'Unknown' for 'check': a rebooting or reconfigured router is what fixes it,
  and a loop that keeps looking is the right reaction. 'up' and 'down' throw.
* __Double NAT__: if the gateway's own external address is private or CGNAT, a
  mapping cannot be reached from the Internet. 'check' says so as a 'Failure'
  and 'up' refuses, rather than reporting success for a mapping nobody can use.
* __Security__: UPnP has no authentication on the LAN side and opens a hole to
  the Internet. This is opt-in per node, one port per node, and no recipe
  should add it by default.

'up' and 'down' verify their own effect by listing again, because @upnpc@'s
exit status is not trusted to reflect a gateway's refusal.

Only @upnpc@ is implemented. NAT-PMP\/PCP (@natpmpc@) is a follow-up.
-}
module Salmon.Builtin.Nodes.PortMapping (
    PortMap (..),
    Report (..),
    portMapping,
    marker,
    Plan (..),
    planFor,
    checkOf,
    TeardownPlan (..),
    teardownFor,
    PortMappingRefused (..),
    UpnpcCommand (..),
    upnpcCommand,
    gatewayExternalAddress,
    module Salmon.Builtin.Nodes.PortMapping.Upnpc,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Dynamic (toDyn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (proc)

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.PortMapping.Upnpc
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunUpnpc !UpnpcCommand !Binary.Report
    deriving (Show)

-- | One mapping to keep on the gateway.
data PortMap = PortMap
    { portMapName :: Text
    -- ^ identifies us in the description (@salmon:NAME@)
    , portMapInternalAddress :: Text
    -- ^ this host's LAN address (not detected: the author states it)
    , portMapInternalPort :: Int
    , portMapExternalPort :: Int
    , portMapProtocol :: Proto
    , portMapLeaseSeconds :: Int
    -- ^ must be positive: a permanent lease is not portable across routers
    }
    deriving (Show)

-- | The text a mapping we made carries as its description.
marker :: PortMap -> Text
marker pm = "salmon:" <> pm.portMapName

-------------------------------------------------------------------------------

data UpnpcCommand
    = Add PortMap
    | Delete PortMap
    deriving (Show)

upnpcCommand :: Command "upnpc" UpnpcCommand
upnpcCommand = Command $ \cmd -> case cmd of
    Add pm ->
        proc "upnpc" $
            [ "-e"
            , Text.unpack (marker pm)
            , "-a"
            , Text.unpack pm.portMapInternalAddress
            , show pm.portMapInternalPort
            , show pm.portMapExternalPort
            , Text.unpack (protoText pm.portMapProtocol)
            , show pm.portMapLeaseSeconds
            ]
    Delete pm ->
        proc
            "upnpc"
            [ "-d"
            , show pm.portMapExternalPort
            , Text.unpack (protoText pm.portMapProtocol)
            ]

-------------------------------------------------------------------------------

-- | What the gateway's listing says about what 'up' has to do.
data Plan
    = -- | in place, with enough lease left
      InPlace
    | -- | absent, stale or about to expire: (re-)add it
      NeedsAdd Text
    | -- | somebody else's mapping holds the external port
      Taken Text
    | -- | the gateway is itself behind a NAT
      DoubleNat Text
    | -- | no Internet Gateway Device
      NoGateway Text
    deriving (Eq, Show)

-- | Pure: the listing (@upnpc -l@, stdout and stderr together) read against a declared mapping.
planFor :: PortMap -> Text -> Plan
planFor pm out = case parseGateway out of
    NoDevice -> NoGateway "no Internet Gateway Device answered; the router may be rebooting or UPnP may be unreachable"
    NotIgd -> NoGateway "a UPnP device answered but it is not an Internet Gateway Device; UPnP may be disabled on the router"
    Igd ext _ -> case classified ext of
        Just reason -> DoubleNat reason
        _ -> case existing of
            Nothing -> NeedsAdd (describe "is not mapped")
            Just m
                | m.mapInternalAddress /= pm.portMapInternalAddress || m.mapInternalPort /= pm.portMapInternalPort ->
                    if m.mapDescription == marker pm
                        then NeedsAdd (describe ("points at " <> target m <> " instead"))
                        else Taken (describe ("is already mapped to " <> target m <> " by " <> quoted m.mapDescription))
                | expiring m -> NeedsAdd (describe "is about to expire")
                | otherwise -> InPlace
  where
    describe what = Text.unwords [protoText pm.portMapProtocol, Text.pack (show pm.portMapExternalPort), what]
    target :: Mapping -> Text
    target m = m.mapInternalAddress <> ":" <> Text.pack (show m.mapInternalPort)
    quoted d = if Text.null d then "an unnamed mapping" else "'" <> d <> "'"
    existing =
        listToMaybe
            [ m
            | m <- parseMappings out
            , m.mapProto == pm.portMapProtocol
            , m.mapExternalPort == pm.portMapExternalPort
            ]
    -- remaining (or original) lease at or under a quarter of the declared one
    expiring :: Mapping -> Bool
    expiring m = m.mapLease /= 0 && m.mapLease * 4 <= pm.portMapLeaseSeconds
    classified ext = case classifyAddress <$> ext of
        Just Private -> Just ("the gateway's external address is private (" <> shown ext <> "): behind another NAT, the mapping cannot be reached from the Internet")
        Just CGNAT -> Just ("the gateway's external address is carrier-grade NAT (" <> shown ext <> "): the mapping cannot be reached from the Internet")
        _ -> Nothing
    shown = maybe "?" id

-- | The verdict a 'Plan' is for 'check'.
checkOf :: Plan -> CheckResult
checkOf InPlace = Success
checkOf (NeedsAdd r) = Failure r
checkOf (Taken r) = Failure r
checkOf (DoubleNat r) = Failure r
checkOf (NoGateway _) = Unknown

-- | What 'down' has to do.
data TeardownPlan
    = AlreadyGone
    | DeleteIt
    | NotOurs Text
    | CannotTell Text
    deriving (Eq, Show)

teardownFor :: PortMap -> Text -> TeardownPlan
teardownFor pm out = case parseGateway out of
    NoDevice -> CannotTell "no Internet Gateway Device answered"
    NotIgd -> CannotTell "a UPnP device answered but it is not an Internet Gateway Device"
    Igd _ _ -> case [m | m <- parseMappings out, m.mapProto == pm.portMapProtocol, m.mapExternalPort == pm.portMapExternalPort] of
        [] -> AlreadyGone
        (m : _)
            | m.mapDescription == marker pm -> DeleteIt
            | otherwise -> NotOurs ("refusing to delete mapping '" <> m.mapDescription <> "' on external port " <> Text.pack (show pm.portMapExternalPort) <> ": it does not carry our marker " <> marker pm)

-- | Thrown by 'up' and 'down' when they must not (or cannot) proceed.
newtype PortMappingRefused = PortMappingRefused Text
    deriving (Show)

instance Exception PortMappingRefused

-------------------------------------------------------------------------------

-- | Runs @upnpc -l@ and hands back everything it printed, whatever its exit status (a missing gateway exits non-zero and is an answer, not an error).
listing :: IO Text
listing = do
    (_code, out, err) <- readCreateProcessWithExitCode (proc "upnpc" ["-l"]) ""
    pure (dec out <> "\n" <> dec err)
  where
    dec = Text.decodeUtf8With Text.lenientDecode

{- | The external address the gateway reports, if there is an IGD (for a DNS
registration node that would rather not guess).
-}
gatewayExternalAddress :: IO (Maybe Text)
gatewayExternalAddress = do
    out <- Text.decodeUtf8With Text.lenientDecode . (\(_, o, e) -> o <> "\n" <> e) <$> readCreateProcessWithExitCode (proc "upnpc" ["-s"]) ""
    pure $ case parseGateway out of
        Igd ext _ -> ext
        _ -> Nothing

-- | Maps one port on the gateway. See the module header.
portMapping :: Reporter Report -> Track' (Binary "upnpc") -> PortMap -> Op
portMapping r upnpc pm =
    withBinary upnpc upnpcCommand (Add pm) $ \add ->
        withBinary upnpc upnpcCommand (Delete pm) $ \del ->
            op "upnp-port-mapping" nodeps $ \actions ->
                actions
                    { help =
                        Text.unwords
                            [ "maps"
                            , protoText pm.portMapProtocol
                            , "external port"
                            , Text.pack (show pm.portMapExternalPort)
                            , "on the gateway to"
                            , pm.portMapInternalAddress <> ":" <> Text.pack (show pm.portMapInternalPort)
                            , "(opens a port to the Internet)"
                            ]
                    , notes =
                        [ "UPnP is unauthenticated on the LAN side: this opens one port to the Internet"
                        , "description on the gateway: " <> marker pm
                        ]
                    , ref = mkRef "upnp-map" (protoText pm.portMapProtocol, pm.portMapExternalPort)
                    , check = checkOf . planFor pm <$> listing
                    , up = do
                        when (pm.portMapLeaseSeconds <= 0) $
                            throwIO (PortMappingRefused "the lease must be positive: a permanent lease is not portable across routers")
                        before <- planFor pm <$> listing
                        case before of
                            InPlace -> pure ()
                            NeedsAdd _ -> do
                                add (contramap (RunUpnpc (Add pm)) r)
                                after <- planFor pm <$> listing
                                when (after /= InPlace) $
                                    throwIO (PortMappingRefused ("the gateway did not take the mapping: " <> Text.pack (show after)))
                            Taken why -> throwIO (PortMappingRefused why)
                            DoubleNat why -> throwIO (PortMappingRefused why)
                            NoGateway why -> throwIO (PortMappingRefused why)
                    , down = do
                        before <- teardownFor pm <$> listing
                        case before of
                            AlreadyGone -> pure ()
                            DeleteIt -> do
                                del (contramap (RunUpnpc (Delete pm)) r)
                                after <- teardownFor pm <$> listing
                                when (after /= AlreadyGone) $
                                    throwIO (PortMappingRefused "the gateway still lists the mapping after deleting it")
                            NotOurs why -> throwIO (PortMappingRefused why)
                            CannotTell why -> throwIO (PortMappingRefused why)
                    , dynamics = [toDyn pm]
                    }
