{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A throwaway, tiered exercise of the "Salmon.Builtin.Nodes.Gcp" builtins
against a real GCP project, meant to be run in a sandbox organization or
billing account where deleting everything afterwards is the point.

Tiers are cumulative and ordered by cost:

* __tier 0__ (≈free): project (optional), billing link, APIs, a bucket, a
  service account, IAM bindings on both, an Artifact Registry repository.
* __tier 1__ (cents): build an image with podman, push it to the
  repository, deploy it to Cloud Run as that service account. The image is
  either a one-line @FROM --base-image@ (Google's hello sample by default) or
  the caller's own @--containerfile@, built with that file's directory as
  context; either way it must serve HTTP on @$PORT@, as Cloud Run requires.
* __tier 2__ (an e2-micro's hourly rate): reserve an address, open ssh to a
  tagged instance, boot a VM whose startup script trusts a salmon-generated
  SSH CA, then upload this very binary and run it there over that CA --
  "SreBox.Gcp.VmProvision", i.e. @specs/gcloud-support.md@ §6's "objective".
* __tier 3__ (a forwarding rule's hourly rate on top): put a regional
  external Application Load Balancer in front of that VM -- a proxy-only
  subnet, an unmanaged instance group holding the instance, a health check,
  and the balancer itself. The VM serves the page through a systemd unit the
  /tier-2 hand-off/ installed, so a @200@ from the balancer's address is
  evidence for both halves at once.

Tier 2 optionally declares a __peer__ (@--vm-internal-ip@ with
@--peer-internal-ip@): a second instance with no external address at all,
both instances pinned to the internal addresses given, and the VM-side
payload fetching a page from the peer on that address. Unlike the external
address below, these are known when the graph is declared, so the firewall
rule that admits the VM by its @\/32@ and the URL the VM fetches are written
in the same pass that creates the machines.

Tier 2 is __one pass__. GCP picks the address, so nothing can name the
machine until after the address node's @up@, and an 'Op' naming it cannot be
built at declaration: the part that has to (the ssh probe, the secret upload,
the remote call) is therefore built inside a node's @up@, from the address
read then ('VmProvision.provisionedVmReadingHost'). @--vm-ip@ is still
accepted and declares that part as ordinary nodes, which is what the driver
script's second pass does and what shows them in @run tree@.

When the project is created by this binary (the default), it is the deepest
node of the graph, so @run down@ tears every resource down individually
first -- which is what is being validated -- and then deletes the project,
which sweeps whatever a buggy @down@ left behind. A failed @down@ leaves the
project standing (its dependants were not all removed), which is the signal
to go and look.

See @salmon-apps/scripts/gcp-toy-validate.sh@ for the up → up → down driver.
-}
module GcpToy (
    main,
    Seed (..),
    Spec (..),
    ParentRef (..),
    Role (..),
    VmConfig (..),
    LbConfig (..),
    PeerConfig (..),
    ImageSource (..),
    defaultBaseImage,
    configure,
    program,
) where

import Control.Exception (throwIO)
import Control.Monad (when)
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiLower, isDigit)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Options.Applicative (auto, execParser, flag', fullDesc, header, helper, info, long, metavar, option, optional, progDesc, strOption, switch, value, (<**>), (<|>))
import qualified Options.Applicative as Opt
import Options.Generic (ParseRecord (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import System.FilePath (takeDirectory)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import qualified Salmon.Actions.UpDown as UpDown
import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Gcp.ArtifactRegistry as ArtifactRegistry
import qualified Salmon.Builtin.Nodes.Gcp.Billing as Billing
import qualified Salmon.Builtin.Nodes.Gcp.CloudDns as CloudDns
import qualified Salmon.Builtin.Nodes.Gcp.CloudRun as CloudRun
import qualified Salmon.Builtin.Nodes.Gcp.Core as Core
import qualified Salmon.Builtin.Nodes.Gcp.Iam as Iam
import qualified Salmon.Builtin.Nodes.Gcp.Monitoring as Monitoring
import qualified Salmon.Builtin.Nodes.Gcp.ResourceManager as ResourceManager
import qualified Salmon.Builtin.Nodes.Gcp.ServiceUsage as ServiceUsage
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage
import qualified Salmon.Builtin.Nodes.Debian.OS as OS
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Gcp.Compute as Compute
import qualified Salmon.Builtin.Nodes.Gcp.LoadBalancing as LoadBalancing
import qualified Salmon.Builtin.Nodes.Gcp.SshAccess as SshAccess
import qualified Salmon.Builtin.Nodes.Systemd as Systemd
import qualified Salmon.Builtin.Nodes.Keys as Keys
import qualified Salmon.Builtin.Nodes.Podman as Podman
import qualified Salmon.Builtin.Nodes.SecretDelivery as SecretDelivery
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (reportPrint, silent)

import qualified SreBox.Gcp.CloudRunAlerts as CloudRunAlerts
import qualified SreBox.Gcp.CloudRunDeploy as CloudRunDeploy
import qualified SreBox.Gcp.VmProvision as VmProvision

main :: IO ()
main = do
    let desc = fullDesc <> progDesc "Tiered throwaway validation of salmon's GCP builtins" <> header "salmon-gcp-toy"
    cmd <- execParser (info parseRecord desc)
    CLI.execCommandOrSeed reportPrint configure program cmd

-------------------------------------------------------------------------------
-- Seed

data SeedParent = SeedOrganization Text | SeedFolder Text | SeedNoParent | SeedExistingProject
    deriving (Eq, Show)

data Seed = Seed
    { seedProject :: Text
    , seedParent :: SeedParent
    , seedBillingAccount :: Maybe Text
    , seedRegion :: Text
    , seedTier :: Int
    , seedPrefix :: Text
    , seedImageTag :: Text
    , seedImageSource :: ImageSource
    , seedWorkDir :: FilePath
    , seedVmZone :: Maybe Text
    , seedVmMachineType :: Text
    , seedVmImageFamily :: Text
    , seedVmImageProject :: Text
    , seedVmUser :: Text
    , seedSshSourceRange :: Text
    , seedVmIp :: Maybe Text
    , seedLbProxyRange :: Text
    , seedLbPort :: Int
    , seedAlertEmail :: Maybe Text
    , seedVmInternalIp :: Maybe Text
    , seedPeerInternalIp :: Maybe Text
    , seedDnsZone :: Maybe Text
    , seedAccount :: Maybe Text
    , seedLbHttps :: Bool
    }
    deriving (Eq, Show)

instance ParseRecord Seed where
    parseRecord =
        build <**> helper
      where
        build =
            Seed
                <$> strOption (long "project" <> metavar "PROJECT_ID" <> Opt.help "project id to create (or use, with --existing-project)")
                <*> parent
                <*> optional (strOption (long "billing-account" <> metavar "XXXXXX-XXXXXX-XXXXXX" <> Opt.help "billing account to link; required unless --existing-project"))
                <*> strOption (long "region" <> value "europe-west1" <> Opt.help "region for the bucket, repository and Cloud Run service")
                <*> option auto (long "tier" <> value 0 <> Opt.help "0: storage/iam/registry; 1: also build+push+deploy to Cloud Run (needs podman)")
                <*> strOption (long "prefix" <> value "salmon-toy" <> Opt.help "prefix for every resource name")
                <*> strOption (long "image-tag" <> value "v1" <> Opt.help "tier 1 image tag; bump it to exercise a redeploy")
                <*> imageSourceP
                <*> strOption (long "workdir" <> value "gcp-toy-work" <> Opt.help "local directory for the Containerfile, authfile, ssh keys and startup script")
                <*> optional (strOption (long "vm-zone" <> Opt.help "tier 2 zone (default: <region>-b)"))
                <*> strOption (long "vm-machine-type" <> value "e2-micro" <> Opt.help "tier 2 machine type")
                <*> strOption (long "vm-image-family" <> value "ubuntu-2404-lts-amd64" <> Opt.help "tier 2 boot image family")
                <*> strOption (long "vm-image-project" <> value "ubuntu-os-cloud" <> Opt.help "tier 2 boot image project")
                <*> strOption (long "vm-user" <> value "salmon" <> Opt.help "tier 2 login user, and the certificate principal signed for it")
                <*> strOption (long "ssh-source-range" <> value "0.0.0.0/0" <> Opt.help "tier 2 CIDR allowed to reach port 22")
                <*> optional (strOption (long "vm-ip" <> Opt.help "tier 2 second pass: the reserved IP, which a first pass cannot know"))
                -- The default is outside 10.128.0.0/9 on purpose: the whole
                -- of that block belongs to the subnets an auto-mode network
                -- (which `default` is) creates per region on its own,
                -- including for regions that do not exist yet.
                <*> strOption (long "lb-proxy-range" <> value "192.168.100.0/24" <> Opt.showDefault <> Opt.help "tier 3 proxy-only subnet range (/26 or larger, must not overlap 10.128.0.0/9)")
                <*> option auto (long "lb-port" <> value (8080 :: Int) <> Opt.showDefault <> Opt.help "tier 3 port the VM serves on, behind the balancer")
                <*> optional (strOption (long "alert-email" <> metavar "ADDRESS" <> Opt.help "tier 1: also declare the standard Cloud Monitoring alerts on the service, to this email (SreBox.Gcp.CloudRunAlerts)"))
                -- No default: the range of the `default` network's subnet
                -- differs per region (10.132.0.0/20 in europe-west1), so the
                -- caller has to look it up.
                <*> optional (strOption (long "vm-internal-ip" <> metavar "IP" <> Opt.help "tier 2: pin the VM to this internal address, a free one in the region's `default` subnet; needs --peer-internal-ip"))
                <*> optional (strOption (long "peer-internal-ip" <> metavar "IP" <> Opt.help "tier 2: also boot a peer with no external address, pinned to this internal one, which the VM then fetches a page from; needs --vm-internal-ip"))
                <*> optional (strOption (long "dns-zone" <> metavar "DNS_NAME" <> Opt.help "tier 0: also create a public Cloud DNS zone for this domain and print the name servers it was assigned (cents per month); at tier 3, also point lb.DNS_NAME at the balancer"))
                <*> optional (strOption (long "account" <> metavar "EMAIL" <> Opt.help "the account gcloud must be acting as; any other active account is refused before anything is created (Gcp.Core.declaredAccount)"))
                <*> switch (long "lb-https" <> Opt.help "tier 3, with --dns-zone: also serve lb.DNS_NAME over HTTPS with a Google-managed certificate, through a host rule, a path rule and a second backend service with a longer timeout")
        -- xor: once one branch has matched, the other flag is rejected by the parser
        imageSourceP =
            (FromContainerfile <$> strOption (long "containerfile" <> metavar "PATH" <> Opt.help "tier 1: build this Containerfile, with its directory as build context"))
                <|> (FromBaseImage <$> strOption (long "base-image" <> metavar "IMAGE" <> value defaultBaseImage <> Opt.showDefault <> Opt.help "tier 1: build `FROM IMAGE`"))
        parent =
            (SeedOrganization <$> strOption (long "organization" <> metavar "ORG_ID" <> Opt.help "create the project under this organization"))
                <|> (SeedFolder <$> strOption (long "folder" <> metavar "FOLDER_ID" <> Opt.help "create the project under this folder"))
                <|> flag' SeedExistingProject (long "existing-project" <> Opt.help "do not create, link or delete the project")
                <|> (const SeedNoParent <$> switch (long "no-parent" <> Opt.help "create the project with no parent (default)"))

-------------------------------------------------------------------------------
-- Spec

data ParentRef = OrganizationParent Text | FolderParent Text | NoParentRef
    deriving (Eq, Show, Generic)

instance FromJSON ParentRef
instance ToJSON ParentRef

-- | What tier 1 builds.
data ImageSource
    = -- | a generated one-line Containerfile, @FROM@ this image
      FromBaseImage Text
    | -- | the caller's own Containerfile (absolute once configured)
      FromContainerfile FilePath
    deriving (Eq, Show, Generic)

instance FromJSON ImageSource
instance ToJSON ImageSource

-- | Google's Cloud Run sample: a tiny server answering on @$PORT@.
defaultBaseImage :: Text
defaultBaseImage = "us-docker.pkg.dev/cloudrun/container/hello"

{- | Which side of a 'SreBox.Gcp.VmProvision' hand-off a directive is for.
The tier-2 VM is provisioned by /this same binary/, uploaded and run there
with a directive of its own: 'OnVm' is what it declares once it arrives, and
it names nothing in GCP at all.
-}
data Role = Control | OnVm
    deriving (Eq, Show, Generic)

instance FromJSON Role
instance ToJSON Role

-- | Tier 2's parameters, resolved.
data VmConfig = VmConfig
    { vmZone :: Text
    , vmMachineType :: Text
    , vmImageFamily :: Text
    , vmImageProject :: Text
    , vmUser :: Text
    , vmSshSourceRange :: Text
    , vmIp :: Maybe Text
    -- ^ 'Nothing' on the first pass: GCP has not picked it yet.
    , vmSelfPath :: Self.SelfPath
    , vmMarkerPath :: FilePath
    -- ^ what the uploaded binary writes on the VM, as proof it ran there.
    }
    deriving (Eq, Show, Generic)

instance FromJSON VmConfig
instance ToJSON VmConfig

{- | Tier 3's parameters, resolved.

Carried on the directive rather than being tier-2 fields because the /VM
side/ needs them too: the port the balancer's backend is configured for is
the same port the systemd unit the uploaded binary installs has to listen
on, and there is exactly one place to say it.
-}
data LbConfig = LbConfig
    { lbProxyRange :: Text
    , lbPort :: Int
    , lbHttps :: Maybe Bool
    -- ^ 'Maybe' so a directive written before the field existed still reads
    }
    deriving (Eq, Show, Generic)

instance FromJSON LbConfig
instance ToJSON LbConfig

{- | Tier 2's optional peer: two instances that name each other by internal
addresses declared here, before either exists.
-}
data PeerConfig = PeerConfig
    { peerVmInternalIp :: Text
    -- ^ the tier-2 VM's, which the peer's firewall rule admits as a @\/32@
    , peerInternalIp :: Text
    -- ^ the peer's, which is its only address
    , peerPort :: Int
    }
    deriving (Eq, Show, Generic)

instance FromJSON PeerConfig
instance ToJSON PeerConfig

data Spec = Spec
    { role :: Role
    , project :: Text
    , createProjectUnder :: Maybe ParentRef
    -- ^ 'Nothing': the project pre-exists and is left alone
    , billingAccount :: Maybe Text
    , region :: Text
    , tier :: Int
    , prefix :: Text
    , imageTag :: Text
    , imageSource :: ImageSource
    , workDir :: FilePath
    , vmConfig :: Maybe VmConfig
    , lbConfig :: Maybe LbConfig
    , alertEmail :: Maybe Text
    -- ^ tier 1: the standard alerts on the service go here, if anywhere
    , peerConfig :: Maybe PeerConfig
    , dnsZone :: Maybe Text
    -- ^ tier 0: a Cloud DNS zone for this domain, if any
    , account :: Maybe Text
    -- ^ the account gcloud must be acting as, if declared
    }
    deriving (Eq, Show, Generic)

instance FromJSON Spec
instance ToJSON Spec

configure :: Configure IO Seed Spec
configure = Configure $ \seed -> do
    let creating = seed.seedParent /= SeedExistingProject
    when (creating && seed.seedBillingAccount == Nothing) $
        fail "--billing-account is required when the project is created (pass --existing-project to use one as-is)"
    when (seed.seedTier < 0 || seed.seedTier > 3) $
        fail "--tier must be 0, 1, 2 or 3"
    -- GCP's own constraints, checked here so a typo fails before anything is created
    when (not (validProjectId seed.seedProject)) $
        fail "--project must be 6-30 characters of [a-z0-9-], starting with a letter"
    when (Text.length (seed.seedPrefix <> "-sa") > 30 || not (validPrefix seed.seedPrefix)) $
        fail "--prefix must be [a-z0-9-] starting with a letter, at most 27 characters"
    source <- case seed.seedImageSource of
        FromBaseImage img -> do
            when (Text.null img || Text.any (`elem` [' ', '\t', '\n', '\r']) img) $
                fail "--base-image must be a single image reference"
            pure (FromBaseImage img)
        FromContainerfile path -> do
            exists <- doesFileExist path
            when (not exists) $
                fail ("--containerfile not found: " <> path)
            FromContainerfile <$> makeAbsolute path
    dir <- makeAbsolute seed.seedWorkDir
    vm <-
        if seed.seedTier < 2
            then pure Nothing
            else do
                self <- Self.readSelfPath_linux
                pure $
                    Just
                        VmConfig
                            { vmZone = maybe (seed.seedRegion <> "-b") id seed.seedVmZone
                            , vmMachineType = seed.seedVmMachineType
                            , vmImageFamily = seed.seedVmImageFamily
                            , vmImageProject = seed.seedVmImageProject
                            , vmUser = seed.seedVmUser
                            , vmSshSourceRange = seed.seedSshSourceRange
                            , vmIp = seed.seedVmIp
                            , vmSelfPath = self
                            , vmMarkerPath = "/var/lib/salmon-toy/provisioned"
                            }
    peer <- case (seed.seedVmInternalIp, seed.seedPeerInternalIp) of
        (Nothing, Nothing) -> pure Nothing
        (Just vmInternal, Just peerInternal) -> do
            when (seed.seedTier < 2) $
                fail "--vm-internal-ip and --peer-internal-ip need --tier 2 or above"
            when (vmInternal == peerInternal) $
                fail "--vm-internal-ip and --peer-internal-ip must differ"
            when (not (validIpv4 vmInternal && validIpv4 peerInternal)) $
                fail "--vm-internal-ip and --peer-internal-ip must be IPv4 literals"
            pure (Just (PeerConfig vmInternal peerInternal 8081))
        _ -> fail "--vm-internal-ip and --peer-internal-ip go together: give both or neither"
    when (maybe False (not . validDnsName) seed.seedDnsZone) $
        fail "--dns-zone must be a domain name: dot-separated labels of [a-z0-9-], at least two"
    when (seed.seedLbHttps && (seed.seedTier < 3 || seed.seedDnsZone == Nothing)) $
        fail "--lb-https needs --tier 3 and --dns-zone: the certificate is for lb.DNS_NAME and is authorized through that zone"
    let lb =
            if seed.seedTier < 3
                then Nothing
                else Just (LbConfig seed.seedLbProxyRange seed.seedLbPort (if seed.seedLbHttps then Just True else Nothing))
    pure $
        Spec
            { role = Control
            , project = seed.seedProject
            , createProjectUnder = case seed.seedParent of
                SeedOrganization org -> Just (OrganizationParent org)
                SeedFolder folder -> Just (FolderParent folder)
                SeedNoParent -> Just NoParentRef
                SeedExistingProject -> Nothing
            , billingAccount = seed.seedBillingAccount
            , region = seed.seedRegion
            , tier = seed.seedTier
            , prefix = seed.seedPrefix
            , imageTag = seed.seedImageTag
            , imageSource = source
            , workDir = dir
            , vmConfig = vm
            , lbConfig = lb
            , alertEmail = if seed.seedTier >= 1 then seed.seedAlertEmail else Nothing
            , peerConfig = peer
            , dnsZone = seed.seedDnsZone
            , account = seed.seedAccount
            }
  where
    validIpv4 t = case Text.splitOn "." t of
        parts@[_, _, _, _] -> all validOctet parts
        _ -> False
    validOctet o =
        not (Text.null o) && Text.length o <= 3 && Text.all isDigit o && (read (Text.unpack o) :: Int) <= 255
    validDnsName t =
        let labels = Text.splitOn "." (Text.dropWhileEnd (== '.') t)
         in length labels >= 2 && all validLabel labels
    validLabel l =
        not (Text.null l)
            && Text.length l <= 63
            && Text.all (\x -> isAsciiLower x || isDigit x || x == '-') l
            && not ("-" `Text.isPrefixOf` l)
            && not ("-" `Text.isSuffixOf` l)
    validProjectId t =
        Text.length t >= 6 && Text.length t <= 30 && validPrefix t && not ("-" `Text.isSuffixOf` t)
    validPrefix t =
        case Text.uncons t of
            Just (c, _) -> isAsciiLower c && Text.all (\x -> isAsciiLower x || isDigit x || x == '-') t
            Nothing -> False

-------------------------------------------------------------------------------
-- Program

program :: Track' Spec
program = Track $ \spec -> case spec.role of
    OnVm -> onVm spec
    Control -> control spec

{- | What the uploaded copy of this binary declares once it is running on the
VM: one file, whose existence is the whole proof that the hand-off worked.
-}
onVm :: Spec -> Op
onVm spec =
    op "gcp-toy-on-vm" (deps (marker : secretRead spec : maybe [] (\lb -> [webServer spec lb]) spec.lbConfig <> maybe [] (\peer -> [peerReached spec peer `inject` marker]) spec.peerConfig)) $ \actions ->
        actions
            { help = "the tier-2 payload, declared by this binary running on the VM"
            , ref = mkRef "gcp-toy-on-vm" spec.project
            }
  where
    path = maybe "/var/lib/salmon-toy/provisioned" vmMarkerPath spec.vmConfig
    marker = FS.filecontents (FS.FileContents path ("provisioned by salmon-gcp-toy for " <> spec.project <> "\n"))

{- | Where the control side's generated secret lands on the VM, delivered by
"Salmon.Builtin.Nodes.SecretDelivery" before the uploaded binary runs.
-}
secretPlacement :: SecretDelivery.Placement
secretPlacement = SecretDelivery.Placement "/etc/salmon-toy/secret" "root" "root" "0600"

-- | What the control side generates: 32 random bytes, as 64 hex characters.
secretLength :: Int
secretLength = 64

-- | Where the VM records that it read the secret, for the driver to read back.
secretMarkerPath :: FilePath
secretMarkerPath = "/var/lib/salmon-toy/secret-read"

{- | The uploaded binary reading the secret the control side delivered.

The secret is not in this directive -- which is printed in the remote call's
own report -- and that is the point being validated: the VM-side graph knows
a /path/, and the bytes got there by another road. The proof left behind is
a marker saying how many bytes were read, which is a property of the
declaration (see 'secretLength') and not of the secret. A file that is
absent, or is not what the control side generates, fails the node.
-}
secretRead :: Spec -> Op
secretRead spec =
    op "gcp-toy-secret-read" nodeps $ \actions ->
        actions
            { help = Text.unwords ["reads the delivered secret at", Text.pack path]
            , ref = mkRef "gcp-toy-secret-read" (spec.project, path)
            , up = do
                present <- doesFileExist path
                when (not present) $
                    throwIO (userError ("no secret was delivered at " <> path))
                bytes <- ByteString.readFile path
                when (ByteString.length bytes /= secretLength) $
                    throwIO (userError ("the file at " <> path <> " is not the generated secret: " <> show (ByteString.length bytes) <> " bytes"))
                createDirectoryIfMissing True (takeDirectory secretMarkerPath)
                writeFile secretMarkerPath ("read " <> show (ByteString.length bytes) <> " bytes of a delivered secret for " <> Text.unpack spec.project <> "\n")
            }
  where
    path = SecretDelivery.placePath secretPlacement

-- | Where the VM leaves what the peer answered, for the driver to read back.
peerMarkerPath :: FilePath
peerMarkerPath = "/var/lib/salmon-toy/peer-reached"

{- | The VM fetching the peer's page on the peer's /declared/ internal
address, and keeping what came back.

Declared on the VM side because that is the only place the claim can be
tested from: the peer has no external address, so nothing outside the VPC can
ask it anything. The fetch waits (up to about five minutes) since the peer
was created moments before the VM and serves only once its own startup
script has run. No @check@: the question is whether the peer answers /now/,
and asking costs what fetching does.
-}
peerReached :: Spec -> PeerConfig -> Op
peerReached spec peer =
    op "gcp-toy-peer-reached" nodeps $ \actions ->
        actions
            { help = Text.unwords ["fetches", url, "from the peer's internal address"]
            , ref = mkRef "gcp-toy-peer-reached" (spec.project, url)
            , up = do
                (code, out, err) <-
                    readProcessWithExitCode
                        "curl"
                        ["-fsS", "--max-time", "10", "--retry", "30", "--retry-delay", "10", "--retry-all-errors", Text.unpack url]
                        ""
                case code of
                    ExitSuccess
                        | spec.project `Text.isInfixOf` Text.pack out -> writeFile peerMarkerPath out
                        | otherwise -> throwIO (userError ("the peer answered, but not with its page: " <> take 200 out))
                    ExitFailure n -> throwIO (userError ("could not reach the peer at " <> Text.unpack url <> " (curl exit " <> show n <> "): " <> take 400 err))
            }
  where
    url = "http://" <> peer.peerInternalIp <> ":" <> Text.pack (show peer.peerPort) <> "/"

{- | Tier 3's backend: a page, and a systemd unit serving it.

Declared on the /VM side/ deliberately. A load balancer whose forwarding rule
merely exists proves nothing -- a balancer in front of no server answers
@502@ just as readily -- so what tier 3 actually checks is a @200@ carrying
the project id, and the only thing that can put that body there is salmon
running on the machine. It is therefore also a second, independent proof
that the tier-2 hand-off worked, this time through the front door.

@python3@ is on every Ubuntu cloud image (cloud-init is written in it), so
the 'Debian.deb' node here is nearly always a 'Skip' -- it is declared
anyway, because "nearly always" is not a dependency.
-}
webServer :: Spec -> LbConfig -> Op
webServer spec lb =
    Systemd.systemdService reportPrint OS.systemctl trackConfig config
  where
    root :: FilePath
    root = "/var/www/salmon-toy"

    trackConfig :: Track' Systemd.Config
    trackConfig = Track $ \_ ->
        op "setup-salmon-toy-web" (deps [indexFile, Debian.deb (Debian.Package "python3")]) id

    indexFile :: Op
    indexFile =
        FS.filecontents
            ( FS.FileContents
                (root <> "/index.html")
                ("served by salmon-gcp-toy from " <> spec.project <> "\n")
            )

    config :: Systemd.Config
    config =
        Systemd.Config
            Systemd.System
            "/etc/systemd/system"
            "salmon-toy-web.service"
            (Systemd.Unit "salmon-gcp-toy tier-3 backend" "network-online.target")
            ( Systemd.Service
                Systemd.Simple
                "root"
                "root"
                "022"
                start
                Systemd.OnFailure
                Systemd.Process
                root
            )
            (Systemd.Install "multi-user.target")

    start :: Systemd.Start
    start =
        Systemd.Start
            "/usr/bin/python3"
            [ "-m"
            , "http.server"
            , Text.pack (show lb.lbPort)
            , "--bind"
            , "0.0.0.0"
            , "--directory"
            , Text.pack root
            ]

control :: Spec -> Op
control spec =
    op "gcp-toy" (deps (tier0 spec <> (if spec.tier >= 1 then tier1 spec else []) <> (if spec.tier >= 2 then tier2 spec else []) <> (if spec.tier >= 3 then tier3 spec else []))) $ \actions ->
        actions
            { help = Text.unwords ["salmon GCP toy validation, tier", Text.pack (show spec.tier), "in", spec.project]
            , ref = mkRef "gcp-toy" (spec.project, spec.prefix)
            }

projectOf :: Spec -> Core.Project
projectOf spec = Core.Project spec.project

regionOf :: Spec -> Core.Region
regionOf spec = Core.Region spec.region

adc :: Op
adc = Core.applicationDefaultCredentials reportPrint Core.gcloud

-- | Whatever every project-scoped resource must wait for.
foundation :: Spec -> [Op]
foundation spec = identity <> catMaybes [projectNode, billingNode]
  where
    -- who acts: ADC is usable, and gcloud's active account is the declared one
    identity = adc : [Core.declaredAccount reportPrint Core.gcloud (Core.Account a) | Just a <- [spec.account]]
    projectNode =
        fmap
            ( \parent ->
                ResourceManager.project
                    reportPrint
                    Core.gcloud
                    (ResourceManager.ProjectSpec (projectOf spec) (toParent parent) mempty)
                    `injectAll` identity
            )
            spec.createProjectUnder
    billingNode =
        fmap
            ( \acct ->
                foldl
                    inject
                    (Billing.linkBillingAccount reportPrint Core.gcloud (projectOf spec) (Billing.BillingAccount acct))
                    (identity <> catMaybes [projectNode])
            )
            spec.billingAccount

    injectAll = foldl inject
    toParent (OrganizationParent org) = ResourceManager.Organization org
    toParent (FolderParent folder) = ResourceManager.Folder folder
    toParent NoParentRef = ResourceManager.NoParent

onFoundation :: Spec -> Op -> Op
onFoundation spec o = foldl inject o (foundation spec)

api :: Spec -> Text -> Op
api spec name =
    onFoundation spec (ServiceUsage.enableService reportPrint Core.gcloud (projectOf spec) (ServiceUsage.Api name))

serviceAccountId :: Spec -> Text
serviceAccountId spec = spec.prefix <> "-sa"

serviceAccountEmail :: Spec -> Text
serviceAccountEmail spec = serviceAccountId spec <> "@" <> spec.project <> ".iam.gserviceaccount.com"

repo :: Spec -> ArtifactRegistry.ArtifactRepo
repo spec = ArtifactRegistry.ArtifactRepo (spec.prefix <> "-repo") (projectOf spec) (regionOf spec) ArtifactRegistry.Docker

serviceAccount :: Spec -> Op
serviceAccount spec =
    Iam.serviceAccount reportPrint Core.gcloud (projectOf spec) (serviceAccountId spec)
        `inject` api spec "iam.googleapis.com"

repository :: Spec -> Op
repository spec =
    ArtifactRegistry.artifactRepository reportPrint Core.gcloud (repo spec)
        `inject` api spec "artifactregistry.googleapis.com"

grant :: Spec -> Text -> Text -> Op
grant spec role resource =
    Iam.iamBinding
        reportPrint
        Core.gcloud
        (Iam.IamBinding (Iam.ServiceAccount (serviceAccountEmail spec)) role resource)
        `inject` serviceAccount spec

tier0 :: Spec -> [Op]
tier0 spec =
    [ grant spec "roles/storage.objectViewer" ("buckets/" <> bucketName) `inject` bucket
    , grant spec "roles/artifactregistry.reader" repoResource `inject` repository spec
    ]
        <> [dnsNameServers spec zone | zone <- maybe [] (pure . dnsZoneOf spec) spec.dnsZone]
  where
    -- bucket names are global: scoping by project id keeps two sandboxes apart
    bucketName = spec.project <> "-" <> spec.prefix
    bucket =
        Storage.bucket reportPrint Core.gcloud (Storage.Bucket bucketName (projectOf spec) (regionOf spec) True)
            `inject` api spec "storage.googleapis.com"
    repoResource =
        Text.intercalate "/" ["projects", spec.project, "locations", spec.region, "repositories", (repo spec).repoName]

dnsZoneOf :: Spec -> Text -> CloudDns.ManagedZone
dnsZoneOf spec dnsName =
    CloudDns.ManagedZone (spec.prefix <> "-zone") (projectOf spec) dnsName "salmon-gcp-toy validation zone"

{- | The zone, and on top of it a node that reads back the name servers
Cloud DNS assigned and prints them: what an operator would enter at the
registrar to delegate the domain.

Printing is a node rather than something @main@ does because only a node
runs /after/ the zone's @up@. No @check@: it answers nothing about an
effect, so it prints on every pass, which is the point.
-}
dnsNameServers :: Spec -> CloudDns.ManagedZone -> Op
dnsNameServers spec zone =
    op "gcp-toy-dns-name-servers" (deps [dnsZoneNode spec zone]) $ \actions ->
        actions
            { help = Text.unwords ["prints the name servers assigned to", CloudDns.fqdn zone.zoneDnsName]
            , ref = mkRef "gcp-toy-dns-name-servers" (spec.project, zone.zoneName)
            , up = do
                servers <- CloudDns.readNameServers zone
                case servers of
                    Nothing -> throwIO (userError ("no name servers could be read for zone " <> Text.unpack zone.zoneName))
                    Just ns ->
                        putStrLn
                            ( Text.unpack
                                (Text.unwords (["name servers for", CloudDns.fqdn zone.zoneDnsName <> ":"] <> ns))
                            )
            }

dnsZoneNode :: Spec -> CloudDns.ManagedZone -> Op
dnsZoneNode spec zone =
    CloudDns.managedZone reportPrint Core.gcloud zone
        `inject` api spec CloudDns.dnsApi

tier1 :: Spec -> [Op]
tier1 spec =
    [ CloudRunDeploy.buildPushDeploy
        reportPrint
        Core.gcloud
        ignoreTrack
        CloudRunDeploy.CloudRunDeployConfig
            { CloudRunDeploy.crd_repo = repo spec
            , CloudRunDeploy.crd_authFile = Podman.AuthFile (spec.workDir <> "/podman-auth.json")
            , CloudRunDeploy.crd_containerfile = containerfile
            , CloudRunDeploy.crd_image = image
            , CloudRunDeploy.crd_service = spec.prefix <> "-hello"
            , CloudRunDeploy.crd_project = projectOf spec
            , CloudRunDeploy.crd_region = regionOf spec
            , CloudRunDeploy.crd_env = mempty
            , CloudRunDeploy.crd_serviceAccount = serviceAccountEmail spec
            , CloudRunDeploy.crd_ingress = CloudRun.All
            , CloudRunDeploy.crd_maxInstances = Just 1
            , CloudRunDeploy.crd_options = CloudRun.defaultCloudRunOptions
            }
        `inject` api spec "run.googleapis.com"
        `inject` repository spec
        `inject` serviceAccount spec
    ]
        <> [ CloudRunAlerts.standardAlerts
            reportPrint
            Core.gcloud
            CloudRunAlerts.CloudRunAlertsConfig
                { CloudRunAlerts.cra_project = projectOf spec
                , CloudRunAlerts.cra_region = regionOf spec
                , CloudRunAlerts.cra_service = spec.prefix <> "-hello"
                , CloudRunAlerts.cra_email = email
                , CloudRunAlerts.cra_channelName = spec.prefix <> " alerts"
                , CloudRunAlerts.cra_maxInstances = Just 1
                , CloudRunAlerts.cra_thresholds = CloudRunAlerts.defaultAlertThresholds
                }
            `inject` api spec Monitoring.monitoringApi
           | Just email <- [spec.alertEmail]
           ]
  where
    image =
        Text.concat [spec.region, "-docker.pkg.dev/", spec.project, "/", (repo spec).repoName, "/hello:", spec.imageTag]
    containerfile :: FS.File "containerfile"
    containerfile = case spec.imageSource of
        -- the label makes each --image-tag build a distinct image rather
        -- than the base image re-pushed under another name
        FromBaseImage base ->
            FS.generateFileContents
                ( Text.unlines
                    [ "FROM " <> base
                    , "LABEL salmon-toy-tag=\"" <> spec.imageTag <> "\""
                    ]
                )
                (spec.workDir <> "/Containerfile")
        FromContainerfile path -> FS.PreExisting path

-------------------------------------------------------------------------------
-- Tier 2: a VM, provisioned over an SSH CA by this same binary.

tier2 :: Spec -> [Op]
tier2 spec = [provisioned spec vm | vm <- maybe [] pure spec.vmConfig]

{- | The machine, created and provisioned.

With @--vm-ip@ the host is declared and every node is an ordinary one.
Without it the host is read from the reserved address once the instance is
up, and what names the machine is a sub-graph walked from inside this node --
one pass instead of two, at the price of that sub-graph being opaque to
@run tree@.
-}
provisioned :: Spec -> VmConfig -> Op
provisioned spec vm = case vm.vmIp of
    Just ip ->
        VmProvision.provisionedVm reportPrint Core.gcloud OS.sshClient (at ip (config ip))
    Nothing ->
        VmProvision.provisionedVmReadingHost
            reportPrint
            Core.gcloud
            OS.sshClient
            (Compute.readAddress (addressSpec spec))
            at
            -- overwritten with the address read
            (config "")
  where
    at ip cfg = cfg{VmProvision.vmp_beforeCall = \opts -> [deliveredSecret spec vm ip opts]}

    config host =
        VmProvision.VmProvisionConfig
            { VmProvision.vmp_name = vmName spec
            , VmProvision.vmp_instance = gceInstance spec vm
            , VmProvision.vmp_ca = caKey spec
            , VmProvision.vmp_clientIdentity = clientKey spec
            , VmProvision.vmp_sshUser = vm.vmUser
            , VmProvision.vmp_sshHost = host
            , VmProvision.vmp_sshPort = 22
            , VmProvision.vmp_prerequisites = vmPrerequisites spec vm
            , VmProvision.vmp_beforeCall = const []
            , VmProvision.vmp_remoteDir = "/home/" <> Text.unpack vm.vmUser
            , VmProvision.vmp_selfPath = vm.vmSelfPath
            , VmProvision.vmp_directiveTrack = program
            , VmProvision.vmp_directive = spec{role = OnVm}
            }

{- | A secret generated on the control side and put on the VM before the
uploaded binary runs: 'Secrets.sharedSecretFile' makes it, and
'SecretDelivery.uploadSecretFile' sends it over the connection the
provisioning already uses (the 'Ssh.ClientOpts' handed to @vmp_beforeCall@),
to be owned by root and readable by nobody else. 'secretRead' is the other
end.
-}
deliveredSecret :: Spec -> VmConfig -> Text -> Ssh.ClientOpts -> Op
deliveredSecret spec vm ip opts =
    SecretDelivery.uploadSecretFile
        opts
        reportPrint
        OS.ssh
        SecretDelivery.SecretUpload
            { SecretDelivery.uploadSource = localSecretPath spec
            , SecretDelivery.uploadRemote = Ssh.Remote vm.vmUser ip
            , SecretDelivery.uploadPlacement = secretPlacement
            , SecretDelivery.uploadElevation = SecretDelivery.WithSudo
            }
        `inject` Secrets.sharedSecretFile reportPrint ignoreTrack (Secrets.Secret Secrets.Hex (secretLength `div` 2) (localSecretPath spec))

localSecretPath :: Spec -> FilePath
localSecretPath spec = spec.workDir <> "/secrets/toy-secret"

-- | The instance on its own, for what stands on the machine rather than on
-- its being provisioned (the balancer's instance group).
instanceNode :: Spec -> VmConfig -> Op
instanceNode spec vm =
    foldl inject (Compute.gceInstance reportPrint Core.gcloud (gceInstance spec vm)) (vmPrerequisites spec vm)

{- | Everything the instance needs to exist before it is created: the
reserved address it claims by name, the firewall rule its sshd needs, and the
startup script its metadata points at.
-}
vmPrerequisites :: Spec -> VmConfig -> [Op]
vmPrerequisites spec vm =
    [ computeApi
    , -- The CA has to be in project metadata before the instance *boots*,
      -- not merely before it is provisioned: the startup script reads the key
      -- at boot and nothing re-runs it afterwards. Declared here as well as
      -- by 'VmProvision' so that 'instanceNode', which does not go through
      -- that recipe, carries it. Both declarations are the same node: same
      -- 'Ref', deduped by the fold.
      sshCaInMetadata spec
    , Compute.address reportPrint Core.gcloud (addressSpec spec) `inject` computeApi
    , Compute.firewallRule
        reportPrint
        Core.gcloud
        Compute.FirewallRule
            { Compute.firewallName = spec.prefix <> "-ssh"
            , Compute.firewallProject = projectOf spec
            , Compute.firewallNetwork = "default"
            , Compute.firewallAllow = "tcp:22"
            , Compute.firewallSourceRanges = [vm.vmSshSourceRange]
            , Compute.firewallTargetTags = [sshTag spec]
            }
        `inject` computeApi
    , VmProvision.caTrustStartupScriptFile (startupScriptPath spec) vm.vmUser
    ]
        -- The peer and the VM's own reservation come before the VM: the VM
        -- is pinned to an address that should be reserved first, and what it
        -- is provisioned to do is fetch a page from the peer.
        <> maybe [] (\peer -> [internalAddress (vmInternalAddressSpec spec peer) `inject` computeApi, peerInstance spec vm peer]) spec.peerConfig
  where
    -- every tier-2 resource is a Compute Engine one, and a fresh project has
    -- that API off: addresses, firewall rules and instances all answer
    -- PERMISSION_DENIED/SERVICE_DISABLED until it is on.
    computeApi = api spec "compute.googleapis.com"

-- | The CA keypair, and its public half published as project metadata.
sshCaInMetadata :: Spec -> Op
sshCaInMetadata spec =
    SshAccess.installMetadataCaKey
        reportPrint
        Core.gcloud
        (SshAccess.MetadataSshCa (projectOf spec) (Keys.publicKeyPath (caKey spec)))
        `inject` Keys.sshKey reportPrint OS.sshClient (caKey spec)

addressSpec :: Spec -> Compute.Address
addressSpec spec = Compute.Address (spec.prefix <> "-ip") (projectOf spec) (regionOf spec) Compute.ExternalAddress

-- | The reservation behind the VM's pinned internal address.
vmInternalAddressSpec :: Spec -> PeerConfig -> Compute.Address
vmInternalAddressSpec spec peer =
    Compute.Address (spec.prefix <> "-vm-internal") (projectOf spec) (regionOf spec) (Compute.InternalAddress "default" (Just peer.peerVmInternalIp))

-- | The reservation behind the peer's.
peerInternalAddressSpec :: Spec -> PeerConfig -> Compute.Address
peerInternalAddressSpec spec peer =
    Compute.Address (spec.prefix <> "-peer-internal") (projectOf spec) (regionOf spec) (Compute.InternalAddress "default" (Just peer.peerInternalIp))

internalAddress :: Compute.Address -> Op
internalAddress = Compute.address reportPrint Core.gcloud

{- | The peer: an instance with no external address, pinned to a declared
internal one, serving one page to the tier-2 VM and to nothing else.

It is everything the pair of flags on 'Compute.Instance' is for. Its firewall
rule admits the VM by the @\/32@ the VM is pinned to, written before either
machine exists -- the shape of a @pg_hba.conf@ line naming a replication
peer. And with no external address it has no way out (the toy declares no
Cloud NAT), so its startup script may only use what the image ships:
@python3@, which every Ubuntu cloud image has.
-}
peerInstance :: Spec -> VmConfig -> PeerConfig -> Op
peerInstance spec vm peer =
    foldl
        inject
        (Compute.gceInstance reportPrint Core.gcloud inst)
        [ computeApi
        , internalAddress (peerInternalAddressSpec spec peer) `inject` computeApi
        , Compute.firewallRule
            reportPrint
            Core.gcloud
            Compute.FirewallRule
                { Compute.firewallName = spec.prefix <> "-peer"
                , Compute.firewallProject = projectOf spec
                , Compute.firewallNetwork = "default"
                , Compute.firewallAllow = "tcp:" <> Text.pack (show peer.peerPort)
                , Compute.firewallSourceRanges = [peer.peerVmInternalIp <> "/32"]
                , Compute.firewallTargetTags = [peerTag spec]
                }
            `inject` computeApi
        , FS.filecontents (FS.FileContents (peerStartupScriptPath spec) (peerStartupScript spec peer))
        ]
  where
    computeApi = api spec "compute.googleapis.com"
    inst =
        (gceInstance spec vm)
            { Compute.instanceName = peerName spec
            , Compute.instanceMetadataFiles = Map.fromList [("startup-script", peerStartupScriptPath spec)]
            , Compute.instanceExternalAddress = Compute.NoExternalAddress
            , Compute.instanceInternalAddress = Compute.PinnedInternal peer.peerInternalIp
            , Compute.instanceTags = [peerTag spec]
            }

{- | Serves one page naming the project, under a transient systemd unit so it
outlives the startup script. Idempotent, because a startup script runs on
every boot (and a transient unit does not survive one).
-}
peerStartupScript :: Spec -> PeerConfig -> Text
peerStartupScript spec peer =
    Text.unlines
        [ "#!/bin/bash"
        , "set -eux"
        , "mkdir -p /var/www/salmon-toy-peer"
        , "echo 'served by the salmon-gcp-toy peer of " <> spec.project <> "' > /var/www/salmon-toy-peer/index.html"
        , "systemctl reset-failed salmon-toy-peer 2>/dev/null || true"
        , "systemctl is-active --quiet salmon-toy-peer \\"
        , "  || systemd-run --unit salmon-toy-peer /usr/bin/python3 -m http.server " <> Text.pack (show peer.peerPort) <> " --bind 0.0.0.0 --directory /var/www/salmon-toy-peer"
        ]

gceInstance :: Spec -> VmConfig -> Compute.Instance
gceInstance spec vm =
    VmProvision.withStartupScriptFile (startupScriptPath spec) $
      Compute.Instance
        { Compute.instanceName = vmName spec
        , Compute.instanceProject = projectOf spec
        , Compute.instanceZone = Core.Zone vm.vmZone
        , Compute.instanceMachineType = Compute.Custom vm.vmMachineType
        , Compute.instanceBootDisk =
            Compute.BootDisk
                { Compute.bootDiskSizeGb = 10
                , Compute.bootDiskImage = Nothing
                , Compute.bootDiskImageFamily = Just vm.vmImageFamily
                , Compute.bootDiskImageProject = Just vm.vmImageProject
                }
        , Compute.instanceNetwork = "default"
        , Compute.instanceSubnet = "default"
        , Compute.instanceServiceAccount = Nothing
        , Compute.instanceMetadata = Map.fromList [("enable-oslogin", "FALSE")]
        , Compute.instanceMetadataFiles = Map.empty
        , Compute.instanceExternalAddress = Compute.ReservedExternal (addressSpec spec).addressName
        , Compute.instanceInternalAddress = maybe Compute.EphemeralInternal (Compute.PinnedInternal . peerVmInternalIp) spec.peerConfig
        , -- tags are fixed at create time, so the tier-3 one has to be on the
          -- instance from the first pass -- there is no adding it later to a
          -- machine the balancer has already been pointed at.
          Compute.instanceTags = [sshTag spec] <> [lbTag spec | spec.tier >= 3]
        , Compute.instancePower = Compute.PoweredOn
        }

-------------------------------------------------------------------------------
-- Tier 3: a regional external ALB in front of that VM.

{- | The balancer and everything GCP insists on having first.

Three of the four nodes below exist only because a /regional external/
Application Load Balancer is an Envoy fleet rather than a Google frontend,
and that changes what has to be true before one can be created:

* it runs its proxies inside the VPC, in a __proxy-only subnet__ that must
  already exist in the region, be @ACTIVE@, and belong to the same network
  as the backends;
* those proxies reach the backends __from that subnet's range__, so the
  backend VMs' own firewall has to allow it -- as does the separate
  @35.191.0.0\/16@ + @130.211.0.0\/22@ pair the health checks come from,
  which is a different source entirely and the usual reason a balancer that
  came up cleanly still answers @502@;
* and a VM is not a backend: an __instance group__ is, so the instance has
  to be put in one.
-}
tier3 :: Spec -> [Op]
tier3 spec = case (spec.vmConfig, spec.lbConfig) of
    (Just vm, Just lb) ->
        balancer spec vm lb
            : concat
                [ balancerRecord spec vm lb zone
                    : [ certificateAuthorizationRecord spec vm lb zone authz
                      | (_domain, authz) <- LoadBalancing.dnsAuthorizations (albSpec spec vm lb)
                      ]
                | zone <- maybe [] (pure . dnsZoneOf spec) spec.dnsZone
                ]
    _ -> []

{- | With @--lb-https@: the @CNAME@ Certificate Manager wants in the zone
before it issues the balancer's certificate.

GCP picks the record's /name/ as well as its data (a per-project
authorization is @_acme-challenge_\<hash\>.lb.DNS_NAME@), so this cannot be
a 'CloudDns.resolvedRecordSet', whose identity is the name. It is a node
that reads the authorization once its node (one of the balancer's) has made
it and runs 'CloudDns.recordSet' for what it read as a nested walk -- checking the
returned 'Bool', since nothing else tells this pass the nested one failed.
No @check@: the nested node has one, so a second pass costs a @describe@.

On the way down the authorization is still there (this node goes before the
authorization's does), so the same read finds the record to delete; if it is
already gone there is no name left to delete by, and nothing is done.
-}
certificateAuthorizationRecord :: Spec -> VmConfig -> LbConfig -> CloudDns.ManagedZone -> Text -> Op
certificateAuthorizationRecord spec vm lb zone authz =
    op "gcp-toy-cert-authorization-record" (deps (dnsZoneNode spec zone : authorization)) $ \actions ->
        actions
            { help = Text.unwords ["publishes the DNS record of certificate authorization", authz]
            , ref = mkRef "gcp-toy-cert-authorization-record" (spec.project, zone.zoneName, authz)
            , up = do
                found <- readRecord
                case found of
                    Nothing -> throwIO (userError ("no DNS record could be read for authorization " <> Text.unpack authz))
                    Just rs -> nested "up" (UpDown.upTree silent (pure . runIdentity) (CloudDns.recordSet reportPrint Core.gcloud rs))
            , down = do
                found <- readRecord
                case found of
                    Nothing -> pure ()
                    Just rs -> nested "down" (UpDown.downTree silent (pure . runIdentity) (CloudDns.recordSet reportPrint Core.gcloud rs))
            }
  where
    -- the authorization alone, not the whole balancer: the record can be
    -- published while the rest of the balancer is still being made, which is
    -- also when the certificate starts waiting for it
    authorization :: [Op]
    authorization =
        maybe [balancer spec vm lb] pure $
            LoadBalancing.applicationLoadBalancerPart
                (balancerPrerequisites spec vm lb)
                reportPrint
                Core.gcloud
                (albSpec spec vm lb)
                (LoadBalancing.DnsAuthorizationPart authz)

    readRecord :: IO (Maybe CloudDns.RecordSet)
    readRecord = (>>= toRecordSet) <$> LoadBalancing.readDnsAuthorizationRecord (albSpec spec vm lb) authz

    -- Certificate Manager only ever asks for a CNAME; anything else is
    -- treated as unreadable rather than published as something it is not.
    toRecordSet :: LoadBalancing.DnsAuthorizationRecord -> Maybe CloudDns.RecordSet
    toRecordSet found
        | found.authorizationRecordType == "CNAME" =
            Just (CloudDns.RecordSet zone found.authorizationRecordName CloudDns.CNAME 300 [found.authorizationRecordData])
        | otherwise = Nothing

    nested :: String -> IO Bool -> IO ()
    nested what act = do
        ok <- act
        when (not ok) $ throwIO (userError ("authorization record " <> what <> " failed for " <> Text.unpack authz))

-- | The name the toy serves over HTTPS, without the trailing dot.
httpsHost :: Spec -> LbConfig -> Maybe Text
httpsHost spec lb = case (lb.lbHttps, spec.dnsZone) of
    (Just True, Just dnsName) -> Just ("lb." <> dnsName)
    _ -> Nothing

{- | With @--dns-zone@: @lb.DNS_NAME@, an @A@ record at the balancer's
address.

GCP picks that address when the forwarding rule is created, so the record's
data is resolved at @up@ ('CloudDns.resolvedRecordSet' over
'LoadBalancing.readAddress') rather than declared -- which is what spares
this a pass of its own, where the VM's address needs one because a /seed/
has to carry it. The zone goes underneath as well as the balancer: Cloud DNS
refuses to delete a zone that still holds the record.
-}
balancerRecord :: Spec -> VmConfig -> LbConfig -> CloudDns.ManagedZone -> Op
balancerRecord spec vm lb zone =
    CloudDns.resolvedRecordSet
        reportPrint
        Core.gcloud
        (CloudDns.RecordSet zone (balancerRecordName zone) CloudDns.A 300 [])
        (fmap pure <$> LoadBalancing.readAddress (albSpec spec vm lb))
        `inject` dnsZoneNode spec zone
        `inject` balancer spec vm lb

-- | The name the toy points at its balancer.
balancerRecordName :: CloudDns.ManagedZone -> Text
balancerRecordName zone = "lb." <> CloudDns.fqdn zone.zoneDnsName

albSpec :: Spec -> VmConfig -> LbConfig -> LoadBalancing.ApplicationLoadBalancer
albSpec spec vm lb =
    LoadBalancing.ApplicationLoadBalancer
        { LoadBalancing.albName = spec.prefix <> "-lb"
        , LoadBalancing.albProject = projectOf spec
        , LoadBalancing.albRegion = regionOf spec
        , -- omitted rather than "default": the forwarding rule falls back
          -- to the default network, which is the one everything else here
          -- is on, and naming it is one more thing to get wrong.
          LoadBalancing.albNetwork = Nothing
        , LoadBalancing.albBackends = [group]
        , LoadBalancing.albHealthCheck = Just healthCheck
        , LoadBalancing.albTimeoutSec = Nothing
        , -- Everything below is --lb-https only, and is there to put each
          -- thing the balancer can express through a real project once: a
          -- second backend service (the same group, a longer timeout), a
          -- host rule, a path rule to that service, a managed certificate.
          -- The VM serves one page, so /slow/* answers 404 through the
          -- balancer -- the rule is to be read back from the URL map.
          LoadBalancing.albServices =
            [ LoadBalancing.BackendService "slow" [group] (Just healthCheck) (Just 120)
            | _ <- hosts
            ]
        , LoadBalancing.albHostRules =
            [ LoadBalancing.HostRule
                [host]
                LoadBalancing.DefaultService
                [LoadBalancing.PathRule ["/slow/*"] (LoadBalancing.NamedService "slow")]
            | host <- hosts
            ]
        , LoadBalancing.albCertificates =
            [ LoadBalancing.ManagedCertificate (spec.prefix <> "-lb-cert") [host]
            | host <- hosts
            ]
        , -- not exercised by the toy: no backend bucket, and port 80 as it
          -- always was (a proxy created once on the balancer's own map)
          LoadBalancing.albBuckets = []
        , LoadBalancing.albHttp = LoadBalancing.HttpCreatedOnce
        }
  where
    hosts = maybe [] pure (httpsHost spec lb)
    healthCheck = LoadBalancing.HealthCheck (spec.prefix <> "-hc") lb.lbPort
    group =
        LoadBalancing.InstanceGroupBackend
            (instanceGroupSpec spec vm).groupName
            (LoadBalancing.InstanceGroupZone vm.vmZone)
            [lb.lbPort]

balancer :: Spec -> VmConfig -> LbConfig -> Op
balancer spec vm lb =
    LoadBalancing.applicationLoadBalancerAfter (balancerPrerequisites spec vm lb) reportPrint Core.gcloud (albSpec spec vm lb)

{- | What has to be there before any of the balancer's resources is made.
Handed to the balancer rather than @inject@ed into it: the balancer is a
node per resource under a root, and a dependency of the root alone would
order nothing.
-}
balancerPrerequisites :: Spec -> VmConfig -> LbConfig -> [Op]
balancerPrerequisites spec vm lb =
    [proxySubnet, membership, backendFirewall]
        <> [api spec LoadBalancing.certificateManagerApi | _ <- maybe [] pure (httpsHost spec lb)]
        <> served
  where
    computeApi = api spec "compute.googleapis.com"

    proxySubnet :: Op
    proxySubnet =
        Compute.subnet
            reportPrint
            Core.gcloud
            Compute.Subnet
                { Compute.subnetName = spec.prefix <> "-proxy"
                , Compute.subnetProject = projectOf spec
                , Compute.subnetRegion = regionOf spec
                , Compute.subnetNetwork = "default"
                , Compute.subnetRange = lb.lbProxyRange
                , Compute.subnetPurpose = Compute.RegionalManagedProxy
                }
            `inject` computeApi

    membership :: Op
    membership =
        Compute.instanceGroupMember
            reportPrint
            Core.gcloud
            (instanceGroupSpec spec vm)
            (vmName spec)
            `inject` group
            `inject` instanceNode spec vm

    group :: Op
    group =
        Compute.instanceGroup reportPrint Core.gcloud (instanceGroupSpec spec vm)
            `inject` computeApi

    backendFirewall :: Op
    backendFirewall =
        Compute.firewallRule
            reportPrint
            Core.gcloud
            Compute.FirewallRule
                { Compute.firewallName = spec.prefix <> "-lb-backend"
                , Compute.firewallProject = projectOf spec
                , Compute.firewallNetwork = "default"
                , Compute.firewallAllow = "tcp:" <> Text.pack (show lb.lbPort)
                , Compute.firewallSourceRanges = [lb.lbProxyRange, "35.191.0.0/16", "130.211.0.0/22"]
                , Compute.firewallTargetTags = [lbTag spec]
                }
            `inject` computeApi

    -- The balancer is declared *after* the machine has been provisioned, so
    -- the backend is already serving by the time the first health check runs.
    served :: [Op]
    served = [provisioned spec vm]

instanceGroupSpec :: Spec -> VmConfig -> Compute.InstanceGroup
instanceGroupSpec spec vm =
    Compute.InstanceGroup (spec.prefix <> "-ig") (projectOf spec) (Core.Zone vm.vmZone)

vmName :: Spec -> Text
vmName spec = spec.prefix <> "-vm"

peerName :: Spec -> Text
peerName spec = spec.prefix <> "-peer"

-- | The tag the peer's firewall rule targets.
peerTag :: Spec -> Text
peerTag spec = spec.prefix <> "-peer"

peerStartupScriptPath :: Spec -> FilePath
peerStartupScriptPath spec = spec.workDir <> "/peer-startup-script.sh"

sshTag :: Spec -> Text
sshTag spec = spec.prefix <> "-ssh"

-- | The tag the tier-3 backend firewall rule targets.
lbTag :: Spec -> Text
lbTag spec = spec.prefix <> "-lb"

startupScriptPath :: Spec -> FilePath
startupScriptPath spec = spec.workDir <> "/startup-script.sh"

caKey :: Spec -> Keys.SSHKeyPair
caKey spec = Keys.SSHKeyPair Keys.ED25519 (spec.workDir <> "/ssh") "toy-ca"

clientKey :: Spec -> Keys.SSHKeyPair
clientKey spec = Keys.SSHKeyPair Keys.ED25519 (spec.workDir <> "/ssh") "toy-client"

