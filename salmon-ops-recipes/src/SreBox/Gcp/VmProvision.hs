{- | Turns up a GCE instance and then runs a Salmon binary on it over SSH,
composing the builtins under "Salmon.Builtin.Nodes.Gcp" with the existing
'Self.uploadAndCallSelfAsSudo' machinery -- the piece @specs/gcloud-support.md@
section 6 calls "the objective" and which nothing before this module actually
wired up.

The SSH trust model is plan section 5's Option B (project-metadata SSH-CA):
we generate an SSH CA once with "Salmon.Builtin.Nodes.Keys" (so its private
key is treated like any other salmon-managed secret, per section 14, rather
than a bare 'FilePath' the caller has to have produced out of band), push the
CA's public key into project metadata with
'Salmon.Builtin.Nodes.Gcp.SshAccess.installMetadataCaKey', and sign a
short-lived client certificate for the connecting user with 'Keys.signKey'.
'Keys.signKey' writes that certificate next to the client's private key as
@\<key\>-cert.pub@, which is exactly the name @ssh -i \<key\>@ auto-loads, so
'SshAccess.sshAvailable' probing with that private key exercises the same
certificate the eventual @ssh@ call to run the self binary will use.

Dependency shape: the self-upload-and-call step depends on 'sshAvailable'
succeeding (plan section 4's own recommendation, not followed by its section
6 pseudocode) rather than only on the instance's GCE-level @RUNNING@ status,
because @RUNNING@ says nothing about sshd being reachable yet.
-}
module SreBox.Gcp.VmProvision (
    VmProvisionConfig (..),
    provisionedVm,
    Report (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Extension
import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Nodes.Binary (Binary)
import qualified Salmon.Builtin.Nodes.Gcp.Compute as Compute
import qualified Salmon.Builtin.Nodes.Gcp.SshAccess as SshAccess
import Salmon.Builtin.Nodes.Keys (SSHKeyPair)
import qualified Salmon.Builtin.Nodes.Keys as Keys
import qualified Salmon.Builtin.Nodes.Self as Self
import qualified Salmon.Builtin.Nodes.Ssh as Ssh
import System.FilePath (takeDirectory, (</>))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = RunCompute !Compute.Report
    | RunSshAccess !SshAccess.Report
    | RunKeys !Keys.Report
    | RunSelf !Self.Report
    deriving (Show)

-------------------------------------------------------------------------------

{- | Everything needed to bring up a GCE VM over SSH and hand it the rest of
its setup as a @directive@, run by a re-uploaded copy of the calling binary.

The SSH endpoint (host/port/user) is supplied by the caller rather than
derived from the 'Compute.Instance', mirroring
@specs/gcloud-support.md@ section 6's own signature: an ephemeral instance's
address generally is not known until after 'up' runs, so a recipe that needs
one has to either reserve a static address up front or thread it through
some other channel -- deriving it here would just move that problem, not
solve it.
-}
data VmProvisionConfig directive = VmProvisionConfig
    { vmp_name :: Text
    -- ^ a unique name for this provisioning declaration, used as the 'Ref' key.
    , vmp_instance :: Compute.Instance
    , vmp_ca :: SSHKeyPair
    -- ^ the CA whose public key is pushed into project metadata; instances
    -- must be configured (e.g. via a startup script writing @sshd_config@'s
    -- @TrustedUserCAKeys@) to trust it, which is outside this module's scope.
    , vmp_clientIdentity :: SSHKeyPair
    -- ^ the key salmon connects with, signed by 'vmp_ca'.
    , vmp_sshUser :: Text
    , vmp_sshHost :: Text
    , vmp_sshPort :: Int
    , vmp_prerequisites :: [Op]
    -- ^ nodes that must be up /before the instance is created/ -- a
    -- startup-script file the instance's metadata points at, a firewall rule
    -- its sshd needs, the address it claims. They inject into the instance
    -- rather than into this recipe's root, because a root only orders itself
    -- after both, which would let the instance boot first.
    , vmp_remoteDir :: FilePath
    -- ^ where the self binary is uploaded on the VM.
    , vmp_selfPath :: Self.SelfPath
    , vmp_directiveTrack :: Track' directive
    , vmp_directive :: directive
    }

{- | How this recipe's ssh, rsync and probe all authenticate: the signed
client key, and a known-hosts file kept beside it rather than in the calling
user's @~\/.ssh@.

The second half matters as much as the first. These machines are disposable
and their addresses are not: rebuild a VM behind a reserved IP and every
later connection fails with @REMOTE HOST IDENTIFICATION HAS CHANGED@ against
the entry the /previous/ machine left behind. Keeping the file next to the
key scopes that record to this recipe (and lets
'SshAccess.sshAvailable' clear a stale entry when it sees one), instead of
leaving a landmine in a file the operator shares with everything else.
-}
clientOpts :: VmProvisionConfig directive -> Ssh.ClientOpts
clientOpts cfg =
    Ssh.ClientOpts
        { Ssh.optIdentity = Just key
        , Ssh.optKnownHosts = Just (takeDirectory key </> "known_hosts")
        }
  where
    key = Keys.privateKeyPath cfg.vmp_clientIdentity

{- | Creates the instance, installs SSH-CA trust, waits for SSH to answer,
then uploads and runs a copy of the calling binary against 'vmp_directive'.
-}
provisionedVm ::
    forall directive.
    (FromJSON directive, ToJSON directive) =>
    Reporter Report ->
    Track' (Binary "gcloud") ->
    Track' (Binary "ssh-keygen") ->
    VmProvisionConfig directive ->
    Op
provisionedVm r gcloudTrack keygenTrack cfg =
    op "gcp-vm-provision" (deps [trackedGraph call `inject` sshReady]) $ \actions ->
        actions
            { help = Text.unwords ["provisions GCE VM", cfg.vmp_instance.instanceName, "over SSH and runs the self binary on it"]
            , ref = mkRef "gcp-vm-provision" cfg.vmp_name
            }
  where
    rCompute = contramap RunCompute r
    rSshAccess = contramap RunSshAccess r
    rKeys = contramap RunKeys r
    rSelf = contramap RunSelf r

    -- The CA's public key has to be in project metadata *before* the
    -- instance boots: the instance's startup script reads it from there to
    -- set sshd's TrustedUserCAKeys, and a boot that happens first trusts
    -- nobody until the next one.
    vm :: Op
    vm =
        foldl
            inject
            (Compute.gceInstance rCompute gcloudTrack cfg.vmp_instance)
            (sshCa : cfg.vmp_prerequisites)

    caKey :: Op
    caKey = Keys.sshKey rKeys keygenTrack cfg.vmp_ca

    sshCa :: Op
    sshCa =
        SshAccess.installMetadataCaKey
            rSshAccess
            gcloudTrack
            (SshAccess.MetadataSshCa cfg.vmp_instance.instanceProject (Keys.publicKeyPath cfg.vmp_ca))
            `inject` caKey

    signedClient :: Op
    signedClient =
        Keys.signKey
            rKeys
            keygenTrack
            (Keys.SSHCertificateAuthority cfg.vmp_ca)
            (Keys.KeyIdentifier cfg.vmp_sshUser)
            [Keys.Principal cfg.vmp_sshUser]
            cfg.vmp_clientIdentity

    sshReady :: Op
    sshReady =
        SshAccess.sshAvailable
            rSshAccess
            (SshAccess.SshEndpoint (Just cfg.vmp_sshUser) cfg.vmp_sshHost cfg.vmp_sshPort (clientOpts cfg))
            `inject` vm
            `inject` signedClient

    call :: Tracked' (Self.RemoteCall directive)
    call =
        -- The key this recipe just had signed lives at a path of its own
        -- choosing, which ssh has no reason to offer otherwise.
        Self.uploadAndCallSelfAsSudoWith
            (clientOpts cfg)
            rSelf
            rSelf
            cfg.vmp_remoteDir
            (Self.Remote cfg.vmp_sshUser cfg.vmp_sshHost)
            cfg.vmp_selfPath
            Ssh.preExistingRemoteMachine
            cfg.vmp_directiveTrack
            CLI.Up
            cfg.vmp_directive
