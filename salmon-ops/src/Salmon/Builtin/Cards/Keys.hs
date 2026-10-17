{- | Web UI cards for the SSH, key and certificate builtins
("Salmon.Builtin.Nodes.Keys", "Salmon.Builtin.Nodes.Certificates",
"Salmon.Builtin.Nodes.WireGuard"'s keys, "Salmon.Builtin.Nodes.Gcp.SshAccess"),
in the style of "Salmon.Builtin.Cards" (feature e593a79b). __Opt-in__,
attached per node with 'Salmon.Builtin.Cards.withCard'.

__A key card shows a path, never a key.__ What a card takes is the
declaration: a key type, a directory, a name, a certificate's @CN@, a host and
port. It never reads a file, so it cannot show a key's bytes, a fingerprint or
any digest of either; whether the file is there is the node's own check, drawn
as the card's verdict. The one link out (the project metadata page, for the
SSH CA key installed there) is built from the project id alone.
-}
module Salmon.Builtin.Cards.Keys (
    -- * SSH
    sshKey,
    sshCaSign,
    sshAvailable,
    metadataSshCa,

    -- * TLS certificates
    tlsKey,
    signingRequest,
    selfSigned,
    certificateAuthority,
    caSigned,

    -- * Other keys
    jwkKey,
    wgPrivateKey,
    wgPublicKey,

    -- * Before rendering
    sshKeyCard,
    sshCaSignCard,
    sshAvailableCard,
    metadataSshCaCard,
    tlsKeyCard,
    signingRequestCard,
    selfSignedCard,
    certificateAuthorityCard,
    caSignedCard,
    jwkKeyCard,
    wgPrivateKeyCard,
    wgPublicKeyCard,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Cards (Card (..), Link (..), Verdicts (..), renderCard)
import Salmon.Builtin.Cards.Gcp (consoleUrl)
import qualified Salmon.Builtin.Nodes.Certificates as Certs
import Salmon.Builtin.Nodes.Gcp.Core (Project (..))
import qualified Salmon.Builtin.Nodes.Gcp.SshAccess as SshAccess
import qualified Salmon.Builtin.Nodes.Keys as Keys

unchecked :: Text
unchecked = "no check result on this page yet"

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-- | The verdicts of a node whose check is "the file is there".
fileVerdicts :: Text -> Text -> Verdicts
fileVerdicts what toApply =
    Verdicts
        { verdictInPlace = what <> " is present"
        , verdictToApply = what <> " is missing: " <> toApply
        , verdictUnsure = "could not tell whether " <> what <> " is there"
        , verdictNoCheck = "the node declares no check"
        , verdictUnchecked = unchecked
        }

-- | The verdicts of a node whose check is 'Certs.checkCertNotExpiringSoon'.
certVerdicts :: Text -> Verdicts
certVerdicts toApply =
    Verdicts
        { verdictInPlace = "the certificate is present and valid for more than a day"
        , verdictToApply = "missing, expired or expiring within a day: " <> toApply
        , verdictUnsure = "could not tell whether the certificate is valid"
        , verdictNoCheck = "the node declares no check"
        , verdictUnchecked = unchecked
        }

-------------------------------------------------------------------------------
-- SSH

-- | For 'Keys.sshKey': the key type and the two paths, never the key.
sshKey :: Keys.SSHKeyPair -> Text
sshKey = renderCard . sshKeyCard

-- | 'sshKey' before rendering.
sshKeyCard :: Keys.SSHKeyPair -> Card
sshKeyCard k =
    Card
        { cardFacts =
            [ ("type", sshKeyType k.sshKeyType)
            , ("private key", Text.pack (Keys.privateKeyPath k))
            , ("public key", Text.pack (Keys.publicKeyPath k))
            ]
        , cardVerdicts = fileVerdicts "the private key" "up generates the pair"
        , cardUp = "ssh-keygen with no passphrase, once: an existing key is never regenerated"
        , cardDown = "nothing: keys are kept"
        , cardLinks = []
        , cardDocs = Just "ssh-key"
        }

sshKeyType :: Keys.KeyType -> Text
sshKeyType Keys.RSA2048 = "RSA 2048"
sshKeyType Keys.RSA4096 = "RSA 4096"
sshKeyType Keys.ED25519 = "Ed25519"

{- | For 'Keys.signKey' with this CA, identifier and principals, on this key:
the paths and the names the certificate is for, never a key.
-}
sshCaSign :: Keys.SSHCertificateAuthority -> Keys.KeyIdentifier -> [Keys.Principal] -> Keys.SSHKeyPair -> Text
sshCaSign ca kid ps k = renderCard (sshCaSignCard ca kid ps k)

-- | 'sshCaSign' before rendering.
sshCaSignCard :: Keys.SSHCertificateAuthority -> Keys.KeyIdentifier -> [Keys.Principal] -> Keys.SSHKeyPair -> Card
sshCaSignCard ca kid ps k =
    Card
        { cardFacts =
            [ ("identifier", kid.getIdentifier)
            , ("principals", Text.intercalate ", " (fmap (.getPrincipal) ps))
            , ("certificate", Text.pack (Keys.publicCAKeyPath k))
            , ("signed key", Text.pack (Keys.publicKeyPath k))
            , ("CA key", Text.pack (Keys.privateKeyPath ca.sshcaKey))
            ]
        , cardVerdicts = fileVerdicts "the certificate" "up signs the key with the CA"
        , cardUp = "ssh-keygen -s with the CA key, once: an existing certificate is not renewed"
        , cardDown = "nothing: the certificate is kept"
        , cardLinks = []
        , cardDocs = Just "ssh-ca-sign"
        }

-- | For 'SshAccess.sshAvailable' (and 'SshAccess.sshAvailableWithin'): the user, host and port.
sshAvailable :: SshAccess.SshEndpoint -> Text
sshAvailable = renderCard . sshAvailableCard

-- | 'sshAvailable' before rendering.
sshAvailableCard :: SshAccess.SshEndpoint -> Card
sshAvailableCard e =
    Card
        { cardFacts =
            [ ("host", e.sshHost)
            , ("port", tshow e.sshPort)
            , ("user", maybe "ssh's default" id e.sshUser)
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "ssh answers"
                , verdictToApply = "ssh does not answer: up waits for it, and fails if it never does"
                , verdictUnsure = "the probe gave no verdict"
                , verdictNoCheck = "the node declares no check"
                , verdictUnchecked = unchecked
                }
        , cardUp = "probes ssh until it answers (a new host key is accepted, a changed one refused)"
        , cardDown = "nothing: it only observes"
        , cardLinks = []
        , cardDocs = Just "gcp-ssh-available"
        }

{- | For 'SshAccess.installMetadataCaKey': the project and the public key's
path, with a link to the project's metadata page in the Cloud console.
-}
metadataSshCa :: SshAccess.MetadataSshCa -> Text
metadataSshCa = renderCard . metadataSshCaCard

-- | 'metadataSshCa' before rendering.
metadataSshCaCard :: SshAccess.MetadataSshCa -> Card
metadataSshCaCard c =
    Card
        { cardFacts =
            [ ("project", c.sshCaProject.projectId)
            , ("CA public key", Text.pack c.sshCaPublicKey)
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "the CA key is in the project metadata"
                , verdictToApply = "not in the project metadata: up adds it"
                , verdictUnsure = "could not ask the project"
                , verdictNoCheck = "no check: run up adds it every time (gcloud replaces the metadata key); under serve it parks and nothing watches it"
                , verdictUnchecked = unchecked
                }
        , cardUp = "gcloud compute project-info add-metadata, from the public key file"
        , cardDown = "nothing: the metadata key stays"
        , cardLinks = [Link "metadata in the Cloud console" (consoleUrl ["compute", "metadata"] c.sshCaProject)]
        , cardDocs = Just "gcp-metadata-ssh-ca"
        }

-------------------------------------------------------------------------------
-- TLS

-- | For 'Certs.tlsKey': the key type and path, never the key.
tlsKey :: Certs.Key -> Text
tlsKey = renderCard . tlsKeyCard

-- | 'tlsKey' before rendering.
tlsKeyCard :: Certs.Key -> Card
tlsKeyCard k =
    Card
        { cardFacts = [("type", tlsKeyType k.keyType), ("key", Text.pack (Certs.keyPath k))]
        , cardVerdicts = fileVerdicts "the key" "up generates it"
        , cardUp = "openssl genrsa, once: an existing key is never regenerated"
        , cardDown = "nothing: the key stays (its directory is archived, not deleted, on teardown)"
        , cardLinks = []
        , cardDocs = Just "tls-key"
        }

tlsKeyType :: Certs.KeyType -> Text
tlsKeyType Certs.RSA2048 = "RSA 2048"
tlsKeyType Certs.RSA4096 = "RSA 4096"

-- | For 'Certs.signingRequest': the CN and the paths.
signingRequest :: Certs.SigningRequest -> Text
signingRequest = renderCard . signingRequestCard

-- | 'signingRequest' before rendering.
signingRequestCard :: Certs.SigningRequest -> Card
signingRequestCard req =
    Card
        { cardFacts = requestFacts req
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "the request is written"
                , verdictToApply = "no request yet: up writes it"
                , verdictUnsure = "could not tell whether a request is there"
                , verdictNoCheck = "no check: run up rewrites the request (PEM and DER) every time; under serve it parks and nothing watches it"
                , verdictUnchecked = unchecked
                }
        , cardUp = "openssl req for this CN with the key, then the same request as DER"
        , cardDown = "nothing: the request stays"
        , cardLinks = []
        , cardDocs = Just "tls-signing-request"
        }

requestFacts :: Certs.SigningRequest -> [(Text, Text)]
requestFacts req =
    [ ("CN", req.certDomain.getDomain)
    , ("request", Text.pack (Certs.csrPath req))
    , ("key", Text.pack (Certs.keyPath req.certKey))
    ]

-- | For 'Certs.selfSign': the certificate's path and CN.
selfSigned :: Certs.SelfSigned -> Text
selfSigned = renderCard . selfSignedCard

-- | 'selfSigned' before rendering.
selfSignedCard :: Certs.SelfSigned -> Card
selfSignedCard s =
    Card
        { cardFacts = ("certificate", Text.pack s.selfSignedPEMPath) : requestFacts s.selfSignedRequest
        , cardVerdicts = certVerdicts "up signs the request with its own key"
        , cardUp = "openssl x509 -req, signed with the request's own key"
        , cardDown = "nothing: the certificate stays"
        , cardLinks = []
        , cardDocs = Just "tls-self-signed"
        }

-- | For 'Certs.certificateAuthority': its CN, validity and paths.
certificateAuthority :: Certs.CertificateAuthority -> Text
certificateAuthority = renderCard . certificateAuthorityCard

-- | 'certificateAuthority' before rendering.
certificateAuthorityCard :: Certs.CertificateAuthority -> Card
certificateAuthorityCard ca =
    Card
        { cardFacts =
            [ ("CN", ca.caCommonName.getDomain)
            , ("certificate", Text.pack ca.caCertPath)
            , ("key", Text.pack (Certs.keyPath ca.caKey))
            , ("validity", tshow ca.caValidityDays <> " days")
            ]
        , cardVerdicts = certVerdicts "up self-signs a new CA certificate with the same key"
        , cardUp = "openssl req -x509 with the CA key, marked as a CA"
        , cardDown = "nothing: the CA stays (losing its key means nothing can be issued to the fleet that trusts it)"
        , cardLinks = []
        , cardDocs = Just "tls-certificate-authority"
        }

-- | For 'Certs.caSign': the CN, the issuing CA and the paths.
caSigned :: Certs.CaSigned -> Text
caSigned = renderCard . caSignedCard

-- | 'caSigned' before rendering.
caSignedCard :: Certs.CaSigned -> Card
caSignedCard s =
    Card
        { cardFacts =
            ("certificate", Text.pack s.caSignedPEMPath)
                : requestFacts s.caSignedRequest
                <> [ ("CA", s.caSignedAuthority.caCommonName.getDomain)
                   , ("validity", tshow s.caSignedValidityDays <> " days")
                   ]
        , cardVerdicts = certVerdicts "up signs the request with the CA"
        , cardUp = "openssl x509 -req, signed with the CA's certificate and key"
        , cardDown = "nothing: the certificate stays"
        , cardLinks = []
        , cardDocs = Just "tls-ca-signed"
        }

-------------------------------------------------------------------------------
-- Other keys

-- | For 'Keys.jwkKey': the key type and path, never the key.
jwkKey :: Keys.JWKKeyPair -> Text
jwkKey = renderCard . jwkKeyCard

-- | 'jwkKey' before rendering.
jwkKeyCard :: Keys.JWKKeyPair -> Card
jwkKeyCard k =
    Card
        { cardFacts = [("type", sshKeyType k.jwkKeyType), ("key", Text.pack (Keys.jwkfilepath k))]
        , cardVerdicts = fileVerdicts "the key" "up generates it"
        , cardUp = "generates a JWK and writes it, once: an existing key is never regenerated"
        , cardDown = "nothing: keys are kept"
        , cardLinks = []
        , cardDocs = Just "jwk-key"
        }

-- | For 'Salmon.Builtin.Nodes.WireGuard.privateKey' at this path: the path, never the key.
wgPrivateKey :: FilePath -> Text
wgPrivateKey = renderCard . wgPrivateKeyCard

-- | 'wgPrivateKey' before rendering.
wgPrivateKeyCard :: FilePath -> Card
wgPrivateKeyCard path =
    Card
        { cardFacts = [("private key", Text.pack path)]
        , cardVerdicts = fileVerdicts "the private key" "up generates it"
        , cardUp = "wg genkey into the file, once: an existing key is never regenerated"
        , cardDown = "nothing: the key stays"
        , cardLinks = []
        , cardDocs = Just "wireguard-private-key"
        }

{- | For 'Salmon.Builtin.Nodes.WireGuard.publicKey' from this private key, at
this path: the two paths, never a key.
-}
wgPublicKey :: FilePath -> FilePath -> Text
wgPublicKey private path = renderCard (wgPublicKeyCard private path)

-- | 'wgPublicKey' before rendering.
wgPublicKeyCard :: FilePath -> FilePath -> Card
wgPublicKeyCard private path =
    Card
        { cardFacts = [("public key", Text.pack path), ("from", Text.pack private)]
        , cardVerdicts = fileVerdicts "the public key" "up derives it"
        , cardUp = "wg pubkey from the private key, once: a later change of the private key is not followed"
        , cardDown = "nothing: the key stays"
        , cardLinks = []
        , cardDocs = Just "wireguard-public-key"
        }
