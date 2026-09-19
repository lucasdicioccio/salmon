module Salmon.Builtin.Nodes.Certificates where

import Salmon.Actions.UpDown (CheckResult (..), skipIfFileExists)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process.ByteString (readCreateProcessWithExitCode)
import System.Process.ListLike (CreateProcess, proc)

-------------------------------------------------------------------------------
data Report
    = RunOpenSSLCommand !OpenSSLCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

newtype Domain = Domain {getDomain :: Text}
    deriving (Show, Ord, Eq)

data KeyType
    = RSA2048
    | RSA4096
    deriving (Show, Ord, Eq)

data Key
    = Key
    { keyType :: KeyType
    , keyDir :: FilePath
    , keyName :: Text
    }
    deriving (Show, Ord, Eq)

data SigningRequest
    = SigningRequest
    { certDomain :: Domain
    , certKey :: Key
    , certCSRDir :: FilePath
    , certCSRName :: Text
    }
    deriving (Show, Ord, Eq)

csrPath :: SigningRequest -> FilePath
csrPath req = req.certCSRDir </> Text.unpack req.certCSRName

derPath :: SigningRequest -> FilePath
derPath req = csrPath req <> ".der"

data SelfSigned
    = SelfSigned
    { selfSignedPEMPath :: FilePath
    , selfSignedRequest :: SigningRequest
    }
    deriving (Show, Ord, Eq)

{- | A certificate authority salmon owns: a key, and a self-signed certificate
naming it.

This exists for the case where the /verifier/ and the /issuer/ are both
configured by the same graph, which public CAs (see
"Salmon.Builtin.Nodes.Acme") are no help with at all. The motivating one is
Postgres client-certificate authentication: the server is told to trust this
CA and nothing else, and each client gets a certificate from it whose
@CN@ /is/ the database role it logs in as.

Two things follow from it being a long-lived root. Its key lives in a
'retainedDir' like every other key here, so a teardown archives rather than
deletes it -- losing it invalidates nothing, but it does mean no certificate
can ever be issued again to the fleet that trusts it. And its validity is
explicit ('caValidityDays'), because a CA that outlives its leaves by less
than their own lifetime quietly breaks every renewal.
-}
data CertificateAuthority
    = CertificateAuthority
    { caKey :: Key
    , caCertPath :: FilePath
    , caCommonName :: Domain
    , caValidityDays :: Int
    }
    deriving (Show, Ord, Eq)

-- | A certificate signed by a 'CertificateAuthority' rather than by itself.
data CaSigned
    = CaSigned
    { caSignedPEMPath :: FilePath
    , caSignedRequest :: SigningRequest
    , caSignedAuthority :: CertificateAuthority
    , caSignedValidityDays :: Int
    }
    deriving (Show, Ord, Eq)

tlsKey :: Reporter Report -> Track' (Binary "openssl") -> Key -> Op
tlsKey r bin key =
    withBinary bin openssl cmd $ \up -> do
        op "certificate-key" (deps [enclosingdir]) $ \actions ->
            actions
                { help = "generate a certificate-key"
                , notes =
                    [ "does not delete keys on down"
                    ]
                , ref = mkRef "openssl" path
                , check = skipIfFileExists path
                , up = up r'
                }
  where
    cmd = GenTLSKey key.keyType path
    r' = contramap (RunOpenSSLCommand cmd) r
    path :: FilePath
    path = keyPath key

    -- retained rather than plain 'dir': tearing down a cert down the line
    -- should leave old key/cert material lying around under a timestamped
    -- name rather than deleting it.
    enclosingdir :: Op
    enclosingdir = retainedDir (Directory key.keyDir)

keyPath :: Key -> FilePath
keyPath key = key.keyDir </> Text.unpack key.keyName

signingRequest :: Reporter Report -> Track' (Binary "openssl") -> SigningRequest -> Op
signingRequest r bin req =
    withCommand (GenCSR kpath csrpath dom) $ \makeCSR ->
        withCommand (ConvertCSR2DER csrpath derpath) $ \convert ->
            op "certificate-csr" (deps [enclosingdir, tlsKey r bin req.certKey]) $ \actions ->
                actions
                    { help = "generate a certificate signing request"
                    , ref = mkRef "openssl-csr" csrpath
                    , up = void $ makeCSR >> convert
                    }
  where
    r' cmd = contramap (RunOpenSSLCommand cmd) r
    withCommand cmd f =
        let
            g :: (Reporter Binary.Report -> IO ()) -> Op
            g callbin = f (callbin (r' cmd))
         in
            withBinary bin openssl cmd g

    kpath :: FilePath
    kpath = keyPath req.certKey

    csrpath :: FilePath
    csrpath = csrPath req

    derpath :: FilePath
    derpath = derPath req

    enclosingdir :: Op
    enclosingdir = retainedDir (Directory csrdir)

    csrdir :: FilePath
    csrdir = req.certCSRDir

    dom :: Domain
    dom = req.certDomain

selfSign :: Reporter Report -> Track' (Binary "openssl") -> SelfSigned -> Op
selfSign r bin selfsigned =
    withBinary bin openssl cmd $ \up ->
        op "certificate-self-sign" (deps [signingRequest r bin selfsigned.selfSignedRequest]) $ \actions ->
            actions
                { help = "self sign a certificate"
                , ref = mkRef "openssl-selfsign" pempath
                , check = checkCertNotExpiringSoon pempath
                , up = up r'
                }
  where
    cmd = SignCSR csr key pempath
    r' = contramap (RunOpenSSLCommand cmd) r
    key :: FilePath
    key = keyPath selfsigned.selfSignedRequest.certKey

    csr :: FilePath
    csr = csrPath selfsigned.selfSignedRequest

    pempath :: FilePath
    pempath = selfsigned.selfSignedPEMPath

{- | Generates the CA's own self-signed certificate (its key comes from
'tlsKey', as a dependency).

Unlike 'selfSign' this goes through @openssl req -x509@ rather than
@openssl x509 -req@, which is what marks the result as a CA
(@basicConstraints=critical,CA:TRUE@ is added by @req -x509@) -- a
certificate signed the other way is not accepted as an issuer, however much
it looks like one.
-}
certificateAuthority :: Reporter Report -> Track' (Binary "openssl") -> CertificateAuthority -> Op
certificateAuthority r bin ca =
    withBinary bin openssl cmd $ \up ->
        op "certificate-authority" (deps [enclosingdir, tlsKey r bin ca.caKey]) $ \actions ->
            actions
                { help = "self-signs the CA certificate " <> getDomain ca.caCommonName
                , notes = ["losing this key means nothing can ever be issued to the fleet that trusts it"]
                , ref = mkRef "openssl-ca" ca.caCertPath
                , check = checkCertNotExpiringSoon ca.caCertPath
                , up = up r'
                }
  where
    cmd = GenSelfSignedCa (keyPath ca.caKey) ca.caCertPath ca.caCommonName ca.caValidityDays
    r' = contramap (RunOpenSSLCommand cmd) r

    enclosingdir :: Op
    enclosingdir = retainedDir (Directory (takeDirectory ca.caCertPath))

{- | Signs a 'SigningRequest' with a 'CertificateAuthority'.

The @CN@ that ends up in the certificate is the request's
'certDomain' -- which, for a Postgres client certificate, is not a domain at
all but the database role the holder will be authenticated as. 'Domain' is
just the @CN@ under an older name; nothing here parses it.

The authority arrives as a 'Track'' rather than being built here, because
the two cases a caller has are genuinely different graphs: a CA this graph
also creates (pass @Track (certificateAuthority r bin)@) and one that was
provisioned out of band and is simply present (pass 'ignoreTrack'). Baking
in the first would make the second declare a node that tries to overwrite
somebody else's root.
-}
caSign :: Reporter Report -> Track' (Binary "openssl") -> Track' CertificateAuthority -> CaSigned -> Op
caSign r bin caTrack signed =
    withBinary bin openssl cmd $ \up ->
        op "certificate-ca-sign" (deps [signingRequest r bin signed.caSignedRequest, run caTrack ca]) $ \actions ->
            actions
                { help = "signs " <> getDomain signed.caSignedRequest.certDomain <> " with CA " <> getDomain ca.caCommonName
                , ref = mkRef "openssl-ca-sign" signed.caSignedPEMPath
                , check = checkCertNotExpiringSoon signed.caSignedPEMPath
                , up = up r'
                }
  where
    ca = signed.caSignedAuthority
    cmd =
        SignCSRWithCa
            (csrPath signed.caSignedRequest)
            ca.caCertPath
            (keyPath ca.caKey)
            signed.caSignedPEMPath
            signed.caSignedValidityDays
    r' = contramap (RunOpenSSLCommand cmd) r

{- | 'Failure' if @path@ is missing, or if the certificate there is already
expired or will expire within a day (@openssl x509 -checkend 86400@) —
'Success' otherwise. Used in place of a plain 'skipIfFileExists' wherever a
node's effect is a certificate rather than an arbitrary file, so an
out-of-date self-signed or ACME-signed certificate is noticed and
regenerated rather than being treated as satisfied forever after the first
run. See "Salmon.Builtin.Nodes.Acme".@acmeChallenge_dns01@ for the ACME
side.
-}
checkCertNotExpiringSoon :: FilePath -> IO CheckResult
checkCertNotExpiringSoon path = do
    exists <- doesFileExist path
    if not exists
        then pure (Failure $ "missing: " <> Text.pack path)
        else do
            (code, _out, err) <-
                readCreateProcessWithExitCode
                    (proc "openssl" ["x509", "-checkend", "86400", "-noout", "-in", path])
                    ""
            pure $ case code of
                ExitSuccess -> Success
                ExitFailure _ ->
                    Failure $
                        "expired or expiring within a day: "
                            <> Text.pack path
                            <> ": "
                            <> Text.decodeUtf8With Text.lenientDecode err

data OpenSSLCommand
    = GenCSR FilePath FilePath Domain
    | ConvertCSR2DER FilePath FilePath
    | SignCSR FilePath FilePath FilePath
    | GenTLSKey KeyType FilePath
    | -- | key, output cert, CN, days
      GenSelfSignedCa FilePath FilePath Domain Int
    | -- | CSR, CA cert, CA key, output cert, days
      SignCSRWithCa FilePath FilePath FilePath FilePath Int
    deriving (Show)

openssl :: Command "openssl" OpenSSLCommand
openssl = Command $ \cmd ->
    case cmd of
        (GenTLSKey kt filepath) ->
            case kt of
                RSA2048 -> proc "openssl" ["genrsa", "-out", filepath, "2048"]
                RSA4096 -> proc "openssl" ["genrsa", "-out", filepath, "4096"]
        (GenCSR keyPath csrPath dom) ->
            proc
                "openssl"
                [ "req"
                , "-new"
                , "-key"
                , keyPath
                , "-out"
                , csrPath
                , "-subj"
                , Text.unpack $ "/CN=" <> getDomain dom
                ]
        (ConvertCSR2DER csrPath derPath) ->
            proc
                "openssl"
                [ "req"
                , "-in"
                , csrPath
                , "-outform"
                , "DER"
                , "-out"
                , derPath
                ]
        (SignCSR csrPath keyPath pemPath) ->
            proc
                "openssl"
                [ "x509"
                , "-req"
                , "-in"
                , csrPath
                , "-signkey"
                , keyPath
                , "-out"
                , pemPath
                ]
        (GenSelfSignedCa keyPath certPath dom days) ->
            proc
                "openssl"
                [ "req"
                , "-x509"
                , "-new"
                , "-sha256"
                , "-key"
                , keyPath
                , "-days"
                , show days
                , "-subj"
                , Text.unpack $ "/CN=" <> getDomain dom
                , "-out"
                , certPath
                ]
        (SignCSRWithCa csrPath caCertPath caKeyPath pemPath days) ->
            proc
                "openssl"
                [ "x509"
                , "-req"
                , "-sha256"
                , "-in"
                , csrPath
                , "-CA"
                , caCertPath
                , "-CAkey"
                , caKeyPath
                , -- without a serial file openssl refuses outright; with
                  -- this it creates one next to the CA cert and increments
                  -- it, which is what makes two certificates issued to the
                  -- same CN distinguishable at revocation time.
                  "-CAcreateserial"
                , "-days"
                , show days
                , "-out"
                , pemPath
                ]
