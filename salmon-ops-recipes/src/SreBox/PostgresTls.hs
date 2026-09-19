{-# LANGUAGE OverloadedStrings #-}

{- | Postgres authenticating its clients by __certificate__ rather than by
password, and the material that makes that possible.

This is the half that
<https://dicioccio.fr/postgrest-over-cloudrun.html the PostgREST-on-Cloud-Run
write-up> leaves unspecified: it gives the client's side of the connection
(@sslmode=verify-ca sslcert=… sslkey=… sslrootcert=…@) and says nothing about
what the server has to be told for that string to work. The answer is four
settings, one @pg_hba.conf@ line, and three files with the right owner.

= Why certificates rather than a password

Because of where the client runs. A password reaching a serverless container
has to be stored somewhere it can be read at start-up, which is a secret
store, which is the same amount of machinery as a certificate -- and then the
password is /also/ replayable by anyone who ever sees it, while a certificate
is only usable by whoever holds the key. The clinching detail is
@clientcert=verify-full@: it requires the certificate's @CN@ to __equal the
database role__, so "who may connect as @api_owner@" becomes "who holds a
certificate this CA issued for that name", and there is no shared secret in
the system at all.

= What this module does not do

It does not move files between machines. The server material belongs on the
database host and the client material belongs wherever the client is
deployed from, and how they get there -- rsync, a secret manager, an operator
with a USB stick -- is the caller's business, expressed by handing in a
'Track''. Recipes here stay transport-agnostic about secrets on purpose.

= The three ways the pieces are usually arranged

* __One box.__ The CA, the server material and the client material are all
  generated on the database host: pass 'generatedAuthority' everywhere.
* __A CA somewhere else.__ The database host receives cert, key and CA cert
  that were signed elsewhere. Pass 'ignoreTrack' as the material track and
  give 'postgresClientCertAuth' the paths the files already occupy.
* __Client deployed from a workstation.__ The CA and 'generateClientMaterial'
  run there, the resulting three files are pushed into a secret store, and
  the database host only ever sees 'postgresClientCertAuth'. This is the
  Cloud Run shape.
-}
module SreBox.PostgresTls (
    Report (..),

    -- * The database side
    ClientAccess (..),
    PostgresClientCertConfig (..),
    postgresClientCertAuth,

    -- * Certificate material
    generatedAuthority,
    ServerMaterialConfig (..),
    generateServerMaterial,
    ClientMaterialConfig (..),
    generateClientMaterial,
    clientMaterialPaths,

    -- * Connecting as a client
    SslMode (..),
    renderSslMode,
    clientConnString,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath ((</>))
import System.Posix.Types (FileMode)

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary)
import qualified Salmon.Builtin.Nodes.Certificates as Certs
import qualified Salmon.Builtin.Nodes.Filesystem as FS
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = GenerateCertificate !Certs.Report
    | ConfigureCluster !Postgres.Report
    deriving (Show)

-------------------------------------------------------------------------------
-- The database side

{- | One role that may authenticate by certificate, and from where.

@ca_cidr@ is worth a thought rather than a default: a certificate is the
whole authentication, so a wide range is not the exposure it would be with a
password -- but it is still the difference between "an attacker needs the key"
and "an attacker needs the key /and/ a foothold in this network". Serverless
clients have no stable egress address without a NAT, which is the usual
reason this ends up wide.
-}
data ClientAccess = ClientAccess
    { ca_role :: Postgres.RoleName
    -- ^ must equal the @CN@ of the certificate this client presents
    , ca_database :: Postgres.DatabaseName
    , ca_cidr :: Postgres.AllowedCidr
    }
    deriving (Eq, Show)

{- | Everything the database host is told.

'pgcc_material' is the track that makes 'pgcc_tls'\'s three files exist:
'generateServerMaterial' when this graph issues them, 'ignoreTrack' when
something else already put them there.
-}
data PostgresClientCertConfig = PostgresClientCertConfig
    { pgcc_cluster :: Postgres.ClusterName
    , pgcc_port :: Postgres.Port
    , pgcc_tls :: Postgres.ServerTls
    , pgcc_owner :: Text
    -- ^ the OS user the cluster runs as, usually @postgres@
    , pgcc_clients :: [ClientAccess]
    }

{- | Serves TLS, trusts one CA, and lets the named roles in on a certificate.

Ordering is the whole content of this node and it is not obvious: the
@pg_hba.conf@ lines are injected __after__ the TLS settings, because a
@hostssl@ line on a cluster with @ssl = off@ is accepted and then matches
nothing, which presents as "my client hangs on a password prompt" a long way
from the cause.
-}
postgresClientCertAuth ::
    Reporter Report ->
    Track' (Binary "psql") ->
    Track' (Binary "pg_ctlcluster") ->
    Track' Postgres.ServerTls ->
    PostgresClientCertConfig ->
    Op
postgresClientCertAuth r psql pgctl materialTrack cfg =
    op "pg-client-cert-auth" (deps (map hba cfg.pgcc_clients)) $ \actions ->
        actions
            { help = Text.unwords ["authenticates", nRoles, "role(s) by certificate on", cfg.pgcc_cluster]
            , ref = mkRef "pg-client-cert-auth" (cfg.pgcc_cluster, cfg.pgcc_port)
            }
  where
    rPg = contramap ConfigureCluster r
    nRoles = Text.pack (show (length cfg.pgcc_clients))

    hba :: ClientAccess -> Op
    hba client =
        Postgres.allowClientCertFrom
            rPg
            pgctl
            cfg.pgcc_cluster
            client.ca_database
            client.ca_role
            client.ca_cidr
            `inject` tls

    tls :: Op
    tls =
        Postgres.serverTls rPg psql cfg.pgcc_port cfg.pgcc_tls
            `inject` ownedMaterial

    -- Postgres refuses to start with a group- or world-readable key, and
    -- says so in terms of permissions rather than of TLS. Declaring the
    -- ownership as its own node means that rule is enforced wherever the
    -- file came from -- including the 'ignoreTrack' case, where salmon did
    -- not write it and cannot know what mode it arrived with.
    ownedMaterial :: Op
    ownedMaterial =
        op "pg-tls-material" (deps [keyOwnership, certOwnership, caOwnership]) $ \actions ->
            actions
                { help = "places the cluster's TLS material with the ownership postgres demands"
                , ref = mkRef "pg-tls-material" (cfg.pgcc_port, cfg.pgcc_tls.tls_keyFile)
                }

    keyOwnership = owned cfg.pgcc_tls.tls_keyFile 0o600
    certOwnership = owned cfg.pgcc_tls.tls_certFile 0o644
    caOwnership = owned cfg.pgcc_tls.tls_caFile 0o644

    owned :: FilePath -> FileMode -> Op
    owned path mode =
        FS.ownedFile
            FS.FileOwnership
                { FS.ownedPath = path
                , FS.ownedUser = Just cfg.pgcc_owner
                , FS.ownedGroup = Just cfg.pgcc_owner
                , FS.ownedMode = mode
                }
            `inject` run materialTrack cfg.pgcc_tls

-------------------------------------------------------------------------------
-- Material

{- | A CA this graph owns. Pass the result to 'generateServerMaterial' and
'generateClientMaterial'; pass 'ignoreTrack' instead wherever the CA is
somebody else's and merely has to be present.
-}
generatedAuthority :: Reporter Report -> Track' (Binary "openssl") -> Track' Certs.CertificateAuthority
generatedAuthority r openssl =
    Track $ Certs.certificateAuthority (contramap GenerateCertificate r) openssl

{- | What to issue for the server.

'smc_commonName' is the name clients will use in @host=@ if they ever move
from @verify-ca@ to @verify-full@; under @verify-ca@ it is not checked at
all, which is a trap worth knowing about — a wrong name here costs nothing
until the day somebody tightens the client's @sslmode@.
-}
data ServerMaterialConfig = ServerMaterialConfig
    { smc_commonName :: Certs.Domain
    , smc_authority :: Certs.CertificateAuthority
    , smc_key :: Certs.Key
    , smc_csrDir :: FilePath
    , smc_validityDays :: Int
    , smc_tls :: Postgres.ServerTls
    -- ^ where the signed certificate, its key and the CA certificate land
    }

{- | Issues the cluster's certificate, as a track over the paths it fills in.

The CA certificate is /copied/ to 'Postgres.tls_caFile' rather than being
pointed at where it already lives, because the cluster reads that file as the
@postgres@ user and the CA's own directory is a
'Salmon.Builtin.Nodes.Filesystem.retainedDir' holding a private key that user
has no business being able to reach.
-}
generateServerMaterial ::
    Reporter Report ->
    Track' (Binary "openssl") ->
    Track' Certs.CertificateAuthority ->
    ServerMaterialConfig ->
    Track' Postgres.ServerTls
generateServerMaterial r openssl caTrack cfg =
    Track $ \_tls ->
        op "pg-server-material" (deps [copyCert, copyKey, copyCa]) $ \actions ->
            actions
                { help = "issues the cluster's TLS certificate"
                , ref = mkRef "pg-server-material" cfg.smc_tls.tls_certFile
                }
  where
    r' = contramap GenerateCertificate r

    request :: Certs.SigningRequest
    request =
        Certs.SigningRequest
            { Certs.certDomain = cfg.smc_commonName
            , Certs.certKey = cfg.smc_key
            , Certs.certCSRDir = cfg.smc_csrDir
            , Certs.certCSRName = Certs.getDomain cfg.smc_commonName <> ".csr"
            }

    pemPath :: FilePath
    pemPath = cfg.smc_csrDir </> Text.unpack (Certs.getDomain cfg.smc_commonName <> ".pem")

    signed :: Op
    signed =
        Certs.caSign
            r'
            openssl
            caTrack
            Certs.CaSigned
                { Certs.caSignedPEMPath = pemPath
                , Certs.caSignedRequest = request
                , Certs.caSignedAuthority = cfg.smc_authority
                , Certs.caSignedValidityDays = cfg.smc_validityDays
                }

    copyCert = FS.fileCopy pemPath cfg.smc_tls.tls_certFile `inject` signed
    copyKey = FS.fileCopy (Certs.keyPath cfg.smc_key) cfg.smc_tls.tls_keyFile `inject` signed
    copyCa = FS.fileCopy cfg.smc_authority.caCertPath cfg.smc_tls.tls_caFile `inject` run caTrack cfg.smc_authority

-------------------------------------------------------------------------------

{- | What to issue for one client.

'cmc_role' is both the @CN@ of the certificate and the Postgres role it will
be accepted as — they are the same string by construction here, which is the
only way @clientcert=verify-full@ ever succeeds. Getting them out of step is
the single most common way this setup fails, and it fails with
@certificate authentication failed for user@ rather than with anything about
names.
-}
data ClientMaterialConfig = ClientMaterialConfig
    { cmc_role :: Postgres.RoleName
    , cmc_authority :: Certs.CertificateAuthority
    , cmc_dir :: FilePath
    , cmc_keyType :: Certs.KeyType
    , cmc_validityDays :: Int
    }

{- | The three files a libpq client needs: key, certificate, CA certificate.

The key is left where 'Certs.tlsKey' put it (a @retainedDir@) and is
'Salmon.Builtin.Nodes.Filesystem.ownedFile'-restricted to @0600@, because
libpq refuses a key any wider — the same rule the server applies, enforced on
the other end and reported just as obscurely.
-}
generateClientMaterial ::
    Reporter Report ->
    Track' (Binary "openssl") ->
    Track' Certs.CertificateAuthority ->
    ClientMaterialConfig ->
    Op
generateClientMaterial r openssl caTrack cfg =
    op "pg-client-material" (deps [restrictedKey, signed, caCopy]) $ \actions ->
        actions
            { help = Text.unwords ["issues a client certificate for role", cfg.cmc_role]
            , ref = mkRef "pg-client-material" (cfg.cmc_dir, cfg.cmc_role)
            }
  where
    r' = contramap GenerateCertificate r
    paths = clientMaterialPaths cfg

    key :: Certs.Key
    key = Certs.Key cfg.cmc_keyType cfg.cmc_dir (cfg.cmc_role <> ".key")

    request :: Certs.SigningRequest
    request =
        Certs.SigningRequest
            { -- the CN *is* the role name; see the note on 'cmc_role'
              Certs.certDomain = Certs.Domain cfg.cmc_role
            , Certs.certKey = key
            , Certs.certCSRDir = cfg.cmc_dir
            , Certs.certCSRName = cfg.cmc_role <> ".csr"
            }

    signed :: Op
    signed =
        Certs.caSign
            r'
            openssl
            caTrack
            Certs.CaSigned
                { Certs.caSignedPEMPath = fst3 paths
                , Certs.caSignedRequest = request
                , Certs.caSignedAuthority = cfg.cmc_authority
                , Certs.caSignedValidityDays = cfg.cmc_validityDays
                }

    restrictedKey :: Op
    restrictedKey =
        FS.ownedFile
            FS.FileOwnership
                { FS.ownedPath = snd3 paths
                , FS.ownedUser = Nothing
                , FS.ownedGroup = Nothing
                , FS.ownedMode = 0o600
                }
            `inject` signed

    caCopy :: Op
    caCopy =
        FS.fileCopy cfg.cmc_authority.caCertPath (thd3 paths)
            `inject` run caTrack cfg.cmc_authority

-- | The (certificate, key, CA certificate) paths 'generateClientMaterial' fills in.
clientMaterialPaths :: ClientMaterialConfig -> (FilePath, FilePath, FilePath)
clientMaterialPaths cfg =
    ( cfg.cmc_dir </> Text.unpack (cfg.cmc_role <> ".pem")
    , cfg.cmc_dir </> Text.unpack (cfg.cmc_role <> ".key")
    , cfg.cmc_dir </> "ca.pem"
    )

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-------------------------------------------------------------------------------

{- | How hard a client checks the server.

@verify-ca@ proves the server's certificate was issued by the expected CA;
@verify-full@ additionally requires its name to match @host=@. With a private
CA the first already excludes everybody but this CA's holders, which is why
it is what the write-up uses — but it does not distinguish /which/ of them
answered, so a CA that also issues to untrusted parties needs the second.
-}
data SslMode
    = VerifyCa
    | VerifyFull
    deriving (Eq, Show)

renderSslMode :: SslMode -> Text
renderSslMode VerifyCa = "verify-ca"
renderSslMode VerifyFull = "verify-full"

{- | A libpq connection string authenticating by certificate.

Deliberately keyword/value form rather than a URI: the three file paths are
what libpq wants and a URI would have to percent-encode them, and this is the
string that ends up in a container's environment where it will be read by
people.

The paths are the client's view of where the files are __at run time__, which
is not necessarily where 'generateClientMaterial' wrote them — a secret
manager may well mount them somewhere else entirely.
-}
clientConnString ::
    Postgres.Server ->
    Postgres.DatabaseName ->
    Postgres.RoleName ->
    SslMode ->
    -- | (certificate, key, CA certificate), as the client will see them
    (FilePath, FilePath, FilePath) ->
    Text
clientConnString server db role mode (cert, key, ca) =
    Text.unwords
        [ "host=" <> server.serverHost
        , "port=" <> Text.pack (show server.serverPort)
        , "dbname=" <> db
        , "user=" <> role
        , "sslmode=" <> renderSslMode mode
        , "sslcert=" <> Text.pack cert
        , "sslkey=" <> Text.pack key
        , "sslrootcert=" <> Text.pack ca
        ]
