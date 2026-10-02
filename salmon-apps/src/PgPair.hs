{-# LANGUAGE OverloadedStrings #-}

{- | A two-machine Postgres pair with a declared primary, and the bouncer
that follows it.

The whole point of this binary is that moving the primary is an /edit/, not
a procedure: one word of the seed changes, and a pass makes the machines
agree with it.

> salmon-pgpair config --primary A --a HOST_A --b HOST_B --bouncer HOST_C | salmon-pgpair run up
> salmon-pgpair config --primary B --a HOST_A --b HOST_B --bouncer HOST_C | salmon-pgpair run up

What it assumes was done before it ever ran, because a recipe that ships
secrets has chosen a transport for everybody who uses it: the two machines
have a Postgres cluster, the bouncer has pgbouncer and a @userlist.txt@, and
all three have the @.pgpass@ files named below. See @specs\/pg-switchover.md@,
and "SreBox.PostgresPairPrereqs" for that layer as nodes, which a binary
composing the pair as a library can declare (this one does not).
-}
module PgPair (main) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Exception (throwIO)
import Options.Applicative (auto, execParser, fullDesc, header, help, helper, info, long, option, optional, progDesc, strOption, switch, value, (<**>))
import Options.Generic (ParseRecord (..))

import qualified Salmon.Actions.Serve as Serve
import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension (Track')
import Salmon.Op.Configure (Configure (..))
import Salmon.Op.Track (Track (..))
import Salmon.Reporter (reportPrint)

import qualified SreBox.PostgresPair as Pair

main :: IO ()
main = do
    let desc =
            fullDesc
                <> progDesc "A Postgres primary and standby, with the primary's location declared rather than discovered"
                <> header "salmon-pgpair"
    cmd <- execParser (info parseRecord desc)
    CLI.execCommandOrSeedWith Serve.reportText reportPrint configure program cmd

-- | The directive is the pair itself: there is nothing to resolve later.
program :: Track' Pair.Pair
program = Track (Pair.pairOp reportPrint)

configure :: Configure IO Seed Pair.Pair
configure = Configure $ \seed ->
    case toPair seed of
        Right pair -> pure pair
        Left why -> throwIO (userError ("salmon-pgpair: " <> why))

{- | What an operator types. Everything else about the pair is convention,
which is what makes the interesting part -- @--primary@ -- short enough to
be obviously the only thing that changed.
-}
data Seed
    = Seed
    { seedName :: Text
    , seedPrimary :: Side
    , seedMayDiscard :: Maybe Side
    , seedSeed :: Maybe Side
    , seedReseed :: Maybe Side
    , seedHostA :: Text
    , seedHostB :: Text
    , seedSshA :: Maybe Text
    , seedSshB :: Maybe Text
    , seedBouncer :: Maybe Text
    , seedIdentity :: Maybe FilePath
    , seedIdentityA :: Maybe FilePath
    , seedIdentityB :: Maybe FilePath
    , seedIdentityBouncer :: Maybe FilePath
    , seedKnownHosts :: Maybe FilePath
    , seedSshUser :: Text
    , seedDatabase :: Text
    , seedConnSecurity :: Security
    , seedTlsCa :: Maybe FilePath
    , seedTlsVerifyFull :: Bool
    , seedTlsServerCert :: Maybe FilePath
    , seedTlsServerKey :: Maybe FilePath
    }

{- | @--conn-security@'s three words. The weakest is the default and has the
plain name, because it is what every pair made before the flag existed is.
-}
data Security = Plain | TlsScram | TlsCert

instance Read Security where
    readsPrec _ s = case span (`notElem` (" \t" :: String)) s of
        ("plain", rest) -> [(Plain, rest)]
        ("tls-scram", rest) -> [(TlsScram, rest)]
        ("tls-cert", rest) -> [(TlsCert, rest)]
        _ -> []

-- | Parsed rather than derived, so that @--primary A@ is what it looks like.
newtype Side = Side {unSide :: Pair.Side}

instance Read Side where
    readsPrec _ s = case span (`notElem` (" \t" :: String)) s of
        ("A", rest) -> [(Side Pair.A, rest)]
        ("a", rest) -> [(Side Pair.A, rest)]
        ("B", rest) -> [(Side Pair.B, rest)]
        ("b", rest) -> [(Side Pair.B, rest)]
        _ -> []

instance ParseRecord Seed where
    parseRecord = build <**> helper
      where
        build =
            Seed
                <$> strOption (long "name" <> help "what to call this pair" <> value "app")
                <*> option auto (long "primary" <> help "which machine should be the primary: A or B")
                <*> optional
                    ( option
                        auto
                        ( long "may-discard"
                            <> help "accept losing the writes on this side that the other does not have; the only thing that makes a failover possible, and it must name the side that is not --primary"
                        )
                    )
                <*> optional
                    ( option
                        auto
                        ( long "seed"
                            <> help "build this side's cluster from the other one: the first clone of a pair's life (does nothing once the two share a cluster; see --reseed for a standby that has fallen too far behind)"
                        )
                    )
                <*> optional
                    ( option
                        auto
                        ( long "reseed"
                            <> help "this side's data may be thrown away and cloned again from the other, if (and only if) the primary reports its replication slot lost; it must name the side that is not --primary"
                        )
                    )
                <*> strOption (long "a" <> help "machine A")
                <*> strOption (long "b" <> help "machine B")
                -- the address the peer and the bouncers use is not always
                -- one the controller can reach: a VPC's internal address
                -- against the external one an operator sees.
                <*> optional (strOption (long "ssh-a" <> help "where to ssh for machine A, when that is not the address its peer reaches it on (--a)"))
                <*> optional (strOption (long "ssh-b" <> help "where to ssh for machine B, when that is not the address its peer reaches it on (--b)"))
                <*> optional (strOption (long "bouncer" <> help "a pgbouncer whose clients should follow the primary"))
                <*> optional (strOption (long "ssh-identity" <> help "a key to reach the machines with"))
                -- a fleet usually has one key; these exist because a test
                -- harness that mints a CA per guest does not.
                <*> optional (strOption (long "ssh-identity-a" <> help "a key for machine A alone"))
                <*> optional (strOption (long "ssh-identity-b" <> help "a key for machine B alone"))
                <*> optional (strOption (long "ssh-identity-bouncer" <> help "a key for the bouncer alone"))
                <*> optional (strOption (long "ssh-known-hosts" <> help "a known-hosts file to learn the machines' keys into"))
                -- stock cloud images refuse root logins; anything but root
                -- has every script run under one `sudo -n`.
                <*> strOption (long "ssh-user" <> help "who to ssh as, on the machines and the bouncer: root, or a login with passwordless sudo" <> value "root")
                <*> strOption (long "db" <> help "the database clients connect to" <> value "app")
                <*> option
                    auto
                    ( long "conn-security"
                        <> help "what the replication and rewind connections between the two machines must be: plain (host, md5, TLS not required; the default), tls-scram (hostssl, scram-sha-256) or tls-cert (hostssl, client certificates pre-provisioned on both machines as /etc/postgresql/salmon-{replication,rewind}.{crt,key})"
                        <> value Plain
                    )
                <*> optional (strOption (long "tls-ca" <> help "a CA certificate, on both machines, to verify the other machine's certificate against (sslmode=verify-ca); without it a TLS connection is encrypted and the server is not verified"))
                <*> switch (long "tls-verify-full" <> help "with --tls-ca: also require the other machine's certificate to name the address given as --a/--b")
                <*> optional (strOption (long "tls-server-cert" <> help "the certificate each machine serves TLS with, pre-provisioned at this path on both; without it the cluster's TLS settings are left as found"))
                <*> optional (strOption (long "tls-server-key" <> help "its key, readable by postgres alone"))

toPair :: Seed -> Either String Pair.Pair
toPair seed = do
    security <- connSecurity seed
    let pair = (plainPair seed){Pair.pair_conn_security = security}
    case Pair.securityProblems pair of
        [] -> Right pair
        problems -> Left (Text.unpack (Text.intercalate "; " problems))

{- | The flags as a 'Pair.ConnSecurity', or what is contradictory about them.
'Nothing' for @plain@, so that such a directive is byte for byte what it was
before the flag existed.
-}
connSecurity :: Seed -> Either String (Maybe Pair.ConnSecurity)
connSecurity seed = case seed.seedConnSecurity of
    Plain
        | tlsFlagGiven -> Left "--tls-ca, --tls-verify-full, --tls-server-cert and --tls-server-key mean nothing with --conn-security plain"
        | otherwise -> Right Nothing
    TlsScram -> Just . Pair.TlsScram <$> tls
    TlsCert -> do
        t <- tls
        pure . Just $
            Pair.TlsClientCert
                t
                Pair.ClientCerts
                    { Pair.client_repl_cert = "/etc/postgresql/salmon-replication.crt"
                    , Pair.client_repl_key = "/etc/postgresql/salmon-replication.key"
                    , Pair.client_rewind_cert = "/etc/postgresql/salmon-rewind.crt"
                    , Pair.client_rewind_key = "/etc/postgresql/salmon-rewind.key"
                    }
  where
    tlsFlagGiven =
        seed.seedTlsVerifyFull
            || any (/= Nothing) [seed.seedTlsCa, seed.seedTlsServerCert, seed.seedTlsServerKey]
    tls = Pair.Tls <$> check <*> server
    check = case (seed.seedTlsCa, seed.seedTlsVerifyFull) of
        (Nothing, False) -> Right Pair.Encrypted
        (Nothing, True) -> Left "--tls-verify-full needs --tls-ca: there is nothing to verify a name against"
        (Just ca, False) -> Right (Pair.VerifyCa ca)
        (Just ca, True) -> Right (Pair.VerifyFull ca)
    server = case (seed.seedTlsServerCert, seed.seedTlsServerKey) of
        (Nothing, Nothing) -> Right Nothing
        -- the CA that signs the servers is taken to sign the clients too:
        -- one CA per pair is the convention this binary is made of.
        (Just cert, Just key) -> Right (Just (Pair.ServerFiles cert key seed.seedTlsCa))
        _ -> Left "--tls-server-cert and --tls-server-key go together"

plainPair :: Seed -> Pair.Pair
plainPair seed =
    Pair.Pair
        { Pair.pair_name = seed.seedName
        , Pair.pair_a = machine (seed.seedIdentityA <|> seed.seedIdentity) seed.seedSshA seed.seedHostA
        , Pair.pair_b = machine (seed.seedIdentityB <|> seed.seedIdentity) seed.seedSshB seed.seedHostB
        , Pair.pair_primary = unSide seed.seedPrimary
        , Pair.pair_repl_role = "replicator"
        , Pair.pair_repl_passfile = "/etc/postgresql/salmon-replication.pgpass"
        , Pair.pair_rewind_role = "rewinder"
        , Pair.pair_rewind_passfile = "/etc/postgresql/salmon-rewind.pgpass"
        , Pair.pair_ssh_known_hosts = seed.seedKnownHosts
        , Pair.pair_catch_up_seconds = 60
        , -- left declared is harmless: the clone does nothing once the two
          -- sides share a cluster, and refuses a machine holding somebody
          -- else's.
          Pair.pair_seed = fmap unSide seed.seedSeed
        , Pair.pair_bouncers = foldMap (pure . bouncer) seed.seedBouncer
        , Pair.pair_may_discard = fmap unSide seed.seedMayDiscard
        , Pair.pair_reseed = fmap unSide seed.seedReseed
        , Pair.pair_conn_security = Nothing
        }
  where
    machine identity sshHost host =
        Pair.Member
            { Pair.member_ssh_user = seed.seedSshUser
            , Pair.member_ssh_host = sshHost
            , Pair.member_host = host
            , Pair.member_cluster = "main"
            , Pair.member_port = 5432
            , Pair.member_ssh_identity = identity
            }

    bouncer host =
        Pair.Bouncer
            { Pair.bouncer_name = host
            , Pair.bouncer_ssh_user = seed.seedSshUser
            , Pair.bouncer_ssh_host = host
            , Pair.bouncer_ssh_identity = seed.seedIdentityBouncer <|> seed.seedIdentity
            , -- pgbouncer's admin console is an ordinary connection to the
              -- database named `pgbouncer` on the port clients use
              Pair.bouncer_console_user = "router"
            , Pair.bouncer_console_port = 6432
            , Pair.bouncer_console_passfile = "/etc/pgbouncer/console.pgpass"
            , Pair.bouncer_alias = seed.seedDatabase
            , Pair.bouncer_dbname = seed.seedDatabase
            , Pair.bouncer_routing_path = "/etc/pgbouncer/routing.ini"
            , Pair.bouncer_config_dir = "/etc/pgbouncer"
            , Pair.bouncer_listen_port = 6432
            }
