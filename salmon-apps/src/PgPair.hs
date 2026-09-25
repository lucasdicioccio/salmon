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
all three have the @.pgpass@ files named below. See @specs\/pg-switchover.md@.
-}
module PgPair (main) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative (auto, execParser, fullDesc, header, help, helper, info, long, option, optional, progDesc, strOption, value, (<**>))
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
configure = Configure (pure . toPair)

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
    , seedBouncer :: Maybe Text
    , seedIdentity :: Maybe FilePath
    , seedIdentityA :: Maybe FilePath
    , seedIdentityB :: Maybe FilePath
    , seedIdentityBouncer :: Maybe FilePath
    , seedKnownHosts :: Maybe FilePath
    , seedDatabase :: Text
    }

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
                <*> optional (strOption (long "bouncer" <> help "a pgbouncer whose clients should follow the primary"))
                <*> optional (strOption (long "ssh-identity" <> help "a key to reach the machines with"))
                -- a fleet usually has one key; these exist because a test
                -- harness that mints a CA per guest does not.
                <*> optional (strOption (long "ssh-identity-a" <> help "a key for machine A alone"))
                <*> optional (strOption (long "ssh-identity-b" <> help "a key for machine B alone"))
                <*> optional (strOption (long "ssh-identity-bouncer" <> help "a key for the bouncer alone"))
                <*> optional (strOption (long "ssh-known-hosts" <> help "a known-hosts file to learn the machines' keys into"))
                <*> strOption (long "db" <> help "the database clients connect to" <> value "app")

toPair :: Seed -> Pair.Pair
toPair seed =
    Pair.Pair
        { Pair.pair_name = seed.seedName
        , Pair.pair_a = machine (seed.seedIdentityA <|> seed.seedIdentity) seed.seedHostA
        , Pair.pair_b = machine (seed.seedIdentityB <|> seed.seedIdentity) seed.seedHostB
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
        }
  where
    machine identity host =
        Pair.Member
            { Pair.member_ssh_user = "root"
            , Pair.member_host = host
            , Pair.member_cluster = "main"
            , Pair.member_port = 5432
            , Pair.member_ssh_identity = identity
            }

    bouncer host =
        Pair.Bouncer
            { Pair.bouncer_name = host
            , Pair.bouncer_ssh_user = "root"
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
