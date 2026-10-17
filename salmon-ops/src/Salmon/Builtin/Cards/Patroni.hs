{- | Web UI cards for the Patroni, pgBackRest and HAProxy builtins
("Salmon.Builtin.Nodes.Patroni", "Salmon.Builtin.Nodes.Patroni.PreferredLeader",
"Salmon.Builtin.Nodes.PgBackRest", "Salmon.Builtin.Nodes.Haproxy"), in the
style of "Salmon.Builtin.Cards" (feature e593a79b). __Opt-in__, attached per
node with 'Salmon.Builtin.Cards.withCard'.

A card takes names, addresses, ports and paths from the declaration. It
never takes a credentials file's contents (Patroni's, the REST API's, or
pgBackRest's fragment, shown by path when there is one), nor an archive's
location beyond its kind: an S3 bucket and endpoint are not on the card.
Like the builtins, no card says which member leads: that is Patroni's, and
its REST API is not linked (it speaks plain HTTP, which the page does not
draw as a link).
-}
module Salmon.Builtin.Cards.Patroni (
    -- * Patroni
    patroniMember,
    preferredLeader,

    -- * pgBackRest
    pgBackRestConfiguration,
    pgBackRestStanza,
    pgBackRestFirstBackup,
    pgBackRestRestore,

    -- * HAProxy
    haproxy,

    -- * Before rendering
    patroniMemberCard,
    preferredLeaderCard,
    pgBackRestConfigurationCard,
    pgBackRestStanzaCard,
    pgBackRestFirstBackupCard,
    pgBackRestRestoreCard,
    haproxyCard,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Cards (Card (..), Verdicts (..), renderCard)
import qualified Salmon.Builtin.Nodes.Haproxy as Haproxy
import qualified Salmon.Builtin.Nodes.Patroni as Patroni
import Salmon.Builtin.Nodes.Patroni.PreferredLeader (PreferredLeader (..))
import qualified Salmon.Builtin.Nodes.PgBackRest as PgBackRest

unchecked :: Text
unchecked = "no check result on this page yet"

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

commas :: [Text] -> Text
commas = Text.intercalate ", "

-------------------------------------------------------------------------------
-- Patroni

{- | For 'Patroni.patroniMember': the scope, the member, its addresses and its
configuration directory; never the credentials file's contents, nor a role.
-}
patroniMember :: Patroni.PatroniConfig -> Text
patroniMember = renderCard . patroniMemberCard

-- | 'patroniMember' before rendering.
patroniMemberCard :: Patroni.PatroniConfig -> Card
patroniMemberCard c =
    Card
        { cardFacts =
            [ ("scope", c.pat_scope)
            , ("member", c.pat_name)
            , ("REST API", c.pat_rest_connect_address)
            , ("Postgres", c.pat_pg_connect_address)
            , ("Postgres version", c.pat_pg_version)
            , ("etcd", commas c.pat_etcd_hosts)
            , ("config dir", Text.pack c.pat_config_dir)
            , ("credentials file", Text.pack c.pat_secrets_file)
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "running and healthy, in the declared scope (its role is Patroni's, not judged here)"
                , verdictToApply = "not answering, not healthy, or in another scope: up starts it and waits for it"
                , verdictUnsure = "starting, or in a state Patroni calls transitional: nothing restarts on this"
                , verdictNoCheck = "the node declares no check"
                , verdictUnchecked = unchecked
                }
        , cardUp = "waits for the member to be healthy; under it, patroni.yml, the unit and the masked Debian cluster unit"
        , cardDown = "nothing of its own: the unit under it is stopped, and the data directory is left alone"
        , cardLinks = []
        , cardDocs = Just "patroni-member"
        }

{- | For 'Salmon.Builtin.Nodes.Patroni.PreferredLeader.preferredLeader': the
scope, the preferred member and its REST address; the credentials file by
path only.
-}
preferredLeader :: PreferredLeader -> Text
preferredLeader = renderCard . preferredLeaderCard

-- | 'preferredLeader' before rendering.
preferredLeaderCard :: PreferredLeader -> Card
preferredLeaderCard c =
    Card
        { cardFacts =
            [ ("scope", c.pl_scope)
            , ("preferred", c.pl_member)
            , ("REST API", c.pl_rest_address)
            , ("credentials file", maybe "none" Text.pack c.pl_credentials_file)
            , ("settle", tshow c.pl_settle_seconds <> " s")
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "the preferred member leads"
                , verdictToApply = "another member leads and the preferred one is eligible: up asks Patroni for a switchover"
                , verdictUnsure = "waiting: no leader, or the preferred member is not eligible right now"
                , verdictNoCheck = "the node declares no check"
                , verdictUnchecked = unchecked
                }
        , cardUp = "POST /switchover to the preferred member, then waits up to the settle time for it to lead"
        , cardDown = "nothing: the leader stays where it is"
        , cardLinks = []
        , cardDocs = Just "patroni-preferred-leader"
        }

-------------------------------------------------------------------------------
-- pgBackRest

-- the facts every pgBackRest card shares
pbrFacts :: PgBackRest.PgBackRestConfig -> [(Text, Text)]
pbrFacts c =
    [ ("stanza", c.pbr_stanza)
    , ("data dir", Text.pack c.pbr_pg_path)
    , ("repository", repoKind c.pbr_repo)
    , ("config", Text.pack c.pbr_config_file)
    ]
  where
    repoKind (PgBackRest.PosixRepo path) = "directory " <> Text.pack path
    repoKind PgBackRest.S3Repo{} = "S3"

-- | For 'PgBackRest.configuration': the stanza, paths, and the secrets fragment's path.
pgBackRestConfiguration :: PgBackRest.PgBackRestConfig -> Text
pgBackRestConfiguration = renderCard . pgBackRestConfigurationCard

-- | 'pgBackRestConfiguration' before rendering.
pgBackRestConfigurationCard :: PgBackRest.PgBackRestConfig -> Card
pgBackRestConfigurationCard c =
    Card
        { cardFacts = pbrFacts c <> [("secrets fragment", maybe "none" Text.pack c.pbr_secrets_file)]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "everything under it is in place"
                , verdictToApply = "something under it is missing: up applies it"
                , verdictUnsure = "could not tell what is under it"
                , verdictNoCheck = "no check of its own: the binary, pgbackrest.conf, the log and repository directories and the fragment's ownership are the nodes under it"
                , verdictUnchecked = unchecked
                }
        , cardUp = "nothing of its own: under it, the binary, pgbackrest.conf and the directories; a missing secrets fragment fails (writing it is not this node's job)"
        , cardDown = "nothing of its own"
        , cardLinks = []
        , cardDocs = Just "pgbackrest-configuration"
        }

-- | For 'PgBackRest.stanza'.
pgBackRestStanza :: PgBackRest.PgBackRestConfig -> Text
pgBackRestStanza = renderCard . pgBackRestStanzaCard

-- | 'pgBackRestStanza' before rendering.
pgBackRestStanzaCard :: PgBackRest.PgBackRestConfig -> Card
pgBackRestStanzaCard c =
    Card
        { cardFacts = pbrFacts c
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "the repository holds the stanza"
                , verdictToApply = "no stanza in the repository: up creates it (only the primary can)"
                , verdictUnsure = "could not read the repository"
                , verdictNoCheck = "the node declares no check"
                , verdictUnchecked = unchecked
                }
        , cardUp = "pgbackrest stanza-create (idempotent; fails on a member that is not the primary)"
        , cardDown = "nothing: the stanza and its archive stay"
        , cardLinks = []
        , cardDocs = Just "pgbackrest-stanza"
        }

-- | For 'PgBackRest.firstBackup'.
pgBackRestFirstBackup :: PgBackRest.PgBackRestConfig -> Text
pgBackRestFirstBackup = renderCard . pgBackRestFirstBackupCard

-- | 'pgBackRestFirstBackup' before rendering.
pgBackRestFirstBackupCard :: PgBackRest.PgBackRestConfig -> Card
pgBackRestFirstBackupCard c =
    Card
        { cardFacts = pbrFacts c <> [("full backups kept", tshow c.pbr_retention_full)]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "the repository holds a backup"
                , verdictToApply = "no backup yet: up takes a full one (only the primary can)"
                , verdictUnsure = "could not read the repository"
                , verdictNoCheck = "the node declares no check"
                , verdictUnchecked = unchecked
                }
        , cardUp = "pgbackrest backup --type=full, when the repository holds none; later backups are a schedule's"
        , cardDown = "nothing: backups stay"
        , cardLinks = []
        , cardDocs = Just "pgbackrest-first-backup"
        }

-- | For 'PgBackRest.restoredCluster' to this target.
pgBackRestRestore :: PgBackRest.PgBackRestConfig -> PgBackRest.RestoreTarget -> Text
pgBackRestRestore c t = renderCard (pgBackRestRestoreCard c t)

-- | 'pgBackRestRestore' before rendering.
pgBackRestRestoreCard :: PgBackRest.PgBackRestConfig -> PgBackRest.RestoreTarget -> Card
pgBackRestRestoreCard c t =
    Card
        { cardFacts = pbrFacts c <> [("target", tshow t)]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "the data directory holds a cluster: it is left alone, whatever it is"
                , verdictToApply = "the data directory holds no cluster: up restores into it"
                , verdictUnsure = "could not tell whether the data directory holds a cluster"
                , verdictNoCheck = "the node declares no check"
                , verdictUnchecked = unchecked
                }
        , cardUp = "pgbackrest restore without --delta or --force, so a directory holding anything is refused; starting the cluster is the next node's"
        , cardDown = "nothing: a restored cluster is data"
        , cardLinks = []
        , cardDocs = Just "pgbackrest-restore"
        }

-------------------------------------------------------------------------------
-- HAProxy

{- | For 'Haproxy.setup' (the haproxy service): the listeners, the members and
the configuration file. Which member leads is not on it, as it is not in
the configuration.
-}
haproxy :: Haproxy.HaproxyConfig -> Text
haproxy = renderCard . haproxyCard

-- | 'haproxy' before rendering.
haproxyCard :: Haproxy.HaproxyConfig -> Card
haproxyCard c =
    Card
        { cardFacts =
            [("config", Text.pack (Haproxy.configPath c))]
                <> [ ("listener " <> l.listener_name, l.listener_bind_addr <> ":" <> tshow l.listener_port <> " → " <> Haproxy.checkPath l.listener_role)
                   | l <- c.haproxy_listeners
                   ]
                <> [("members", commas [m.member_name <> " (" <> m.member_host <> ":" <> tshow m.member_pg_port <> ")" | m <- c.haproxy_members])]
                <> [("stats", a <> ":" <> tshow p) | Just (a, p) <- [c.haproxy_stats]]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "haproxy is active and enabled, as loaded"
                , verdictToApply = "not as declared: up reloads, enables and restarts it"
                , verdictUnsure = "in transition, or systemctl did not answer: nothing restarts on this"
                , verdictNoCheck = "the node declares no check"
                , verdictUnchecked = unchecked
                }
        , cardUp = "the systemd unit, restarted when haproxy.cfg changes (a changed member list drops connections in flight; a failover changes nothing here)"
        , cardDown = "systemctl stop; the configuration file stays"
        , cardLinks = []
        , cardDocs = Just "haproxy-service"
        }
