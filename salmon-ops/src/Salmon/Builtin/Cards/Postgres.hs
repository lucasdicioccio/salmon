{- | Web UI cards for more of the Postgres builtins
("Salmon.Builtin.Nodes.Postgres"), in the style of "Salmon.Builtin.Cards"
(feature d8a79380; 'Salmon.Builtin.Cards.pgDatabase' is there already).
__Opt-in__, attached per node with 'Salmon.Builtin.Cards.withCard'.

A card takes only what the builtin already publishes in its @ref@ and
@help@: a role name, a port, a slot, a cluster, a setting, a file's path.
It never takes a password, so it cannot show one or any digest of one; for a
password file or a TLS key it shows the path only.
-}
module Salmon.Builtin.Cards.Postgres (
    -- * Cards
    user,
    userPassFile,
    replicationUser,
    replicationSlot,
    hbaLine,
    alterSystem,
    serverTls,

    -- * Before rendering
    userCard,
    userPassFileCard,
    replicationUserCard,
    replicationSlotCard,
    hbaLineCard,
    alterSystemCard,
    serverTlsCard,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Cards (Card (..), Verdicts (..), renderCard)
import qualified Salmon.Builtin.Nodes.Postgres as Postgres

unchecked :: Text
unchecked = "no check result on this page yet"

port_ :: Postgres.Port -> (Text, Text)
port_ p = ("port", Text.pack (show p))

-- the sentence for a builtin with no check
reapplied :: Text
reapplied = "no check: run up applies it every time (it is idempotent); under serve it parks and nothing watches it"

-- | For 'Postgres.user' on this port: the role, never the password.
user :: Postgres.Port -> Postgres.User -> Text
user p u = renderCard (userCard p u)

-- | 'user' before rendering.
userCard :: Postgres.Port -> Postgres.User -> Card
userCard p u =
    Card
        { cardFacts = [("role", u.userRole), port_ p]
        , cardVerdicts = userVerdicts "the declared one"
        , cardUp = "creates the login role if missing, then sets its password (either way)"
        , cardDown = "nothing: a role is never dropped by this node"
        , cardLinks = []
        , cardDocs = Just "postgres-user"
        }

userVerdicts :: Text -> Verdicts
userVerdicts which =
    Verdicts
        { verdictInPlace = "the role exists, may log in, and its password is " <> which
        , verdictToApply = "missing, not allowed to log in, or holding another password: up sets it"
        , verdictUnsure = "could not ask the cluster, or read the declared password"
        , verdictNoCheck = "the node declares no check"
        , verdictUnchecked = unchecked
        }

{- | For 'Postgres.userPassFile' on this port, with the password file at
this path: the role and the path, never the file's contents.
-}
userPassFile :: Postgres.Port -> FilePath -> Postgres.User -> Text
userPassFile p path u = renderCard (userPassFileCard p path u)

-- | 'userPassFile' before rendering.
userPassFileCard :: Postgres.Port -> FilePath -> Postgres.User -> Card
userPassFileCard p path u =
    Card
        { cardFacts = [("role", u.userRole), port_ p, ("password file", Text.pack path)]
        , cardVerdicts = userVerdicts "the file's"
        , cardUp = "creates the login role if missing, then sets its password from the file, read at that moment"
        , cardDown = "nothing: a role is never dropped by this node"
        , cardLinks = []
        , cardDocs = Just "postgres-user-passfile"
        }

-- | For 'Postgres.replicationUser' on this port: the role, never the password.
replicationUser :: Postgres.Port -> Postgres.User -> Text
replicationUser p u = renderCard (replicationUserCard p u)

-- | 'replicationUser' before rendering.
replicationUserCard :: Postgres.Port -> Postgres.User -> Card
replicationUserCard p u =
    Card
        { cardFacts = [("role", u.userRole), port_ p, ("attributes", "REPLICATION LOGIN")]
        , cardVerdicts = noCheckVerdicts "the role is there" "missing: up creates it"
        , cardUp = "creates the role with REPLICATION LOGIN if missing, then sets its password (either way)"
        , cardDown = "nothing: a role is never dropped by this node"
        , cardLinks = []
        , cardDocs = Just "postgres-replication-user"
        }

noCheckVerdicts :: Text -> Text -> Verdicts
noCheckVerdicts inPlace toApply =
    Verdicts
        { verdictInPlace = inPlace
        , verdictToApply = toApply
        , verdictUnsure = "could not ask the server"
        , verdictNoCheck = reapplied
        , verdictUnchecked = unchecked
        }

-- | For 'Postgres.replicationSlot' on this port.
replicationSlot :: Postgres.Port -> Postgres.ReplicationSlotName -> Text
replicationSlot p slot = renderCard (replicationSlotCard p slot)

-- | 'replicationSlot' before rendering.
replicationSlotCard :: Postgres.Port -> Postgres.ReplicationSlotName -> Card
replicationSlotCard p slot =
    Card
        { cardFacts = [("slot", slot), ("kind", "physical"), port_ p]
        , cardVerdicts = noCheckVerdicts "the slot is there" "missing: up creates it"
        , cardUp = "creates the physical replication slot unless pg_replication_slots has it"
        , cardDown = "nothing: the slot is left (a slot nobody reads from holds WAL back, so drop it by hand when its standby is gone)"
        , cardLinks = []
        , cardDocs = Just "postgres-replication-slot"
        }

-- | For 'Postgres.hbaLine' on this cluster.
hbaLine :: Postgres.ClusterName -> Text -> Text
hbaLine cluster line = renderCard (hbaLineCard cluster line)

-- | 'hbaLine' before rendering.
hbaLineCard :: Postgres.ClusterName -> Text -> Card
hbaLineCard cluster line =
    Card
        { cardFacts = [("cluster", cluster), ("line", line)]
        , cardVerdicts = noCheckVerdicts "the line is in pg_hba.conf" "missing: up appends it"
        , cardUp = "appends the line to pg_hba.conf unless it is there verbatim, then reloads the cluster"
        , cardDown = "nothing: the line stays (and a changed declaration leaves the old line in place)"
        , cardLinks = []
        , cardDocs = Just "postgres-hba-line"
        }

-- | For 'Postgres.alterSystemSet' on this port.
alterSystem :: Postgres.Port -> Text -> Text -> Text
alterSystem p param val = renderCard (alterSystemCard p param val)

-- | 'alterSystem' before rendering.
alterSystemCard :: Postgres.Port -> Text -> Text -> Card
alterSystemCard p param val =
    Card
        { cardFacts = [("setting", param), ("value", val), port_ p]
        , cardVerdicts = noCheckVerdicts "the setting is written" "not written yet: up writes it"
        , cardUp = "ALTER SYSTEM SET, written to postgresql.auto.conf; it takes effect on a reload or, for some settings, a restart"
        , cardDown = "nothing: the setting stays"
        , cardLinks = []
        , cardDocs = Just "postgres-alter-system"
        }

{- | For 'Postgres.serverTls' (and 'Postgres.serverTlsAfter') on this port:
the three files' paths, never what is in them (the key above all).
-}
serverTls :: Postgres.Port -> Postgres.ServerTls -> Text
serverTls p tls = renderCard (serverTlsCard p tls)

-- | 'serverTls' before rendering.
serverTlsCard :: Postgres.Port -> Postgres.ServerTls -> Card
serverTlsCard p tls =
    Card
        { cardFacts =
            [ port_ p
            , ("certificate", Text.pack tls.tls_certFile)
            , ("key", Text.pack tls.tls_keyFile)
            , ("client CA", Text.pack tls.tls_caFile)
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "TLS is on, as declared"
                , verdictToApply = "TLS is off or not as declared"
                , verdictUnsure = "could not ask the server"
                , verdictNoCheck = "no check of its own: the settings and the reload under it run on each pass; a broken certificate is not noticed until a client connects"
                , verdictUnchecked = unchecked
                }
        , cardUp = "nothing of its own: under it, ssl and the three file settings are set, then the configuration is reloaded"
        , cardDown = "nothing: TLS stays configured"
        , cardLinks = []
        , cardDocs = Just "postgres-server-tls"
        }
