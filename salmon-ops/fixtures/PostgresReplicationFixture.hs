{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | A fixture demonstrating "Salmon.Builtin.Nodes.Postgres"'s WAL streaming
replication primitives end to end: one node running the (Debian-default,
\"main\") cluster as a primary, another cloning it and running as a
streaming standby.

This repo has no automated test suite (see CLAUDE.md) — like
'salmon-ops-dot-fixture', this binary is meant to be run and eyeballed by
hand, against two real machines (or, easiest, two podman containers on
podman's default network, which resolves containers by name via embedded
DNS). For example:

> podman run -dt --name pg-primary --hostname pg-primary debian:bookworm sleep infinity
> podman run -dt --name pg-standby --hostname pg-standby debian:bookworm sleep infinity
> podman exec pg-primary bash -c "apt-get update -qq && DEBIAN_FRONTEND=noninteractive apt-get install -y -qq sudo"
> podman exec pg-standby bash -c "apt-get update -qq && DEBIAN_FRONTEND=noninteractive apt-get install -y -qq sudo"
>
> cabal build salmon-postgres-replication-fixture
> BIN=dist-newstyle/build/*/*/salmon-ops-0.1.0.0/x/salmon-postgres-replication-fixture/build/salmon-postgres-replication-fixture/salmon-postgres-replication-fixture
> podman cp "$BIN" pg-primary:/usr/local/bin/fixture
> podman cp "$BIN" pg-standby:/usr/local/bin/fixture
>
> # allow the standby's container subnet to authenticate as the replication role
> podman exec pg-primary fixture primary 0.0.0.0/0
> podman exec pg-standby fixture standby pg-primary
>
> # eyeball it: the standby should now be streaming from the primary
> podman exec pg-standby sudo -u postgres psql -c "SELECT status FROM pg_stat_wal_receiver;"
> podman exec pg-primary sudo -u postgres psql -c "SELECT client_addr, state FROM pg_stat_replication;"

The @0.0.0.0\/0@ above is a fixture-only shortcut (no need to look up the
standby's actual container IP by hand) — a real deployment should pass the
standby's actual address\/CIDR instead, per "Postgres.primaryReplicationSetup"'s
own doc.

> salmon-postgres-replication-fixture dot primary 10.0.0.2/32   # print the primary-side graph
> salmon-postgres-replication-fixture dot standby pg-primary    # print the standby-side graph
-}
module Main (main) where

import Control.Monad (unless)
import Control.Monad.Identity (runIdentity)
import qualified Data.Text as Text
import System.Environment (getArgs)
import System.Exit (die, exitFailure)

import Salmon.Actions.Dot (printDigraph)
import Salmon.Actions.UpDown (upTree)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (justInstall)
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Track
import Salmon.Reporter (reportPrint)

-------------------------------------------------------------------------------
-- fixture-only constants: a real deployment would generate/store these per-pair, not hardcode them

replRole :: Postgres.RoleName
replRole = "replicator"

replPassword :: Postgres.Password
replPassword = Postgres.Password "fixture-replication-password"

replSlot :: Postgres.ReplicationSlotName
replSlot = "standby_slot"

primaryPort :: Postgres.Port
primaryPort = 5432

-------------------------------------------------------------------------------

-- | Ensures the (Debian-default) "main" cluster is installed and started, dependend on by both roles.
serverUp :: Op
serverUp = Postgres.pgLocalCluster reportPrint Debian.postgres Debian.pg_ctlcluster Postgres.localServer

serverTrack :: Track' Postgres.Server
serverTrack = Track $ const serverUp

-- | Primary role: creates the replication user, then turns "main" into a replication-capable primary.
primaryOp :: Text.Text -> Op
primaryOp standbyCidr =
    Postgres.primaryReplicationSetup
        reportPrint
        Debian.psql
        Debian.pg_ctlcluster
        Postgres.mainCluster
        Postgres.defaultReplicationTuning
        replRole
        standbyCidr
        replSlot
        `inject` replUser
        `inject` serverUp
  where
    replUser = Postgres.replicationUser reportPrint serverTrack Debian.psql (Postgres.User replRole) replPassword

-- | Standby role: installs postgres (for pg_basebackup) and clones "main" off the given primary host.
standbyOp :: Text.Text -> Op
standbyOp primaryHost =
    Postgres.standbyReplicationSetup reportPrint Debian.pg_ctlcluster standbySetup
        `inject` justInstall Debian.postgres
  where
    standbySetup =
        Postgres.StandbySetup
            { Postgres.standby_cluster = Postgres.mainCluster
            , Postgres.standby_primary_host = primaryHost
            , Postgres.standby_primary_port = primaryPort
            , Postgres.standby_repl_user = Postgres.User replRole
            , Postgres.standby_repl_password = replPassword
            , Postgres.standby_slot = Just replSlot
            }

-------------------------------------------------------------------------------

-- | Runs the graph and, unlike blindly ignoring 'upTree''s result, actually
-- exits non-zero if a command failed (and everything downstream of it got
-- 'Blocked') instead of reporting false success.
runUpOrDie :: Op -> IO ()
runUpOrDie o = do
    ok <- upTree reportPrint (pure . runIdentity) o
    unless ok exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["primary", standbyCidr] -> runUpOrDie (primaryOp (Text.pack standbyCidr))
        ["standby", primaryHost] -> runUpOrDie (standbyOp (Text.pack primaryHost))
        ["dot", "primary", standbyCidr] -> printDigraph (pure . runIdentity) (primaryOp (Text.pack standbyCidr))
        ["dot", "standby", primaryHost] -> printDigraph (pure . runIdentity) (standbyOp (Text.pack primaryHost))
        _ -> die "usage: salmon-postgres-replication-fixture ((dot (primary|standby))|primary <standby-cidr>|standby <primary-host>)"
