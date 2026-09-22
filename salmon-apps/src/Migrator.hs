{-# LANGUAGE OverloadedStrings #-}

module Migrator where

import qualified Data.Text as Text
import Options.Applicative (execParser, fullDesc, header, info, progDesc)
import Options.Generic (ParseRecord (..))

import qualified Salmon.Actions.Serve as Serve
import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension (Op, Track', deps, notes, op, ref)
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Postgres as Postgres

import Salmon.Op.Configure (Configure (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter

import Migrator.Ops
import Migrator.Seed
import Migrator.Spec

main :: IO ()
main = do
    let desc = fullDesc <> progDesc "Standalone db migration tool" <> header "for Postgres"
    let opts = info parseRecord desc
    cmd <- execParser opts
    -- the apt-get collection is a registered rewrite rather than an
    -- `Op -> Op` applied inside `program` below: a rewrite runs after the
    -- fold, so it sees every declaration `run serve` currently holds and
    -- which way each package node is wanted, and it can emit a removal batch
    -- as well as an install one. Neither is expressible in `Track' Spec`,
    -- which is a function of one directive alone.
    CLI.execCommandOrSeedWithRewrites
        Serve.reportText
        reportPrint
        [Debian.batchPackages reportPrint]
        configure
        program
        cmd

program :: Track' Spec
program =
    go 0
  where
    go n = Track $ \spec ->
        op "program" (deps $ specOp (n + 1) spec) $ \actions ->
            actions
                { notes = [Text.pack $ "at depth " <> show n]
                , ref = mkRef "program" n
                }

    specOp :: Int -> Spec -> [Op]
    -- meta
    specOp _ (Migrate setup1 setup2) = [migrate setup2 `inject` migrateSuperUser setup1]
    specOp _ (BuildTemplate setup1 setup2 fp) = [buildTemplate fp setup1 setup2]
    specOp _ (Clone retention c) = [cloneOp retention c]

configure :: Configure IO Seed Spec
configure = Configure go
  where
    go :: Seed -> IO Spec
    go (Seed mode root1 tip1 root2 tip2 dbname username passfile extrausers) = do
        setup1 <- prepare root1 tip1 dbname username extrausers passfile
        setup2 <- prepare root2 tip2 dbname username extrausers passfile
        case mode of
            InPlace -> pure $ Migrate setup1 setup2
            AsTemplate -> BuildTemplate setup1 setup2 <$> fingerprintInputs setup1 setup2
    go (CloneSeed dbname template owner retain) =
        pure $
            Clone
                (if retain then Postgres.Retain else Postgres.Discard)
                (Postgres.Clone dbname template owner)
