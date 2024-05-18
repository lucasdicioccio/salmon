{-# LANGUAGE OverloadedStrings #-}

module Migrator where

import qualified Data.Text as Text
import Options.Applicative (execParser, fullDesc, header, info, progDesc)
import Options.Generic (ParseRecord (..))

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension (Op, Track', deps, notes, op, ref)
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian

import Salmon.Op.Configure (Configure (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (dotRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter

import Migrator.Ops
import Migrator.Spec
import Migrator.Seed

main :: IO ()
main = do
    let desc = fullDesc <> progDesc "Standalone db migration tool" <> header "for Postgres"
    let opts = info parseRecord desc
    cmd <- execParser opts
    CLI.execCommandOrSeed reportPrint configure program cmd

program :: Track' Spec
program =
    go 0
  where
    go n = Track $ \spec ->
        optimizedDeps $ op "program" (deps $ specOp (n + 1) spec) $ \actions ->
            actions
                { notes = [Text.pack $ "at depth " <> show n]
                , ref = dotRef $ Text.pack $ "program:" <> show n
                }

    specOp :: Int -> Spec -> [Op]
    -- meta
    specOp _ (Migrate setup1 setup2) = [migrate setup2 `inject` migrateSuperUser setup1]

    optimizedDeps :: Op -> Op
    optimizedDeps base =
        let pkgs = Debian.installAllDebsAtOnce base
         in Debian.removeSinglePackages base `inject` pkgs

configure :: Configure IO Seed Spec
configure = Configure go
  where
    go :: Seed -> IO Spec
    go (Seed root1 tip1 root2 tip2 dbname username passfile) =
        Migrate
            <$> prepare root1 tip1 dbname username passfile
            <*> prepare root2 tip2 dbname username passfile


