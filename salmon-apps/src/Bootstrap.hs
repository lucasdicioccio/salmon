{-# LANGUAGE OverloadedStrings #-}

module Bootstrap where

import qualified Data.Text as Text
import Options.Applicative (execParser, fullDesc, info, progDesc)
import Options.Generic (ParseRecord (..))

import qualified Salmon.Builtin.CommandLine as CLI
import Salmon.Builtin.Extension (Op, Track', deps, notes, op, ref)
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Self as Self

import Salmon.Op.Configure (Configure (..))
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Ref (dotRef)
import Salmon.Op.Track (Track (..))
import Salmon.Reporter

import Bootstrap.Ops
import Bootstrap.Seed
import Bootstrap.Spec

main :: IO ()
main = do
    let desc = fullDesc <> progDesc "Standalone salmon boot-strapping tool"
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
    specOp _ (Bootstrap self image) = [bootstrapUbuntu self image]

    optimizedDeps :: Op -> Op
    optimizedDeps base =
        let pkgs = Debian.installAllDebsAtOnce base
         in Debian.removeSinglePackages base `inject` pkgs

configure :: Configure IO Seed Spec
configure = Configure go
  where
    go :: Seed -> IO Spec
    go (Seed image) = do
        self <- Self.readSelfPath_linux
        pure $ Bootstrap self (Text.pack image)
