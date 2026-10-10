module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.PgTurretSpec as PgTurretSpec

main :: IO ()
main = defaultMain (testGroup "salmon-ops-recipes-experimental" [PgTurretSpec.tests])
