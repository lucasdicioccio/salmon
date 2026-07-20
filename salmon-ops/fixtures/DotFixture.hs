{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | A fixture graph exercising every edge-color branch of
'Salmon.Actions.Dot' (plain\/OV, red\/CL, orange\/CR+Connect, gray\/CR+Overlay),
an Actionless node (dot-render skip-through), and a dependency shared by two
parents (to exercise 'Salmon.Actions.UpDown''s dedup-by-Ref path).

This repo has no automated test suite (see CLAUDE.md) — this binary isn't
one either. It's a fixture to run and eyeball\/diff by hand, e.g. before and
after touching graph-traversal or dot-rendering code:

> salmon-ops-dot-fixture dot > before.dot   # on a known-good commit
> salmon-ops-dot-fixture dot > after.dot    # after your change
> diff before.dot after.dot                 # expect no diff

> salmon-ops-dot-fixture up                 # eyeball the Eval\/Skip\/Redundant report
-}
module Main (main) where

import Control.Monad.Identity (runIdentity)
import qualified Data.Text as Text
import System.Environment (getArgs)
import System.Exit (die)

import Salmon.Actions.Dot (printDigraph)
import Salmon.Actions.UpDown (upTree)
import Salmon.Builtin.Extension
import Salmon.Op.Graph
import Salmon.Op.OpGraph (OpGraph (..), inject, overlaid)
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter (reportPrint)

leaf :: Text.Text -> Op
leaf name =
    op name nodeps $ \x ->
        x{ref = mkRef "leaf" name, up = putStrLn ("up " <> Text.unpack name)}

-- | Depended on by both 'childV' and 'childO', to exercise upTree's
-- dedup-by-Ref path (one Eval, one Redundant).
commonLeaf :: Op
commonLeaf = leaf "common-leaf"

leafCL :: Op
leafCL = leaf "leaf-cl"

-- | Own predecessors are 'Vertices' (shape V): the edge into this node from
-- 'root' is tagged (CR,V) -> plain.
childV :: Op
childV =
    op "child-v" (deps [commonLeaf]) $ \x ->
        x{ref = mkRef "child" ("child-v" :: Text.Text), up = putStrLn "up child-v"}

-- | Own predecessors are 'Overlay' (shape O): the edge into this node from
-- 'root' is tagged (CR,O) -> gray. Also overlays an 'Actionless' node
-- ('realNoop'), to exercise dot's skip-through-Actionless path.
childO :: Op
childO =
    ( op "child-o" nodeps $ \x ->
        x{ref = mkRef "child" ("child-o" :: Text.Text), up = putStrLn "up child-o"}
    )
        `overlaid` commonLeaf
        `overlaid` realNoop

-- | Own predecessors are 'Connect' (shape C, via 'inject'): the edge into
-- this node from 'root' is tagged (CR,C) -> orange. Its own inject produces
-- a further CL\/red edge one level down.
childC :: Op
childC =
    ( op "child-c" nodeps $ \x ->
        x{ref = mkRef "child" ("child-c" :: Text.Text), up = putStrLn "up child-c"}
    )
        `inject` leaf "leaf-c-inner"

-- | 'root''s predecessors are hand-built as @Connect (Vertices ..) (Vertices
-- ..)@ so that 'leafCL' is reached via the Connect's left arm (CL -> red),
-- while 'childV'\/'childO'\/'childC' are reached via the right arm (CR),
-- each then getting a different edge color from its own predecessor shape.
root :: Op
root =
    (op "root" nodeps $ \x -> x{ref = mkRef "root" (), up = putStrLn "up root"})
        { predecessors = pure (Connect (Vertices [leafCL]) (Vertices [childV, childO, childC]))
        }

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["dot"] -> printDigraph (pure . runIdentity) root
        ["up"] -> upTree reportPrint (pure . runIdentity) root
        _ -> die "usage: salmon-ops-dot-fixture (dot|up)"
