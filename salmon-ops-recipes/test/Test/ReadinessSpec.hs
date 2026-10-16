{-# LANGUAGE OverloadedStrings #-}

{- | Layer 0 coverage for "Salmon.Builtin.Nodes.Readiness": the wait node run
through 'Salmon.Actions.UpDown.upTree' with in-process probes (an @IO Bool@
that turns true after a while, never, or at once), the poll loop against a
fake clock, and the rendered probes. The only processes started are @true@
and @false@; no container, unit or VM.
-}
module Test.ReadinessSpec (tests) where

import Control.Exception (ErrorCall (..), fromException, throwIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (Report (..))
import Salmon.Builtin.Extension (Extension (..), Op, deps, op)
import Salmon.Builtin.Nodes.Readiness
import Salmon.Op.Actions (Act (..))
import qualified Salmon.Op.Actions as Actions
import Salmon.Op.OpGraph (OpGraph (..))
import Salmon.Op.Ref (mkRef)

import Test.Harness (runUpCapturing)

tests :: TestTree
tests =
    testGroup
        "Salmon.Builtin.Nodes.Readiness"
        [ testCase "a probe that turns true after 2s: the node is Done, its dependant runs" turnsTrue
        , testCase "a probe never true with a 1s deadline: Failed, its dependant Blocked" neverTrue
        , testCase "a probe already true: the node is skipped and up never runs" alreadyTrue
        , testCase "a probe that throws counts as no" throwingIsNo
        , testCase "pollUntil asks at the interval until the budget, against a fake clock" pollBudget
        , testCase "pollUntil asks once with no budget" pollOnce
        , testCase "a command probe is its exit code" commandProbe
        , testCase "rendered probes and node text" rendered
        ]

-------------------------------------------------------------------------------

-- | A dependant that records whether it ran.
dependantOf :: IORef Bool -> Op -> Op
dependantOf ran w =
    op "user" (deps [w]) $ \x -> x{ref = mkRef "test-user" (), up = atomicModifyIORef' ran (const (True, ()))}

counting :: IORef Int -> IO Bool -> IO Bool
counting n probe = atomicModifyIORef' n (\k -> (k + 1, ())) >> probe

nowMs :: IO Int
nowMs = (\ns -> fromIntegral (ns `div` 1000000)) <$> getMonotonicTimeNSec

shorthands :: (Act Extension -> Bool) -> [Report Extension] -> [Text]
shorthands _ [] = []
shorthands p (r : rs) = here <> shorthands p rs
  where
    here = case r of
        Done a | p a -> ["Done " <> a.shorthand]
        Skip a | p a -> ["Skip " <> a.shorthand]
        Failed a _ | p a -> ["Failed " <> a.shorthand]
        Blocked a | p a -> ["Blocked " <> a.shorthand]
        _ -> []

outcomes :: [Report Extension] -> [Text]
outcomes = shorthands (const True)

-------------------------------------------------------------------------------

turnsTrue :: IO ()
turnsTrue = do
    t0 <- nowMs
    ran <- newIORef False
    let probe = (\t -> t - t0 >= 2000) <$> nowMs
        w = waitUntil (untilIO "svc" "the test clock passed 2s" probe 5)
    reports <- runUpCapturing (dependantOf ran w)
    t1 <- nowMs
    assertEqual "outcomes" ["Done wait-until", "Done user"] (outcomes reports)
    assertBool "the dependant ran" =<< readIORef ran
    assertBool ("waited about 2s, not the deadline: " <> show (t1 - t0) <> "ms") (t1 - t0 >= 2000 && t1 - t0 < 4500)

neverTrue :: IO ()
neverTrue = do
    ran <- newIORef False
    let w = waitUntil (untilIO "svc" "never" (pure False) 1)
    reports <- runUpCapturing (dependantOf ran w)
    assertEqual "outcomes" ["Failed wait-until", "Blocked user"] (outcomes reports)
    assertEqual "dependant did not run" False =<< readIORef ran
    let thrown = [e | Failed _ e <- reports]
    assertBool "the failure is NotReadyInTime" (all (\e -> isJust (fromException e :: Maybe NotReadyInTime)) thrown && not (null thrown))

alreadyTrue :: IO ()
alreadyTrue = do
    ran <- newIORef False
    asked <- newIORef (0 :: Int)
    let w = waitUntil (untilIO "svc" "always" (counting asked (pure True)) 5)
    reports <- runUpCapturing (dependantOf ran w)
    assertEqual "outcomes" ["Skip wait-until", "Done user"] (outcomes reports)
    assertEqual "asked once, by check" 1 =<< readIORef asked

throwingIsNo :: IO ()
throwingIsNo = do
    ok <- askOnce 1000 (throwIO (ErrorCall "boom"))
    assertEqual "askOnce" False ok
    let w = waitUntil (untilIO "svc" "throws" (throwIO (ErrorCall "boom")) 0)
    reports <- runUpCapturing w
    assertEqual "outcomes" ["Failed wait-until"] (outcomes reports)

-- | Time moves only when the loop sleeps: attempts at 0, 250, 500, 750, 1000.
pollBudget :: IO ()
pollBudget = do
    clock <- newIORef (0 :: Int)
    asked <- newIORef []
    let fake =
            Clock
                { clockNow = readIORef clock
                , clockSleep = \ms -> atomicModifyIORef' clock (\t -> (t + ms, ()))
                }
        probe = do
            t <- readIORef clock
            atomicModifyIORef' asked (\ts -> (t : ts, ()))
            pure False
    ok <- pollUntil fake 1000 250 probe
    assertEqual "gave up" False ok
    assertEqual "asked at each interval" [0, 250, 500, 750, 1000] . reverse =<< readIORef asked
    -- and a yes stops it at once
    atomicModifyIORef' clock (const (0, ()))
    yes <- pollUntil fake 1000 250 ((>= 500) <$> readIORef clock)
    assertEqual "answered" True yes
    assertEqual "stopped at the first yes" 500 =<< readIORef clock

pollOnce :: IO ()
pollOnce = do
    asked <- newIORef (0 :: Int)
    ok <- pollUntil systemClock 0 250 (counting asked (pure False))
    assertEqual "gave up" False ok
    assertEqual "asked exactly once" 1 =<< readIORef asked

commandProbe :: IO ()
commandProbe = do
    assertEqual "true" True =<< runProbe (ProbeCommand "true" [])
    assertEqual "false" False =<< runProbe (ProbeCommand "false" [])
    assertEqual "missing binary" False =<< runProbe (ProbeCommand "salmon-no-such-binary-for-readiness" [])

rendered :: IO ()
rendered = do
    assertEqual
        "pg_isready"
        (ProbeCommand "pg_isready" ["--host", "127.0.0.1", "--port", "5432", "--quiet"])
        (pgIsReady "127.0.0.1" 5432)
    assertEqual
        "podman exec"
        (ProbeCommand "podman" ["exec", "db", "pg_isready", "--quiet"])
        (podmanExec "db" "pg_isready" ["--quiet"])
    -- the Quadlet readiness notes render these; moving the type must not change them
    assertEqual "describe tcp" "tcp 127.0.0.1:8080 accepts" (describeProbe (ProbeTcp "127.0.0.1" 8080))
    assertEqual
        "describe command"
        "`pg_isready --host 127.0.0.1 --port 5432 --quiet` exits 0"
        (describeProbe (pgIsReady "127.0.0.1" 5432))
    let w = waitUntil (untilProbe "db-pg" (pgIsReady "127.0.0.1" 5432) 60)
    case w.node of
        Actions.Actions act -> do
            assertEqual "shorthand" "wait-until" act.shorthand
            assertEqual
                "help"
                ("waits until `pg_isready --host 127.0.0.1 --port 5432 --quiet` exits 0 (up to 60s)" :: Text)
                act.extension.help
            assertEqual "ref is the key alone" (mkRef "wait-until" ("db-pg" :: Text)) act.extension.ref
            assertEqual
                "notes"
                [ "asks: `pg_isready --host 127.0.0.1 --port 5432 --quiet` exits 0"
                , "every 250ms, for up to 60s, then fails"
                , "check asks once: no answer is Unknown, not Failure"
                , "down does nothing"
                ]
                act.extension.notes
        _ -> fail "waitUntil built an actionless node"
