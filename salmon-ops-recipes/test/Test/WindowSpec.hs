{-# LANGUAGE OverloadedStrings #-}

-- | Layer 0 coverage for "Salmon.Op.Window": membership (midnight, weekly,
-- offsets), the next opening, parsing, and the gate holding a disruptive
-- node until an injected clock passes into the window.
module Test.WindowSpec (tests) where

import Data.Functor.Identity (runIdentity)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Salmon.Actions.UpDown (upTreeWith)
import Salmon.Builtin.Extension (Extension (..), nodeps, op)
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Window

import Test.Harness (capture)

-- 2026-09-26 is a Saturday.
at :: String -> UTCTime
at = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M"

win :: Text -> Window
win t = either (error . show) id (parseWindow t)

tests :: TestTree
tests =
    testGroup
        "Salmon.Op.Window"
        [ testCase "daily window: start inclusive, end exclusive" $ do
            let w = win "02:00-04:00"
            assertBool "in" (inWindow w (at "2026-09-26 02:00"))
            assertBool "in" (inWindow w (at "2026-09-26 03:59"))
            assertBool "out at end" (not (inWindow w (at "2026-09-26 04:00")))
            assertBool "out before" (not (inWindow w (at "2026-09-26 01:59")))
        , testCase "a daily window crosses midnight" $ do
            let w = win "23:00-01:00"
            assertBool "late" (inWindow w (at "2026-09-26 23:30"))
            assertBool "early" (inWindow w (at "2026-09-27 00:30"))
            assertBool "out" (not (inWindow w (at "2026-09-27 01:00")))
            assertBool "out midday" (not (inWindow w (at "2026-09-27 12:00")))
        , testCase "a weekly window applies to its day only" $ do
            let w = win "Sat:02:00-04:00"
            assertBool "Sat" (inWindow w (at "2026-09-26 03:00"))
            assertBool "Sun" (not (inWindow w (at "2026-09-27 03:00")))
        , testCase "a weekly window crossing midnight ends on the next day" $ do
            let w = win "Sat:23:00-01:00"
            assertBool "Sat night" (inWindow w (at "2026-09-26 23:30"))
            assertBool "Sun early" (inWindow w (at "2026-09-27 00:30"))
            assertBool "Sat early is not" (not (inWindow w (at "2026-09-26 00:30")))
            assertBool "Mon early is not" (not (inWindow w (at "2026-09-28 00:30")))
        , testCase "a Sunday window crossing midnight ends on Monday" $ do
            let w = win "Sun:23:00-01:00"
            assertBool "Mon early" (inWindow w (at "2026-09-28 00:30"))
        , testCase "the offset moves the window (and its weekday) against UTC" $ do
            let w = win "Sun:01:00-03:00@+02:00" -- Sat 23:00-01:00 UTC
            assertBool "in" (inWindow w (at "2026-09-26 23:30"))
            assertBool "out" (not (inWindow w (at "2026-09-27 01:00")))
            let n = win "Sat:22:00-23:00@-05:00" -- Sun 03:00-04:00 UTC
            assertBool "neg" (inWindow n (at "2026-09-27 03:30"))
        , testCase "several windows: any one suffices" $
            assertBool "second" (inAnyWindow [win "02:00-03:00", win "12:00-13:00"] (at "2026-09-26 12:30"))
        , testCase "nextOpening" $ do
            let ws = [win "Sun:02:00-04:00"]
            assertEqual "from Sat" (Just (at "2026-09-27 02:00")) (nextOpening ws (at "2026-09-26 10:00"))
            assertEqual "open now" Nothing (nextOpening ws (at "2026-09-27 03:00"))
            assertEqual "just after" (Just (at "2026-10-04 02:00")) (nextOpening ws (at "2026-09-27 04:00"))
            assertEqual "with offset" (Just (at "2026-09-27 00:00")) (nextOpening [win "Sun:02:00-04:00@+02:00"] (at "2026-09-26 10:00"))
            assertEqual "none" Nothing (nextOpening [] (at "2026-09-26 10:00"))
        , testCase "parsing refuses what it cannot mean" $ do
            mapM_
                (\t -> assertBool (show t) (either (const True) (const False) (parseWindow t)))
                ["", "02:00", "02:00-02:00", "Xyz:02:00-03:00", "25:00-03:00", "02:00-03:60", "02:00-03:00@Mars", "2:0-3:0"]
            assertEqual "roundtrip" "Sun:02:00-04:00@+02:00" (renderWindow (win "Sun:02:00-04:00@+02:00"))
        , testCase "a held node is not applied, then is once the clock enters the window" gateHolds
        ]

gateHolds :: IO ()
gateHolds = do
    applied <- newIORef (0 :: Int)
    plainApplied <- newIORef (0 :: Int)
    clock <- newIORef (at "2026-09-26 10:00")
    heldLog <- newIORef ([] :: [Maybe UTCTime])
    let restart = op "restart" nodeps $ \x ->
            disruptive x{ref = mkRef "restart" ("r" :: Text), up = modifyIORef' applied (+ 1)}
        plain = op "plain" nodeps $ \x ->
            x{ref = mkRef "plain" ("p" :: Text), up = modifyIORef' plainApplied (+ 1)}
        gate = windowGateAt (readIORef clock) [win "02:00-04:00"] (\_ next -> modifyIORef' heldLog (next :))
        pass o = do
            (r, _) <- capture
            ok <- upTreeWith gate r (pure . runIdentity) o
            assertBool "pass succeeds" ok
    pass restart
    pass plain
    assertEqual "held outside the window" 0 =<< readIORef applied
    assertEqual "a node with no opinion is untouched" 1 =<< readIORef plainApplied
    assertEqual "held, told when it opens" [Just (at "2026-09-27 02:00")] =<< readIORef heldLog
    writeIORef clock (at "2026-09-27 02:30")
    pass restart
    assertEqual "applied inside the window" 1 =<< readIORef applied
