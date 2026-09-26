{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Maintenance windows: a pure value saying when disruptive nodes may run,
and the 'Gate' that holds them back the rest of the time.

A node opts in with 'disruptive' (a marker on @dynamics@, the channel
"Salmon.Op.Supervision" uses for the same reason), so nothing changes for the
many nodes with no opinion. The window itself belongs to whoever runs the
graph, not to the node: the node knows it is a restart, only the operator
knows when a restart is welcome. @run up --maintenance-window SPEC@ supplies
it; @--override-window@ is the operator who means it.

A node held by the gate is reported 'Skippable' like any gated node, and
stays wanted: the gate says "not now", nothing is frozen, so the next pass
inside the window applies it. Dependants of a skipped node are not blocked by
that (a 'Skip' is not a failure); order, not success, is what edges carry.

Time zones: a window carries a fixed UTC offset, because salmon-ops has no
time-zone database. A zone with daylight saving needs its offsets spelled
twice, as two windows.
-}
module Salmon.Op.Window (
    Window (..),
    parseWindow,
    renderWindow,
    inWindow,
    inAnyWindow,
    nextOpening,

    -- * Nodes opting in
    Disruptive (..),
    disruptive,
    isDisruptive,

    -- * The gate
    windowGate,
    windowGateAt,
) where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Char (isAlpha)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (
    DayOfWeek (..),
    UTCTime (..),
    addDays,
    addUTCTime,
    dayOfWeek,
    getCurrentTime,
    secondsToDiffTime,
 )
import GHC.Records (HasField, getField)
import Text.Read (readMaybe)

import Salmon.Actions.UpDown (Gate, Requirement (..))
import Salmon.Op.Actions (Act (..))
import Salmon.Builtin.Extension (Extension)
import qualified Salmon.Builtin.Extension as Extension

{- | One recurring span. Minutes are counted from local midnight; a span
whose start is later than its end crosses midnight (a weekly one then ends
on the following day).
-}
data Window = Window
    { winDay :: Maybe DayOfWeek
    -- ^ 'Nothing' is every day.
    , winStart :: Int
    , winEnd :: Int
    , winOffset :: Int
    -- ^ minutes east of UTC.
    }
    deriving (Show, Eq, Ord)

dayNames :: [(Text, DayOfWeek)]
dayNames =
    [ ("Mon", Monday), ("Tue", Tuesday), ("Wed", Wednesday), ("Thu", Thursday)
    , ("Fri", Friday), ("Sat", Saturday), ("Sun", Sunday)
    ]

{- | @HH:MM-HH:MM@ or @Day:HH:MM-HH:MM@, optionally followed by
@\@UTC@ or @\@+HH:MM@ / @\@-HH:MM@ (UTC when absent). Start and end must
differ.
-}
parseWindow :: Text -> Either Text Window
parseWindow spec = do
    let (body, zone) = Text.breakOn "@" spec
    off <- if Text.null zone then Right 0 else parseOffset (Text.drop 1 zone)
    let (dayTxt, afterDay) = Text.breakOn ":" body
    (day, times) <-
        if not (Text.null dayTxt) && Text.all isAlpha dayTxt
            then case lookup dayTxt dayNames of
                Just wd -> Right (Just wd, Text.drop 1 afterDay)
                Nothing -> Left ("unknown day in window " <> quoted)
            else Right (Nothing, body)
    (s, e) <- case Text.splitOn "-" times of
        [a, b] -> (,) <$> parseHm a <*> parseHm b
        _ -> Left ("malformed window " <> quoted)
    if s == e then Left ("empty window " <> quoted) else Right (Window day s e off)
  where
    quoted = "\"" <> spec <> "\""
    parseHm t = case Text.splitOn ":" t of
        [h, m]
            | Just h' <- readMaybe (Text.unpack h)
            , Just m' <- readMaybe (Text.unpack m)
            , Text.length h <= 2
            , Text.length m == 2
            , h' >= 0
            , h' < (24 :: Int)
            , m' >= 0
            , m' < (60 :: Int) ->
                Right (h' * 60 + m')
        _ -> Left ("malformed time \"" <> t <> "\" in window " <> quoted)
    parseOffset z
        | z `elem` ["UTC", "Z"] = Right 0
        | Just (sign, rest) <- Text.uncons z
        , sign `elem` ("+-" :: String)
        , [h, m] <- Text.splitOn ":" rest
        , Just h' <- readMaybe (Text.unpack h)
        , Just m' <- readMaybe (Text.unpack m)
        , h' <= (14 :: Int)
        , m' < (60 :: Int) =
            Right ((if sign == '-' then negate else id) (h' * 60 + m'))
        | otherwise = Left ("malformed zone \"" <> z <> "\" in window " <> quoted)

renderWindow :: Window -> Text
renderWindow w =
    maybe "" (\d -> maybe "" id (lookup d [(b, a) | (a, b) <- dayNames]) <> ":") w.winDay
        <> hm w.winStart
        <> "-"
        <> hm w.winEnd
        <> zone
  where
    hm n = pad (n `div` 60) <> ":" <> pad (n `mod` 60)
    pad n = Text.justifyRight 2 '0' (Text.pack (show n))
    zone
        | w.winOffset == 0 = "@UTC"
        | otherwise =
            "@" <> (if w.winOffset < 0 then "-" else "+")
                <> hm (abs w.winOffset)

-- | Local (day of week, minute of day) at an instant.
localParts :: Int -> UTCTime -> (DayOfWeek, Int)
localParts off t = (dayOfWeek (utctDay l), floor (utctDayTime l) `div` 60)
  where
    l = addUTCTime (fromIntegral off * 60) t

inWindow :: Window -> UTCTime -> Bool
inWindow w t
    | w.winStart < w.winEnd = dayOk dow && m >= w.winStart && m < w.winEnd
    | otherwise =
        (dayOk dow && m >= w.winStart) || (dayOk (pred' dow) && m < w.winEnd)
  where
    (dow, m) = localParts w.winOffset t
    dayOk d = maybe True (== d) w.winDay
    pred' d = toEnum ((fromEnum d + 5) `mod` 7 + 1)

inAnyWindow :: [Window] -> UTCTime -> Bool
inAnyWindow ws t = any (`inWindow` t) ws

{- | When the next window opens after an instant that is inside none of them.
'Nothing' when one is open now, or there are no windows.
-}
nextOpening :: [Window] -> UTCTime -> Maybe UTCTime
nextOpening ws t
    | inAnyWindow ws t = Nothing
    | null candidates = Nothing
    | otherwise = Just (minimum candidates)
  where
    candidates = concatMap starts ws
    starts w =
        [ i
        | k <- [0 .. 8]
        , let d = addDays k (utctDay (addUTCTime (fromIntegral w.winOffset * 60) t))
        , maybe True (== dayOfWeek d) w.winDay
        , let i =
                addUTCTime (negate (fromIntegral w.winOffset * 60)) $
                    UTCTime d (secondsToDiffTime (fromIntegral w.winStart * 60))
        , i > t
        ]

-- | Marker: this node restarts, upgrades or otherwise disturbs something.
data Disruptive = Disruptive
    deriving (Show, Eq)

-- | Mark a node as one a maintenance window applies to.
disruptive :: Extension -> Extension
disruptive e = e{Extension.dynamics = toDyn Disruptive : Extension.dynamics e}

isDisruptive :: (HasField "dynamics" ext [Dynamic]) => ext -> Bool
isDisruptive ext = any (isJust . (fromDynamic :: Dynamic -> Maybe Disruptive)) (getField @"dynamics" ext)

{- | A 'Gate' holding every 'disruptive' node while the clock is outside all
the windows. No windows means no gate. The callback is told which node is
held and when the next window opens, for the caller to report.
-}
windowGate :: [Window] -> (Act Extension -> Maybe UTCTime -> IO ()) -> Gate Extension
windowGate = windowGateAt getCurrentTime

-- | 'windowGate' over a caller's clock, so a test can move time.
windowGateAt :: IO UTCTime -> [Window] -> (Act Extension -> Maybe UTCTime -> IO ()) -> Gate Extension
windowGateAt _ [] _ = const (pure Required)
windowGateAt clock ws onHeld = \act -> gate act
  where
    gate :: Act Extension -> IO Requirement
    gate act
        | not (isDisruptive act.extension) = pure Required
        | otherwise = do
            now <- clock
            if inAnyWindow ws now
                then pure Required
                else onHeld act (nextOpening ws now) >> pure Skippable

