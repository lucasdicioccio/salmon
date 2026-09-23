{-# LANGUAGE OverloadedStrings #-}

{- | @salmon-fleet@: the reader's side of the status sink (milestone 5 of
@specs/pull-mode.md@). @salmon-fleet status DIR@ folds every status
document in @DIR@ — one per host, written by each host's @run serve
--status-sink@ — into one line per host. It only ever reads; the fold
itself is "Salmon.Actions.Fleet", and this is the command line over it.
-}
module Fleet (main) where

import Control.Monad (forM_, unless)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Salmon.Actions.Fleet as Fleet

data Command
    = Status FilePath (Maybe String) Double Bool

main :: IO ()
main = do
    cmd <- execParser (info (commandP <**> helper) (fullDesc <> progDesc "Folds a directory of salmon status sink documents" <> header "salmon-fleet"))
    case cmd of
        Status dir label stale asJson -> do
            (docs, rejected) <- Fleet.readStatusDir dir
            forM_ rejected $ \(path, why) ->
                hPutStrLn stderr ("salmon-fleet: skipping " <> path <> ": " <> why)
            now <- getCurrentTime
            let opts = Fleet.Options (Text.pack <$> label) (realToFrac stale)
                rows = Fleet.fold opts now docs
            if asJson
                then LByteString.putStr (encode rows <> "\n")
                else do
                    Text.putStrLn Fleet.renderHeader
                    forM_ rows (Text.putStrLn . Fleet.renderRow)
            unless (null docs || not (null rows) || label == Nothing) $
                hPutStrLn stderr ("salmon-fleet: no host carries label " <> maybe "" id label)
            -- a directory with nothing readable in it is an error worth an
            -- exit code: the fold has nothing to say and probably was not
            -- pointed at the right place
            unless (not (null docs) || null rejected) exitFailure

commandP :: Parser Command
commandP =
    hsubparser $
        command "status" (info statusP (progDesc "One line per host from the status documents in DIR."))
  where
    statusP =
        Status
            <$> strArgument (metavar "DIR" <> help "A directory of *.json status sink documents (one per host).")
            <*> optional (strOption (long "label" <> metavar "LABEL" <> help "Only hosts whose applied documents include this label."))
            <*> option auto (long "stale" <> metavar "SECONDS" <> value 60 <> showDefault <> help "Flag a host whose document was written longer ago than this.")
            <*> switch (long "json" <> help "Emit the fold as one JSON array instead of lines.")
