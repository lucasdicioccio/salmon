{-# LANGUAGE OverloadedStrings #-}

{- | @salmon-fleet@: the controller's side of pull mode. @salmon-fleet
status DIR@ (milestone 5 of @specs/pull-mode.md@) folds every status
document in @DIR@ — one per host, written by each host's @run serve
--status-sink@ — into one line per host; the fold itself is
"Salmon.Actions.Fleet", and this is the command line over it. It reads
only, and writes nothing a host reads.

@salmon-fleet keygen --out FILE@ and @salmon-fleet sign --key FILE@ are the
signing half of "Salmon.Actions.Follow.Signature": a key pair as two JWK
files (@FILE@, mode 0600, and @FILE.pub@ for the hosts' @--follow-key@), and
a document on standard input wrapped in a signed envelope on standard
output — so the round trip from a document to a host that verifies it needs
no tool but this one. @sign@ writes to the filesystem only where @--out@
says; @keygen@ refuses to overwrite a key that exists.
-}
module Fleet (main) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime)
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Salmon.Actions.Fleet as Fleet
import qualified Salmon.Actions.Follow as Follow
import qualified Salmon.Actions.Follow.Signature as Signature

data Command
    = Status FilePath (Maybe String) Double Bool
    | Keygen FilePath
    | Sign FilePath (Maybe FilePath) (Maybe String)

main :: IO ()
main = do
    cmd <- execParser (info (commandP <**> helper) (fullDesc <> progDesc "The controller's side of pull mode: fold status sink documents, make signing keys, sign documents" <> header "salmon-fleet"))
    case cmd of
        Keygen path -> do
            taken <- doesFileExist path
            when taken $ do
                hPutStrLn stderr ("salmon-fleet: " <> path <> " exists; not overwriting a key")
                exitFailure
            key <- Signature.generateKeyPair
            Signature.writeKeyPair path key
            hPutStrLn stderr ("salmon-fleet: wrote " <> path <> " (private, 0600) and " <> path <> ".pub (public); key id " <> Text.unpack (Signature.keyId (Signature.publicKey key)))
        Sign keyPath out mlabel -> do
            loaded <- Signature.readPrivateKeyFile keyPath
            key <- case loaded of
                Left err -> hPutStrLn stderr ("salmon-fleet: --key " <> Text.unpack err) >> exitFailure
                Right k -> pure k
            document <- LByteString.getContents
            lbl <- case traverse (Follow.mkLabel . Text.pack) mlabel of
                Left err -> hPutStrLn stderr ("salmon-fleet: --label: " <> Text.unpack err) >> exitFailure
                Right l -> pure l
            signed <- Signature.signDocumentFor key lbl document
            case signed of
                Left err -> hPutStrLn stderr ("salmon-fleet: cannot sign: " <> Text.unpack err) >> exitFailure
                Right envelope -> maybe LByteString.putStr LByteString.writeFile out envelope
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
            <> command "keygen" (info keygenP (progDesc "Write a fresh Ed25519 signing key pair: FILE (private, 0600) and FILE.pub (public, for --follow-key)."))
            <> command "sign" (info signP (progDesc "Wrap the document on standard input in a signed envelope, on standard output (or --out FILE)."))
  where
    keygenP =
        Keygen
            <$> strOption (long "out" <> metavar "FILE" <> help "Where to write the private key; the public key goes to FILE.pub.")
    signP =
        Sign
            <$> strOption (long "key" <> metavar "FILE" <> help "The private key (JWK) to sign with, as `keygen --out FILE` wrote it.")
            <*> optional (strOption (long "out" <> metavar "FILE" <> help "Write the signed envelope here instead of standard output."))
            <*> optional (strOption (long "label" <> metavar "LABEL" <> help "The label this document is for; put into the signed document so a host following another label refuses it. Hosts run with --follow-key refuse a signed document that names none, unless --follow-accept-unlabelled."))
    statusP =
        Status
            <$> strArgument (metavar "DIR" <> help "A directory of *.json status sink documents (one per host).")
            <*> optional (strOption (long "label" <> metavar "LABEL" <> help "Only hosts whose applied documents include this label."))
            <*> option auto (long "stale" <> metavar "SECONDS" <> value 60 <> showDefault <> help "Flag a host whose document was written longer ago than this.")
            <*> switch (long "json" <> help "Emit the fold as one JSON array instead of lines.")
