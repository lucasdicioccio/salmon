module Salmon.Builtin.Nodes.Secrets where

import Salmon.Actions.UpDown (skipIfFileExists)
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Builtin.Nodes.Filesystem as FS
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Control.Monad (void)
import qualified Data.ByteString.Char8 as ByteString
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath (takeDirectory, (</>))
import System.Process.ListLike (CreateProcess, proc)

-------------------------------------------------------------------------------
data Report
    = Generate !Secret !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

data SecretType
    = Base64
    | Base64SafeUrl
    | Hex
    deriving (Show)

data Secret
    = Secret
    { secret_type :: SecretType
    , secret_bytes :: Int
    , secret_path :: FilePath
    }
    deriving (Show)

sharedSecretFile :: Reporter Report -> Track' (Binary "openssl") -> Secret -> Op
sharedSecretFile r bin sec =
    withBinary bin openssl (GenRandom sec) $ \up -> do
        op "secret:gen" (deps [enclosingdir]) $ \actions ->
            actions
                { help = "generates a secret file for shared-secret"
                , ref = dotRef $ "gen-secret" <> Text.pack sec.secret_path
                , prelim = skipIfFileExists sec.secret_path
                , up = up r' >> modifyInPlace sec.secret_path
                }
  where
    r' = contramap (Generate sec) r
    enclosingdir :: Op
    enclosingdir = FS.dir (FS.Directory $ takeDirectory sec.secret_path)
    modifyInPlace path =
        case sec.secret_type of
            Base64SafeUrl -> chompNewLines path >> safeUrlizeB64 path
            Base64 -> chompNewLines path
            Hex -> chompNewLines path

data GenRandom
    = GenRandom Secret

openssl :: Command "openssl" GenRandom
openssl = Command $ \(GenRandom s) ->
    case s.secret_type of
        Hex -> proc "openssl" ["rand", "-hex", "-out", s.secret_path, show s.secret_bytes]
        Base64 -> proc "openssl" ["rand", "-base64", "-out", s.secret_path, show s.secret_bytes]
        Base64SafeUrl -> proc "openssl" ["rand", "-base64", "-out", s.secret_path, show s.secret_bytes]

chompNewLines :: FilePath -> IO ()
chompNewLines path =
    ByteString.readFile path >>= ByteString.writeFile path . chomp
  where
    chomp = ByteString.filter ((/=) '\n')

safeUrlizeB64 :: FilePath -> IO ()
safeUrlizeB64 path =
    ByteString.readFile path >>= ByteString.writeFile path . tr
  where
    tr = ByteString.map f
    f '+' = '-'
    f '/' = '_'
    f x = x
