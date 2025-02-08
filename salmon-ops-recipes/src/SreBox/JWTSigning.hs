{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SreBox.JWTSigning where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Jose.Jwa as Jose
import qualified Jose.Jws as Jose

import Salmon.Actions.UpDown (skipIfFileExists)
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.Nodes.Secrets as Secrets
import Salmon.Op.OpGraph (OpGraph (..), inject)
import Salmon.Op.Ref (dotRef)
import Salmon.Op.Track (using)
import Salmon.Reporter

-------------------------------------------------------------------------------
data Report
    = Sign !Secrets.Secret FilePath
    deriving (Show)

signHmac :: Reporter Report -> Tracked' Secrets.Secret -> ByteString -> FilePath -> Op
signHmac r secret jwtPayload jwtPath =
    using secret $ \sekret ->
        op "jwt-signed-token" nodeps $ \actions ->
            actions
                { help = "derive a JWT for claims and and a given signing Key"
                , ref = dotRef $ "sign-jwt" <> Text.pack jwtPath
                , prelim = skipIfFileExists jwtPath
                , up = up sekret
                }
  where
    up sekret = do
        runReporter r (Sign sekret jwtPath)
        keytxt <- ByteString.readFile sekret.secret_path
        let key = case sekret.secret_type of
                Secrets.Base64 -> B64.decodeLenient keytxt
                Secrets.Hex -> keytxt
        print key
        let jwt = Jose.hmacEncode Jose.HS512 key jwtPayload
        for_ jwt $ \blob ->
            LByteString.writeFile jwtPath (Aeson.encode blob)
