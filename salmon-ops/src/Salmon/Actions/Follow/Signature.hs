{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Signed documents for pull mode: the verifier that fills in
'Salmon.Actions.Follow.followVerify', and the signer a controller runs
(@salmon-fleet sign@).

Pulling inverts trust — a host following a registry trusts whatever the
registry serves — so a document can be wrapped in a /signed envelope/:

@
{ "salmon-signed": 1
, "document": { "salmon": 1, "id": "web\@2026-09-24", "seeds": [...] }
, "signatures": [ { "key": "\<key id\>", "alg": "EdDSA", "sig": "\<base64\>" } ]
}
@

The document rides inside it as fetched, an object, not a string; the
signature is over its /canonical bytes/ — 'canonicalBytes', which is
"Data.Aeson"'s 'encode' of the parsed 'Value'. That encoder writes an
object's keys in sorted order (aeson 2's 'Data.Aeson.KeyMap' is a
@Data.Map@ under its default @ordered-keymap@ flag; the plan pins aeson
2.2.5.1, and the ordering has held since 2.0) and one spelling per string
and number, so a registry, a proxy or a pretty-printer that re-serialises
the envelope — reorders keys, changes whitespace — does not break the
signature; only a change to the document's /content/ does. Both the signer
and the verifier parse first and encode with the same function, which is
the whole of the canonicalisation story: no separate canonical-JSON
library, and nothing to keep in step with one.

__What the verifier hands the loop is the inner document__, canonical
bytes, so that what is parsed as a 'Salmon.Actions.Follow.Document' is
exactly what was signed. The digest the fetcher keeps — in @history@, in
'Salmon.Actions.Follow.Rejected', and beside the cached bytes — stays the
digest of the bytes /as fetched/, i.e. of the envelope: change detection
compares fetched bytes, the DNS index's @sha256=@ names them, and a cache
entry is verified from those same bytes on replay, so the envelope is what
the cache keeps and the digest is what identifies it.

__The key is a JWK__ ("Crypto.JOSE.JWK", the @jose@ package
"Salmon.Builtin.Nodes.Keys" already generates JWK files with): the one key
format this tree already has, kept rather than adding a PEM story beside
it. @salmon-fleet keygen@ writes an Ed25519 pair as two JWK files; the
algorithm is EdDSA over Ed25519 — @jose@ carries it on @crypton@, which
was already a dependency — with the RSA and EC algorithms @jose@ also
signs with accepted for a key that is one of those (the signer picks
'JWK.bestJWSAlg'). @none@ and the HMAC algorithms are refused outright: a
public key can verify neither. The key id is the RFC 7638 thumbprint, the
SHA-256 of the public key's canonical JSON, in hex.

Verification is per 'signedVerifier': every signature is tried against
every key it names, any one that verifies accepts. What is refused, and
with what reason: a plain unsigned document (there is a key, so it is
required), an envelope that does not parse, one with no signatures, and
one whose signatures all fail — the reason names which.
-}
module Salmon.Actions.Follow.Signature (
    -- * Keys
    PrivateKey,
    PublicKey,
    generateKeyPair,
    publicKey,
    keyId,
    readPrivateKeyFile,
    readPublicKeyFile,
    writeKeyPair,

    -- * The envelope
    envelopeVersion,
    canonicalBytes,
    signDocument,
    Envelope (..),
    Signature (..),

    -- * The verifier
    signedVerifier,
    verifyEnvelope,
) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecode, encode, object, withObject, (.:), (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.Either (lefts, rights)
import Data.Functor.Const (Const (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Posix.Files (setFileMode)

import qualified Crypto.JOSE.Error as JOSE
import qualified Crypto.JOSE.JWA.JWS as JWS
import Crypto.JOSE.JWK (JWK)
import qualified Crypto.JOSE.JWK as JWK

import Salmon.Actions.Follow (Digest, Verifier)

-------------------------------------------------------------------------------
-- keys

-- | A key with private material: what signs.
newtype PrivateKey = PrivateKey JWK
    deriving (Show, Eq)

-- | A key without private material: what a host verifies against.
newtype PublicKey = PublicKey JWK
    deriving (Show, Eq)

-- | Read a getter or lens without a @lens@ dependency: 'Const' is both
-- the functor a lens needs and the contravariant one a getter needs.
view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view l = getConst . l Const

-- | A fresh Ed25519 pair, as one JWK holding both halves.
generateKeyPair :: IO PrivateKey
generateKeyPair = PrivateKey <$> JWK.genJWK (JWK.OKPGenParam JWK.Ed25519)

-- | The public half of a key. Every key material @jose@ generates has one.
publicKey :: PrivateKey -> PublicKey
publicKey (PrivateKey k) =
    PublicKey (maybe k id (view JWK.asPublicKey k))

-- | The RFC 7638 thumbprint of the public key, SHA-256, hex: what an
-- envelope's @key@ member names.
keyId :: PublicKey -> Text
keyId (PublicKey k) = Text.pack (show (view JWK.thumbprint k :: JWK.Digest JWK.SHA256))

-- | A JWK file holding private material: the reason it cannot be used, or
-- the key. Never throws.
readPrivateKeyFile :: FilePath -> IO (Either Text PrivateKey)
readPrivateKeyFile path = do
    parsed <- readKeyFile path
    pure $ case parsed of
        Left err -> Left err
        Right k
            | view JWK.asPublicKey k == Just k -> Left (Text.pack path <> ": holds no private material")
            | otherwise -> Right (PrivateKey k)

-- | A JWK file holding a public key — or a private one, whose public half
-- is taken. Never throws.
readPublicKeyFile :: FilePath -> IO (Either Text PublicKey)
readPublicKeyFile path = do
    parsed <- readKeyFile path
    pure $ case parsed of
        Left err -> Left err
        Right k -> case view JWK.asPublicKey k of
            Nothing -> Left (Text.pack path <> ": not a key with a public half")
            Just pk -> Right (PublicKey pk)

readKeyFile :: FilePath -> IO (Either Text JWK)
readKeyFile path = do
    attempt <- try (LByteString.readFile path >>= \b -> LByteString.length b `seq` pure b)
    pure $ case attempt of
        Left (ex :: SomeException) -> Left (Text.pack path <> ": " <> Text.pack (show ex))
        Right bytes -> case eitherDecode bytes of
            Left err -> Left (Text.pack path <> ": not a JWK: " <> Text.pack err)
            Right k -> Right k

{- | Write a pair as two JWK files: the private key at @path@, mode 0600,
and its public half at @path.pub@. May throw. -}
writeKeyPair :: FilePath -> PrivateKey -> IO ()
writeKeyPair path key@(PrivateKey k) = do
    LByteString.writeFile path (encode k <> "\n")
    setFileMode path 0o600
    let PublicKey pk = publicKey key
    LByteString.writeFile (path <> ".pub") (encode pk <> "\n")

-------------------------------------------------------------------------------
-- the envelope

-- | The one value of @salmon-signed@ this reader understands.
envelopeVersion :: Int
envelopeVersion = 1

-- | The bytes a signature is over: 'encode' of the parsed value. See the
-- module comment for why that is canonical enough.
canonicalBytes :: Value -> ByteString
canonicalBytes = encode

data Signature = Signature
    { sigKey :: !Text
    -- ^ the signing key's 'keyId'
    , sigAlg :: !JWS.Alg
    , sigBytes :: !ByteString.ByteString
    -- ^ raw; base64 on the wire
    }
    deriving (Show, Eq)

instance FromJSON Signature where
    parseJSON = withObject "salmon signature" $ \o -> do
        k <- o .: "key"
        alg <- o .: "alg"
        b64 <- o .: "sig"
        case Base64.decode (Text.encodeUtf8 b64) of
            Left err -> fail ("sig is not base64: " <> err)
            Right raw -> pure (Signature k alg raw)

instance ToJSON Signature where
    toJSON s =
        object
            [ "key" .= s.sigKey
            , "alg" .= s.sigAlg
            , "sig" .= Text.decodeUtf8 (Base64.encode s.sigBytes)
            ]

-- | The wire shape: the document as a 'Value', signatures beside it.
data Envelope = Envelope
    { envDocument :: !Value
    , envSignatures :: ![Signature]
    }
    deriving (Show, Eq)

instance FromJSON Envelope where
    parseJSON = withObject "salmon signed envelope" $ \o -> do
        v <- o .: "salmon-signed"
        unless (v == envelopeVersion) $
            fail ("unsupported envelope format: salmon-signed=" <> show v <> " (this reader understands " <> show envelopeVersion <> ")")
        Envelope <$> o .: "document" <*> o .: "signatures"

instance ToJSON Envelope where
    toJSON e =
        object
            [ "salmon-signed" .= envelopeVersion
            , "document" .= e.envDocument
            , "signatures" .= e.envSignatures
            ]

{- | Wrap a document's bytes in a signed envelope: the reason they cannot be
signed (not JSON, not an object, a key that signs nothing), or the
envelope's bytes. The document is kept as parsed, so a publisher's
annotations survive; what is signed is its canonical form. -}
signDocument :: PrivateKey -> ByteString -> IO (Either Text ByteString)
signDocument key@(PrivateKey k) bytes =
    case eitherDecode bytes of
        Left err -> pure (Left ("the document is not JSON: " <> Text.pack err))
        Right doc@(Object _) -> do
            outcome <- JOSE.runJOSE $ do
                alg <- JWK.bestJWSAlg k
                sig <- JWK.sign alg (view JWK.jwkMaterial k) (LByteString.toStrict (canonicalBytes doc))
                pure (alg, sig)
            pure $ case outcome of
                Left (err :: JOSE.Error) -> Left ("cannot sign with this key: " <> Text.pack (show err))
                Right (alg, sig) ->
                    Right (encode (Envelope doc [Signature (keyId (publicKey key)) alg sig]) <> "\n")
        Right _ -> pure (Left "the document is not a JSON object")

-------------------------------------------------------------------------------
-- the verifier

{- | Refuse everything but an envelope one of these keys signed, and hand
the loop the document inside it. 'Right' is the inner document's canonical
bytes; the digest handed in is only for the reasons' sake. -}
signedVerifier :: [PublicKey] -> Verifier
signedVerifier keys _ bytes = pure (verifyEnvelope keys bytes)

-- | 'signedVerifier', pure.
verifyEnvelope :: [PublicKey] -> ByteString -> Either Text ByteString
verifyEnvelope keys bytes =
    case eitherDecode bytes :: Either String Value of
        Left err -> Left ("not a signed envelope, not even JSON: " <> Text.pack err)
        Right (Object o)
            | not (KeyMap.member "salmon-signed" o) ->
                Left "unsigned document: a signing key is configured (--follow-key) and this document carries no signed envelope"
        Right v -> case eitherDecode (encode v) :: Either String Envelope of
            Left err -> Left ("the signed envelope does not parse: " <> Text.pack err)
            Right env
                | null env.envSignatures -> Left "the signed envelope carries no signatures"
                | null keys -> Left "no signing key to verify against"
                | otherwise ->
                    let signed = LByteString.toStrict (canonicalBytes env.envDocument)
                        verdicts = [check signed key sig | sig <- env.envSignatures, key <- keys]
                     in if or (rights verdicts)
                            then Right (canonicalBytes env.envDocument)
                            else
                                Left $
                                    "no signature verifies against any of the "
                                        <> Text.pack (show (length keys))
                                        <> " configured key(s): "
                                        <> Text.intercalate "; " (dedupe (lefts verdicts))
  where
    -- one signature against one key: a mismatched key id is not tried (its
    -- reason says so), an algorithm a public key cannot verify with is
    -- refused rather than handed to jose, and a signature that does not
    -- verify says which key it was tried against.
    check :: ByteString.ByteString -> PublicKey -> Signature -> Either Text Bool
    check signed pk@(PublicKey k) sig
        | sig.sigKey /= keyId pk = Left ("signature by " <> short sig.sigKey <> " names no configured key")
        | not (publicAlg sig.sigAlg) = Left ("signature by " <> short sig.sigKey <> " uses " <> Text.pack (show sig.sigAlg) <> ", which no public key can verify")
        | otherwise = case JWK.verify sig.sigAlg (view JWK.jwkMaterial k) signed sig.sigBytes of
            Left (err :: JOSE.Error) -> Left ("signature by " <> short sig.sigKey <> ": " <> Text.pack (show err))
            Right True -> Right True
            Right False -> Left ("signature by " <> short sig.sigKey <> " does not verify: the document was altered after signing, or signed by another key")
    short = Text.take 12
    dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | The algorithms a /public/ key verifies: not @none@, not an HMAC.
publicAlg :: JWS.Alg -> Bool
publicAlg alg = case alg of
    JWS.None -> False
    JWS.HS256 -> False
    JWS.HS384 -> False
    JWS.HS512 -> False
    _ -> True
