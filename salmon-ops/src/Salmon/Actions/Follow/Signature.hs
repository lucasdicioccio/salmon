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

__A signature is worth something only at the address it was signed for.__ A
registry that can be written to but not signed for could otherwise copy a
validly signed @canary@ document to @prod@'s address, and every host following
@prod@ would apply it (the cache, read back through the same verifier, is
exposed the same way). So the verifier is told the label it is looking at
('Salmon.Actions.Follow.Verifier') and holds two rules:

* the __document names its label__: a top-level @label@ member of the
  document, so the signature (which is over the document's canonical bytes)
  covers it, where a label kept beside the signatures would not be signed at
  all. A document whose @label@ is not the one it was fetched for is refused,
  the reason naming both. @salmon-fleet sign --label L@ writes it.
* a __key may speak for some labels only__ ('TrustedKey'): a key given as
  @--follow-key LABEL=FILE@ verifies documents for that label and no other;
  a bare @--follow-key FILE@ keeps meaning any label.

A signed document with no @label@ (signed before this) is refused unless the
'AcceptUnlabelled' migration policy is on, which is @--follow-accept-unlabelled@.
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

    signDocumentFor,

    -- * The verifier
    TrustedKey (..),
    trustsAnyLabel,
    trustsOnly,
    Legacy (..),
    parseKeySpec,
    signedVerifier,
    verifyEnvelope,
    verifyEnvelopeFor,
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

import Salmon.Actions.Follow (Label, Verifier, labelText, mkLabel)

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
signDocument key = signDocumentFor key Nothing

{- | 'signDocument' for a document that names the label it is for: the
@label@ member is put into the document before it is signed, so the
signature covers it. A document that already names a different label is not
signed (a mistake worth stopping on); one that names the same is left as it
is. -}
signDocumentFor :: PrivateKey -> Maybe Label -> ByteString -> IO (Either Text ByteString)
signDocumentFor key@(PrivateKey k) mlabel bytes =
    case eitherDecode bytes of
        Left err -> pure (Left ("the document is not JSON: " <> Text.pack err))
        Right (Object o0) | Just lbl <- mlabel, Just existing <- KeyMap.lookup "label" o0, existing /= String (labelText lbl) ->
            pure (Left ("the document already names " <> Text.pack (show existing) <> " as its label, not " <> labelText lbl))
        Right (Object o0) -> signObject (Object (maybe o0 (\lbl -> KeyMap.insert "label" (String (labelText lbl)) o0) mlabel))
        Right _ -> pure (Left "the document is not a JSON object")
  where
    signObject doc = do
            outcome <- JOSE.runJOSE $ do
                alg <- JWK.bestJWSAlg k
                sig <- JWK.sign alg (view JWK.jwkMaterial k) (LByteString.toStrict (canonicalBytes doc))
                pure (alg, sig)
            pure $ case outcome of
                Left (err :: JOSE.Error) -> Left ("cannot sign with this key: " <> Text.pack (show err))
                Right (alg, sig) ->
                    Right (encode (Envelope doc [Signature (keyId (publicKey key)) alg sig]) <> "\n")

-------------------------------------------------------------------------------
-- the verifier

{- | Refuse everything but an envelope one of these keys signed, and hand
the loop the document inside it. 'Right' is the inner document's canonical
bytes; the digest handed in is only for the reasons' sake. -}
signedVerifier :: Legacy -> [TrustedKey] -> Verifier
signedVerifier legacy keys lbl _ bytes = pure (verifyEnvelopeFor legacy keys lbl bytes)

{- | A @--follow-key@ argument: @FILE@, or @LABEL=FILE@ for a key that speaks
for that label only. Something is a label only if what precedes the first
@=@ has no @/@ and is a valid label, so a path that happens to contain an
@=@ stays a path. -}
parseKeySpec :: Text -> Either Text (Maybe Label, FilePath)
parseKeySpec spec = case Text.breakOn "=" spec of
    (l, r)
        | not (Text.null r), not (Text.null l), not (Text.any (== '/') l) -> do
            lbl <- mkLabel l
            pure (Just lbl, Text.unpack (Text.drop 1 r))
    _ -> Right (Nothing, Text.unpack spec)

-- | A public key, and the labels it may speak for.
data TrustedKey = TrustedKey
    { trustedKey :: !PublicKey
    , trustedLabels :: !(Maybe [Label])
    -- ^ 'Nothing': any label. 'Just': these labels and no others.
    }
    deriving (Show, Eq)

-- | A key that verifies documents for every label.
trustsAnyLabel :: PublicKey -> TrustedKey
trustsAnyLabel k = TrustedKey k Nothing

-- | A key that verifies documents for one label only.
trustsOnly :: Label -> PublicKey -> TrustedKey
trustsOnly l k = TrustedKey k (Just [l])

speaksFor :: Label -> TrustedKey -> Bool
speaksFor l t = maybe True (l `elem`) t.trustedLabels

-- | What to do with a signed document that names no label: one signed before
-- documents named theirs.
data Legacy
    = -- | refuse it (the default)
      RefuseUnlabelled
    | -- | accept it, the migration flag
      AcceptUnlabelled
    deriving (Show, Eq)

{- | 'signedVerifier', pure: the signature is checked against the keys that
may speak for this label, then the document's own @label@ against the label
it was fetched for. -}
verifyEnvelopeFor :: Legacy -> [TrustedKey] -> Label -> ByteString -> Either Text ByteString
verifyEnvelopeFor legacy keys lbl bytes = do
    let here = [trustedKey t | t <- keys, speaksFor lbl t]
        elsewhere = [trustedKey t | t <- keys, not (speaksFor lbl t)]
    doc <- case verifiedDocument here bytes of
        Right d -> Right d
        Left why -> Left (why <> notTrustedHere elsewhere)
    case doc of
        Object o -> case KeyMap.lookup "label" o of
            Just (String t)
                | t == labelText lbl -> Right (canonicalBytes doc)
                | otherwise ->
                    Left ("the document is signed for label " <> t <> " but was fetched for label " <> labelText lbl <> ": refusing to apply one label's document at another's address")
            Just other -> Left ("the document's label is not a string: " <> Text.pack (show other))
            Nothing -> case legacy of
                AcceptUnlabelled -> Right (canonicalBytes doc)
                RefuseUnlabelled ->
                    Left ("the signed document names no label, so it could have been signed for any address (this one is " <> labelText lbl <> "); sign it again with `salmon-fleet sign --label " <> labelText lbl <> "`, or accept unlabelled documents while migrating with --follow-accept-unlabelled")
        _ -> Left "the signed document is not a JSON object"
  where
    -- a signature by a key that exists but may not speak here says so,
    -- rather than the vaguer "names no configured key"
    notTrustedHere elsewhere = case [keyId k | k <- elsewhere, signedBy k] of
        [] -> ""
        ids -> " (signed by " <> Text.intercalate ", " (fmap short ids) <> ", which may not speak for label " <> labelText lbl <> ")"
    signedBy k = case eitherDecode bytes :: Either String Envelope of
        Right env -> keyId k `elem` fmap sigKey env.envSignatures
        Left _ -> False
    short = Text.take 12

-- | 'signedVerifier' for keys that each speak for every label, and a
-- document that need not name one. The check that is only about the
-- signature: what 'verifyEnvelopeFor' builds on.
verifyEnvelope :: [PublicKey] -> ByteString -> Either Text ByteString
verifyEnvelope keys bytes = canonicalBytes <$> verifiedDocument keys bytes

-- | The document inside an envelope one of these keys signed.
verifiedDocument :: [PublicKey] -> ByteString -> Either Text Value
verifiedDocument keys bytes =
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
                            then Right env.envDocument
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
