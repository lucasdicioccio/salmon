{-# LANGUAGE OverloadedStrings #-}

{- | A small JSON Schema checker for the OpenAPI document the loop serves
('Salmon.Actions.Serve.Http.openApiDocument'), and the lookups the drift
tests need: the schema of a named component, the schema of a documented
response.

It understands only what the document uses: @$ref@ into @#/components/schemas@,
@type@ (a name or a list), @const@, @enum@, @properties@, @required@,
@additionalProperties@, @items@ and @oneOf@. That is deliberate: a validator
library would be a new dependency for a test suite, and a keyword the document
starts using that this ignores is caught by 'unsupportedKeywords'.

'strict' is what makes it a drift guard rather than a shape check: an object
carrying a key its schema does not declare is an error, so a field added to an
encoder without an entry in the document fails a test.
-}
module Test.ServeApi (
    document,
    schemaNamed,
    validateAs,
    validateAgainst,
    validateResponse,
    validateEventData,
    documentedRoutes,
    unixRoutes,
    unsupportedKeywords,
) where

import Data.Aeson (Value (..), eitherDecodeStrict)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as C8
import Data.Foldable (toList)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty.HUnit (assertFailure)

import Salmon.Actions.Serve.Http (openApiDocument)

-- | The document the binary embeds.
document :: Value
document = either (error . ("the embedded OpenAPI document is not JSON: " <>)) id (eitherDecodeStrict openApiDocument)

lookupPath :: [Text] -> Value -> Maybe Value
lookupPath [] v = Just v
lookupPath (k : ks) (Object o) = KeyMap.lookup (Key.fromText k) o >>= lookupPath ks
lookupPath _ _ = Nothing

schemaNamed :: Text -> Maybe Value
schemaNamed n = lookupPath ["components", "schemas", n] document

-- | Errors validating a value against a named component schema (empty: valid).
validateAs :: Bool -> Text -> Value -> [String]
validateAs strict name v = case schemaNamed name of
    Nothing -> ["no schema named " <> Text.unpack name]
    Just s -> validateAgainst strict s v

validateAgainst :: Bool -> Value -> Value -> [String]
validateAgainst strict = go "$"
  where
    go :: String -> Value -> Value -> [String]
    go path schema v = case schema of
        Object o
            | Just (String r) <- KeyMap.lookup "$ref" o -> case resolve r of
                Nothing -> [path <> ": unresolvable " <> Text.unpack r]
                Just target -> go path target v
            | otherwise ->
                concat
                    [ maybe [] (\c -> [path <> ": expected " <> show c | c /= v]) (KeyMap.lookup "const" o)
                    , maybe [] (\e -> [path <> ": not one of " <> show e | not (isElem v e)]) (KeyMap.lookup "enum" o)
                    , maybe [] (typeCheck path v) (KeyMap.lookup "type" o)
                    , objectChecks path o v
                    , arrayChecks path o v
                    , maybe [] (oneOf path v) (KeyMap.lookup "oneOf" o)
                    ]
        _ -> []

    isElem v (Array xs) = v `elem` toList xs
    isElem _ _ = False

    resolve r = case Text.stripPrefix "#/components/schemas/" r of
        Just n -> schemaNamed n
        Nothing -> Nothing

    typeCheck path v (String t) = [path <> ": expected " <> Text.unpack t | not (isType t v)]
    typeCheck path v (Array ts) = [path <> ": expected one of " <> show ts | not (any (\t -> case t of String t' -> isType t' v; _ -> False) ts)]
    typeCheck _ _ _ = []

    isType "string" (String _) = True
    isType "integer" (Number n) = n == fromIntegral (round n :: Integer)
    isType "number" (Number _) = True
    isType "boolean" (Bool _) = True
    isType "object" (Object _) = True
    isType "array" (Array _) = True
    isType "null" Null = True
    isType _ _ = False

    objectChecks path o (Object vo) =
        let props = case KeyMap.lookup "properties" o of Just (Object p) -> p; _ -> KeyMap.empty
            required = case KeyMap.lookup "required" o of Just (Array rs) -> [r | String r <- toList rs]; _ -> []
            missing = [path <> ": missing " <> Text.unpack r | r <- required, not (KeyMap.member (Key.fromText r) vo)]
            declared =
                concat
                    [ go (path <> "." <> Key.toString k) s x
                    | (k, x) <- KeyMap.toList vo
                    , Just s <- [KeyMap.lookup k props]
                    ]
            open = KeyMap.lookup "additionalProperties" o == Just (Bool True)
            unknown =
                [ path <> ": undocumented field " <> Key.toString k
                | strict
                , not (KeyMap.null props)
                , not open
                , (k, _) <- KeyMap.toList vo
                , not (KeyMap.member k props)
                ]
         in missing <> declared <> unknown
    objectChecks _ _ _ = []

    arrayChecks path o (Array xs) = case KeyMap.lookup "items" o of
        Just s -> concat [go (path <> "[" <> show i <> "]") s x | (i, x) <- zip [0 :: Int ..] (toList xs)]
        Nothing -> []
    arrayChecks _ _ _ = []

    oneOf path v (Array branches) =
        let results = [(b, go path b v) | b <- toList branches]
            valid = [b | (b, []) <- results]
         in case valid of
                [_] -> []
                [] -> [path <> ": no branch matches: " <> shorten (blame v results)]
                _ -> [path <> ": several branches match"]
    oneOf _ _ _ = []

    -- of the failing branches, the ones whose kind/stream the value names, else all
    blame v results =
        let named = [es | (b, es) <- results, sameTag v b]
         in concat (if null named then map snd results else named)
    sameTag (Object vo) b = case resolveBranch b of
        Object bo | Just (Object ps) <- KeyMap.lookup "properties" bo ->
            all (\k -> case (KeyMap.lookup k vo, KeyMap.lookup k ps >>= constOf) of
                    (Just x, Just c) -> x == c
                    _ -> True) ["kind", "stream", "verb"]
        _ -> False
    sameTag _ _ = False
    resolveBranch b@(Object o) | Just (String r) <- KeyMap.lookup "$ref" o = fromMaybe b (resolve r)
    resolveBranch b = b
    constOf (Object o) = KeyMap.lookup "const" o
    constOf _ = Nothing
    shorten es = take 600 (unwords (take 6 es))

-- | The documented schema for this method, path (query stripped) and status,
-- and the errors validating the body against it. A status the operation does
-- not list is an error unless it is one every route may answer (404, 405).
validateResponse :: C8.ByteString -> C8.ByteString -> Int -> Value -> [String]
validateResponse method rawPath status body =
    case matchPath path of
        Nothing
            | status == 404 -> validateAs True "Error" body
            | otherwise -> ["undocumented path " <> Text.unpack path]
        Just template ->
            let op = lookupPath ["paths", template, Text.toLower (Text.pack (C8.unpack method))] document
                resp = op >>= lookupPath ["responses", Text.pack (show status)]
             in case (op, resp) of
                    (Nothing, _)
                        | status == 405 -> validateAs True "Error" body
                        | otherwise -> ["undocumented operation " <> C8.unpack method <> " " <> Text.unpack template]
                    (_, Nothing)
                        | status `elem` [404, 405, 401] -> validateAs True "Error" body
                        | otherwise -> ["undocumented status " <> show status <> " for " <> C8.unpack method <> " " <> Text.unpack template]
                    (_, Just r) -> case lookupPath ["content", "application/json", "schema"] r of
                        Just s -> validateAgainst True s body
                        Nothing -> []
  where
    path = Text.pack (C8.unpack (C8.takeWhile (/= '?') rawPath))

-- | Errors in the @data:@ of one server-sent event, whichever kind it is: the
-- synthetic @gap@, the server's own @enqueued@, or a report with @seq@ (and
-- @origin@ when it belongs to a command) added.
validateEventData :: Value -> [String]
validateEventData v = case (field "stream" v, field "kind" v) of
    (Just (String "server"), Just (String "gap")) -> validateAs True "GapEvent" v
    (Just (String "server"), Just (String "enqueued")) -> validateAs True "EnqueuedEvent" v
    _ ->
        validateAs True "Report" (without "origin" (without "seq" v))
            <> validateAs False "EventData" v
            <> maybe [] (validateAs True "Origin") (field "origin" v)
  where
    field k (Object o) = KeyMap.lookup (Key.fromText k) o
    field _ _ = Nothing
    without k (Object o) = Object (KeyMap.delete (Key.fromText k) o)
    without _ x = x

-- | The document's path template a request path matches.
matchPath :: Text -> Maybe Text
matchPath path = case lookupPath ["paths"] document of
    Just (Object ps) ->
        case [Key.toText k | (k, _) <- KeyMap.toList ps, matches (Key.toText k)] of
            (t : _) -> Just t
            [] -> Nothing
    _ -> Nothing
  where
    segs = filter (not . Text.null) . Text.splitOn "/"
    matches template =
        let ts = segs template
            ps = segs path
         in length ts == length ps && and (zipWith (\t p -> t == p || ("{" `Text.isPrefixOf` t)) ts ps)

-- | Every @(METHOD, template)@ the document describes.
documentedRoutes :: [(Text, Text)]
documentedRoutes = case lookupPath ["paths"] document of
    Just (Object ps) ->
        [ (Text.toUpper (Key.toText m), Key.toText p)
        | (p, Object item) <- KeyMap.toList ps
        , (m, _) <- KeyMap.toList item
        , Key.toText m `elem` ["get", "post", "put", "delete", "patch"]
        ]
    _ -> []

-- | 'documentedRoutes' without the operations marked @x-tcp-only@: the ones
-- the middleware of the TCP listener answers and the unix socket does not.
unixRoutes :: [(Text, Text)]
unixRoutes = case lookupPath ["paths"] document of
    Just (Object ps) ->
        [ (Text.toUpper (Key.toText m), Key.toText p)
        | (p, Object item) <- KeyMap.toList ps
        , (m, op) <- KeyMap.toList item
        , Key.toText m `elem` ["get", "post", "put", "delete", "patch"]
        , lookupPath ["x-tcp-only"] op /= Just (Bool True)
        ]
    _ -> []

-- | Keywords used anywhere in the document's schemas that 'validateAgainst' ignores.
unsupportedKeywords :: [Text]
unsupportedKeywords = nub (filter (`notElem` known) (concatMap collect schemas))
  where
    schemas = case lookupPath ["components", "schemas"] document of
        Just (Object ss) -> KeyMap.elems ss
        _ -> []
    known =
        [ "$ref", "type", "const", "enum", "properties", "required", "additionalProperties", "items", "oneOf"
        , "description", "format", "discriminator", "propertyName"
        ]
    -- keys that are schema keywords: the keys of a schema object, not of a `properties` map
    collect :: Value -> [Text]
    collect (Object o) = concat [keyword k v | (k, v) <- KeyMap.toList o]
    collect _ = []
    keyword k v
        | Key.toText k `elem` ["properties"] = case v of Object ps -> concatMap (collect . snd) (KeyMap.toList ps); _ -> []
        | Key.toText k `elem` ["items"] = collect v
        | Key.toText k `elem` ["oneOf"] = case v of Array bs -> concatMap collect (toList bs); _ -> []
        | otherwise = [Key.toText k]
