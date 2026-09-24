{-# LANGUAGE OverloadedStrings #-}

{- | The HTTP registry for "Salmon.Actions.Follow": the document for a label
is one @GET@, and the server does the change detection.

The template is the operator's: @--follow https://host/path@ fetches
@\<path\>/\<label\>.json@, and a @{label}@ anywhere in the URL places the
label there instead (@https://host/seed/latest/{label}@, the spec's example).
The stamp is the response's @ETag@, or its @Last-Modified@ when there is no
@ETag@, handed back as @If-None-Match@ / @If-Modified-Since@ so that an
unchanged document is a @304@ and no body crosses the wire — the same "ask
before reading" the directory registry does with an mtime, done by the
server. A server that sends neither is read in full every round and the
digest does the work, as ever.

What is what: @200@ is a document, @304@ is 'Unchanged', @404@ is 'Absent'
(the registry answered; not a failed round), and everything else — a @5xx@,
a @403@, a connection refused, a timeout — throws and is a failed round on
the scheduler's ladder. Redirects are followed by @http-client@'s default.
Bytes that do not parse are the fetcher's 'Salmon.Actions.Follow.Malformed',
not this module's concern.
-}
module Salmon.Actions.Follow.Registry.Http (
    Options (..),
    defaultOptions,
    newManager,
    httpRegistry,
    addressFor,
    fetchUrl,
    HttpFailed (..),
) where

import Control.Exception (Exception, throwIO)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client (Manager, httpLbs, parseRequest, requestHeaders, responseBody, responseHeaders, responseStatus, responseTimeoutMicro)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types (statusCode)

import Salmon.Actions.Follow (Digest (..), Fetch (..), Label, Registry (..), Stamp (..), digestOf, labelText)

-- | What every HTTP-backed registry shares.
newtype Options = Options
    { optTimeout :: Int
    -- ^ microseconds a whole response may take, connection included
    }
    deriving (Show, Eq)

-- | Thirty seconds: a registry is a small document, and a round that hangs
-- holds every other label's fetch behind it.
defaultOptions :: Options
defaultOptions = Options{optTimeout = 30 * 1000000}

-- | One manager per process — TLS or plain, decided per request by its scheme.
newManager :: Options -> IO Manager
newManager o = newTlsManagerWith tlsManagerSettings{HTTP.managerResponseTimeout = responseTimeoutMicro o.optTimeout}

{- | Where a label's document is: the template with @{label}@ replaced, or
@\<base\>/\<label\>.json@ when the template has no placeholder (a trailing
slash on the base is not doubled).
-}
addressFor :: Text -> Label -> Text
addressFor template lbl
    | placeholder `Text.isInfixOf` template = Text.replace placeholder (labelText lbl) template
    | otherwise = Text.dropWhileEnd (== '/') template <> "/" <> labelText lbl <> ".json"
  where
    placeholder = "{label}"

-- | A registry over a URL template; its name is the template as given.
httpRegistry :: Manager -> Text -> Registry
httpRegistry mgr template =
    Registry
        { registryName = template
        , registryFetch = \lbl previous -> fetchUrl mgr (addressFor template lbl) previous
        }

-- | A status this module has no answer for.
data HttpFailed = HttpFailed
    { httpFailedUrl :: Text
    , httpFailedStatus :: Int
    }

instance Show HttpFailed where
    show e = "GET " <> Text.unpack e.httpFailedUrl <> " answered " <> show e.httpFailedStatus

instance Exception HttpFailed

{- | @GET@ a URL conditionally on the stamp from the last time, which is
@etag:...@ or @last-modified:...@ so that the header it goes back in is
known. Throws for anything but @200@, @304@ and @404@; a URL that does not
parse throws too.
-}
fetchUrl :: Manager -> Text -> Maybe Stamp -> IO Fetch
fetchUrl mgr url previous = do
    req0 <- parseRequest (Text.unpack url)
    let conditional = case previous of
            Just (Stamp s)
                | Just etag <- Text.stripPrefix etagPrefix s -> [("If-None-Match", Text.encodeUtf8 etag)]
                | Just lm <- Text.stripPrefix lastModifiedPrefix s -> [("If-Modified-Since", Text.encodeUtf8 lm)]
            _ -> []
        req = req0{requestHeaders = ("Accept", "application/json") : conditional ++ requestHeaders req0}
    resp <- httpLbs req mgr
    case statusCode (responseStatus resp) of
        200 -> do
            let bytes = responseBody resp
            pure (Found (stampOf (responseHeaders resp)) (digestOf bytes) bytes)
        304 -> pure Unchanged
        404 -> pure Absent
        code -> throwIO (HttpFailed url code)
  where
    etagPrefix = "etag:"
    lastModifiedPrefix = "last-modified:"
    stampOf headers =
        case (lookup "ETag" headers, lookup "Last-Modified" headers) of
            (Just etag, _) -> Stamp (etagPrefix <> Text.decodeUtf8Lenient etag)
            (Nothing, Just lm) -> Stamp (lastModifiedPrefix <> Text.decodeUtf8Lenient lm)
            -- nothing to ask conditionally with: read every round, and let
            -- the digest say whether anything changed
            (Nothing, Nothing) -> Stamp "none"
