{- | Where @run serve@'s HTTP surface listens, and the two things a TCP
listener is configured with before anything is bound: its session limits
and its token file.

Apart from "Salmon.Actions.Serve.Http" so that the command line can parse
and validate @--http@\/@--http-tcp@ in a @salmon-ops@ built without the
@http-server@ flag (no warp, warp-tls or wai): such a build knows what the
flags mean and refuses them, rather than not knowing them at all.
"Salmon.Actions.Serve.Http" re-exports everything here.
-}
module Salmon.Actions.Serve.HttpBind (
    Bind (..),
    TlsBind (..),
    SessionPolicy (..),
    defaultSessionPolicy,
    TokenError (..),
    readTokenFile,
) where

import Control.Exception (Exception)
import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Char (isSpace)
import System.Posix.Files (fileMode, getFileStatus)

{- | Where a server listens. A unix socket needs nothing but its path; TCP
needs everything in 'TlsBind', and there is deliberately no constructor for
TCP without it.
-}
data Bind
    = BindUnix FilePath
    | BindTls TlsBind
    deriving (Show)

{- | A TCP listener: the address to bind, the certificate and key warp-tls
serves, and the token every request on it must present. The token is the
file's content already read and trimmed ('readTokenFile'), so that a
server is refused before it binds rather than after.
-}
data TlsBind = TlsBind
    { tlsHost :: String
    -- ^ an address to bind, never a wildcard by omission: the caller spells it
    , tlsPort :: Int
    -- ^ @0@ for any free port; 'serverBoundTcp' says which
    , tlsCertFile :: FilePath
    , tlsKeyFile :: FilePath
    , tlsToken :: ByteString.ByteString
    , tlsSessions :: SessionPolicy
    -- ^ how long a browser's sign-in lasts ('defaultSessionPolicy' unless told)
    }
    deriving (Show)


{- | How long a session lasts. Two limits, each 'Nothing' for none, because
they guard against different things. The __lifetime__ counts from sign-in
and ends a session however busy it is — it bounds how long a stolen
cookie or a tab left open over a weekend stays good, and it cuts an event
stream the session has open. The __idle__ limit counts from the session's
last use, and an open @\/events@ stream is use: the page makes almost no
requests while it is being watched, and a dashboard left open should not
expire for being looked at. So "idle" means nobody is looking, and the
lifetime is what ends a session somebody is.

What bounds the listener's memory is that an ended session is dropped at
every sign-in ('newSession'), and at the next request that presents it —
at most the sessions signed in within one lifetime are held. With both
limits off nothing ends a session but signing out or a restart.
-}
data SessionPolicy = SessionPolicy
    { sessionLifetime :: Maybe Double
    -- ^ seconds from sign-in
    , sessionIdle :: Maybe Double
    -- ^ seconds since last use, not counting while a stream is open
    }
    deriving (Show, Eq)

-- | Twelve hours from sign-in, one hour of nobody looking.
defaultSessionPolicy :: SessionPolicy
defaultSessionPolicy = SessionPolicy (Just (12 * 3600)) (Just 3600)


-- | Why a token file was not accepted.
data TokenError
    = -- | others can read it, so it is not a secret: the file's mode
      TokenFileReadable FilePath
    | -- | nothing but whitespace in it
      TokenFileEmpty FilePath
    deriving (Show, Eq)

instance Exception TokenError

{- | The token in a file: its content with surrounding whitespace removed
(so a trailing newline from @echo@ is not part of it). Refused when the
file is readable by others — a token anyone on the box can read is not one —
and when it is empty, which would make every request with an empty
@Bearer@ valid. A file that cannot be read at all throws as any read does.
-}
readTokenFile :: FilePath -> IO (Either TokenError ByteString.ByteString)
readTokenFile path = do
    st <- getFileStatus path
    if fileMode st .&. 0o004 /= 0
        then pure (Left (TokenFileReadable path))
        else do
            raw <- ByteString.readFile path
            let token = Char8.dropWhileEnd isSpace (Char8.dropWhile isSpace raw)
            pure (if ByteString.null token then Left (TokenFileEmpty path) else Right token)

