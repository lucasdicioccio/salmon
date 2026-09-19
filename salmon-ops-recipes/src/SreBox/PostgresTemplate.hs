{-# LANGUAGE DeriveGeneric #-}

{- | Template databases: build a database once, lock it, and hand out copies.

The builtins ("Salmon.Builtin.Nodes.Postgres", under /Template databases/)
know how to lock, stamp, clone and drop. This module adds the one thing they
cannot express: that the template's contents are /behind/ its check.

= Why the build is opaque

The obvious shape is a lock node depending on the migrations that fill the
database. It breaks on the second @run up@: a walk applies dependencies
before it asks a dependant's check, so the migrations run first -- against a
database that now refuses connections, since that is what locking it means.
Nothing in a node's check can stop its dependencies being applied.

So 'template' carries the build as an 'Op' of its own and walks it from
inside @up@, the same move as
"SreBox.PostgresMigrations".@remoteMigrateOpaqueSetup@. The check then
guards the whole build: a template stamped with the current inputs is
skipped outright, and anything else is rebuilt __from nothing__ -- dropped,
recreated, filled, locked. Never migrated in place, which is what makes the
fingerprint worth trusting: it describes everything that went into the
database, not the last few steps.

= The fingerprint

'template_fingerprint' is whatever identifies the inputs -- normally a hash
of the migration files, see 'fingerprint'. It is stamped on the database
when a build finishes and compared by the check, and it goes into the node's
@notes@ so that under @run serve@ a re-declaration with new inputs is seen
as a change to the node (see "Salmon.Actions.Serve"'s @Stale@) rather than
as the same node declared again.
-}
module SreBox.PostgresTemplate (
    Report (..),
    Template (..),
    template,
    fingerprint,
    fileFingerprintParts,
) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.Identity (runIdentity)
import Crypto.Hash.SHA256 as SHA256
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as C8
import Data.Dynamic (toDyn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)

import qualified Salmon.Actions.Dot as Dot
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, withBinaryStdin)
import qualified Salmon.Builtin.Nodes.Postgres as Postgres
import Salmon.Op.Ref (mkRef)
import Salmon.Op.Track
import Salmon.Reporter

-------------------------------------------------------------------------------

data Report
    = TemplateSql !Postgres.Report
    | Build !Postgres.DatabaseName !(UpDown.Report Extension)
    deriving (Show)

-------------------------------------------------------------------------------

data Template
    = Template
    { template_database :: Postgres.DatabaseName
    , template_fingerprint :: Text
    -- ^ identifies the build's inputs; any change rebuilds the template.
    }
    deriving (Eq, Show, Generic)

instance FromJSON Template
instance ToJSON Template

{- | A database built by @build@, then locked as a template.

@build@ fills the database named 'template_database' -- a migration graph,
normally -- and may assume it exists and is empty: 'template' creates it
first. It runs as a nested walk (see the module header), so it does not
appear in @run tree@ and its nodes are not shared with the surrounding graph.
The cluster is an ordinary dependency instead, since the database has to be
created before @build@ starts.

@down@ drops the template. Clones already taken from it are independent
copies and are unaffected.
-}
template ::
    Reporter Report ->
    Track' Postgres.Server ->
    Track' (Binary "psql") ->
    Postgres.Server ->
    Template ->
    Op ->
    Op
template r cluster psql server tpl build =
    batch (Postgres.prepareTemplateSql name) $ \prepare ->
        batch (Postgres.lockTemplateSql name tpl.template_fingerprint) $ \lock ->
            batch (Postgres.dropTemplateSql name) $ \dropIt ->
                op "pg-template" (deps [run cluster server]) $ \actions ->
                    actions
                        { -- the same key as 'Postgres.database': a template is a
                          -- database at that site, and declaring a template and a
                          -- clone under one name is a collision worth reporting.
                          ref = mkRef "pg-db" (port, name)
                        , help = Text.unwords ["builds template database", name]
                        , notes = ["built from inputs " <> tpl.template_fingerprint, "rebuilt from nothing whenever the inputs change"]
                        , check = Postgres.checkTemplate port name tpl.template_fingerprint
                        , up = do
                            prepare r'
                            ok <- UpDown.upTree (contramap (Build name) r) (pure . runIdentity) build
                            -- a nested walk's failure is invisible to the outer
                            -- one unless this throws; the database is left
                            -- marked half-built, so the next pass starts over.
                            unless ok (throwIO (userError ("building template " <> Text.unpack name <> " failed")))
                            lock r'
                        , down = dropIt r'
                        , dynamics = [toDyn (Dot.OpaqueNode "template build")]
                        }
  where
    name = tpl.template_database
    port = server.serverPort
    r' = contramap (TemplateSql . Postgres.PGTemplate name) r
    batch sql = withBinaryStdin psql (Postgres.psqlBatchRun_Sudo port) Postgres.PsqlBatch (Text.encodeUtf8 sql)

-------------------------------------------------------------------------------

{- | A stable hash over a list of parts.

Each part is length-prefixed, so @["ab", "c"]@ and @["a", "bc"]@ differ --
which matters when the parts are paths and contents laid end to end.
-}
fingerprint :: [ByteString.ByteString] -> Text
fingerprint parts =
    Text.decodeUtf8 $ Base16.encode $ SHA256.finalize $ SHA256.updates SHA256.init (concatMap framed parts)
  where
    framed p = [C8.pack (show (ByteString.length p)) <> ":", p]

-- | Each file's path and contents, in the order given; feed to 'fingerprint'.
fileFingerprintParts :: [FilePath] -> IO [ByteString.ByteString]
fileFingerprintParts paths =
    concat <$> traverse (\p -> (\c -> [C8.pack p, c]) <$> ByteString.readFile p) paths
