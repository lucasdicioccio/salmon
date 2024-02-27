{-# LANGUAGE DeriveGeneric #-}

module SreBox.PostgresMigrations where

import Control.Comonad.Cofree (Cofree(..))
import Data.ByteString.Char8 as C8
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Maybe (catMaybes)
import Data.List (stripPrefix)
import System.FilePath ((</>))
import qualified Data.Text as Text
import GHC.Generics

import Salmon.Op.OpGraph
import Salmon.Op.Graph
import Salmon.Op.Eval
import Salmon.Op.Ref
import Salmon.Op.G
import Salmon.Builtin.Extension
import Salmon.Builtin.Migrations
import Salmon.Builtin.Nodes.Debian.OS as Debian
import Salmon.Builtin.Nodes.Binary
import Salmon.Builtin.Nodes.Postgres
import Salmon.Builtin.Nodes.Filesystem

data MigrationFile
  = MigrationFile
  { path :: FilePath
  } deriving (Show, Generic)

instance FromJSON MigrationFile
instance ToJSON MigrationFile

defaultMigrationReader :: MigrationReader MigrationFile
defaultMigrationReader =
    MigrationReader
      (pure . MigrationFile)
      (parsePredecessors)
      id
  where
    parsePredecessors :: C8.ByteString -> [FilePath]
    parsePredecessors txt =
      catMaybes $ fmap parsePredecessorLine $ C8.lines txt

    parsePredecessorLine :: C8.ByteString -> Maybe FilePath
    parsePredecessorLine line =
      C8.unpack <$> C8.stripPrefix prefix line

    prefix :: C8.ByteString
    prefix = "-- migrate.after: "

migrateG
  :: Track' (Binary "psql")
  -> Track' (ConnString FilePath)
  -> ConnString FilePath
  -> G MigrationFile
  -> Op
migrateG psql mksetup connstring g = migrate psql mksetup connstring (coerce g)

-- | A helper to turn a migration graph into an Op.
migrate
  :: Track' (Binary "psql")
  -> Track' (ConnString FilePath)
  -> ConnString FilePath
  -> Cofree Graph MigrationFile
  -> Op
migrate psql mksetup connstring (m :< x) =
  let 
      current, pred :: Op
      current = userScript psql mksetup connstring (PreExisting m.path) 
      pred = evalPred "" x
  in
  current `inject` pred
  where
    gorec = migrate psql mksetup connstring
    setref lineage actions =
      actions { ref = dotRef $ Text.pack $ m.path  <> " " <> lineage }

    -- the string in eval pred accumulates left/right branches choices to disambiguate noop nodes by ref
    evalPred :: String -> Graph (Cofree Graph MigrationFile) -> Op
    evalPred l (Vertices []) = realNoop
    evalPred l (Vertices zs) =
      op "migrate:deps" (deps $ fmap gorec zs) (setref l)
    evalPred l (Overlay g1 g2) =
      op "migrate:deps" (deps [evalPred ('l':l) g1, evalPred ('r':l) g2]) (setref l)
    evalPred l (Connect g1 g2) =
      op "migrate:deps" (deps [evalPred ('l':l) g2 `inject` evalPred ('r':l) g1]) (setref l)

-------------------------------------------------------------------------------

data MigrationPlan a =
  MigrationPlan
  { migration_connstring :: ConnString a
  , migration_file :: G MigrationFile
  } deriving (Generic)
instance ToJSON a => ToJSON (MigrationPlan a)
instance FromJSON a => FromJSON (MigrationPlan a)

migratePlan :: MigrationPlan FilePath -> Op
migratePlan plan =
  migrateG Debian.psql ignoreTrack plan.migration_connstring plan.migration_file
