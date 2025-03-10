{-# LANGUAGE DeriveGeneric #-}

module Bootstrap.Spec where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Salmon.Builtin.Nodes.Self as Self

data Spec
    = Bootstrap
    { bootstrapSelf :: Self.SelfPath
    , bootstrapImage :: Text
    }
    deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec
