
module Salmon.Op.G where

import Control.Comonad.Cofree (Cofree(..))
import Data.Aeson (FromJSON, ToJSON(..), (.=), (.:))
import Data.Coerce (coerce)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Salmon.Op.Graph (Graph)

-- | Helper to provide Aeson instances for (Cofree Graph a).
newtype G a = G { getCofreeGraph :: (Cofree Graph a) }

instance ToJSON a => ToJSON (G a) where
  toJSON (G (x :< p)) =
    Aeson.object
      [ "node" .= toJSON x
      , "preds" .= (toJSON $ fmap G p)
      ]

instance FromJSON a => FromJSON (G a) where
  parseJSON = Aeson.withObject "cofree layer" $ \o -> do
    obj <- o .: "node"
    preds <- o .: "preds" :: Aeson.Parser (Graph (G a))
    pure $ G $ obj :< fmap coerce preds

