
module SreBox.Environment where

data Environment
  = Production
  | Staging
  deriving (Show, Ord, Eq)
