{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The tag of "Salmon.Op.PassPhase", on its own so that "Salmon.Op.Dag"
can leave it out of a node's representative without importing the
rewrite that reads it.
-}
module Salmon.Op.PassPhase.Tag (
    PassPhase (..),
    parsePassPhase,
    renderPassPhase,
    passPhaseOf,
    isPassPhase,
    setPassPhase,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Which side of the barrier a node is on.
data PassPhase
    = -- | builds and checks: all of them succeed before any 'DeployPhase' node starts
      BuildPhase
    | -- | pushes and deploys: start only once every 'BuildPhase' node succeeded
      DeployPhase
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Only for "Salmon.Builtin.CommandLine"'s 'RunCommand' instances.
instance FromJSON PassPhase

instance ToJSON PassPhase

-- | The words the command line uses: @build@, @deploy@.
renderPassPhase :: PassPhase -> Text
renderPassPhase BuildPhase = "build"
renderPassPhase DeployPhase = "deploy"

parsePassPhase :: Text -> Either Text PassPhase
parsePassPhase "build" = Right BuildPhase
parsePassPhase "deploy" = Right DeployPhase
parsePassPhase other = Left ("unknown phase " <> other <> " (expected build or deploy)")

-- | The phase among a node's dynamics, the last one if several.
passPhaseOf :: [Dynamic] -> Maybe PassPhase
passPhaseOf ds =
    case mapMaybe fromDynamic ds of
        [] -> Nothing
        ps -> Just (last ps)

isPassPhase :: Dynamic -> Bool
isPassPhase d = isJust (fromDynamic d :: Maybe PassPhase)

-- | Replace any phase among the dynamics with this one.
setPassPhase :: PassPhase -> [Dynamic] -> [Dynamic]
setPassPhase p ds = filter (not . isPassPhase) ds <> [toDyn p]
