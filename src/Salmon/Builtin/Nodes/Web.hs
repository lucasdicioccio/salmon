module Salmon.Builtin.Nodes.Web where

import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Builtin.Extension

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import Network.HTTP.Client (Manager, Request, httpNoBody)

data Call
  = Call
  { callManager :: Manager
  , callRequest :: Request
  }

call :: Track' Call -> Call -> Op
call t call =
  op "http-call" (deps [run t call]) $ \actions -> actions {
      help = "performs an HTTP call"
    , ref = dotRef $ "http:call:" <> Text.pack (show call.callRequest)
    , up = up
    }
  where
    up = void $ httpNoBody call.callRequest call.callManager
