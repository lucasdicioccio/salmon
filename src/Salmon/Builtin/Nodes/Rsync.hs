module Salmon.Builtin.Nodes.Rsync where

import Salmon.Op.Ref
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Builtin.Nodes.Binary

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text

import System.FilePath ((</>))
import System.Process.ListLike (CreateProcess, proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

data Remote = Remote { remoteUser :: Text , remoteHost :: Text }
  deriving (Show, Ord, Eq)

sendFile :: Track' (Binary "rsync") -> File "source" -> Remote -> FilePath -> Op
sendFile rsync script remote remotepath =
  withFile script $ \filepath ->
  using rsync rsyncRun (SendFile filepath remote  remotepath) $ \up -> 
    op "scp:sendfile" nodeps $ \actions -> actions {
        help = "copies over rsync"
      , ref = dotRef $ "rsync-copy:" <> Text.pack (show (filepath, remote))
      , up = up
      }

data Run = SendFile FilePath Remote FilePath

rsyncRun :: Command "rsync" Run
rsyncRun = Command $ \(SendFile src rem dst) ->
  proc "rsync"
    [ src
    , Text.unpack (loginAtHost rem) <> ":" <> dst
    ]

loginAtHost :: Remote -> Text
loginAtHost rem = mconcat [rem.remoteUser,"@",rem.remoteHost]
