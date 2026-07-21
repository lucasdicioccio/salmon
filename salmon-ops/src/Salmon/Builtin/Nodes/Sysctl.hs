module Salmon.Builtin.Nodes.Sysctl where

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Data.Text (Text)
import qualified Data.Text as Text

import System.Process.ListLike (proc)

-------------------------------------------------------------------------------
data Report
    = RunSysctl !SysctlCommand !Binary.Report
    deriving (Show)

-------------------------------------------------------------------------------

data Setting
    = Setting
    { settingKey :: Text
    , settingValue :: Text
    }
    deriving (Show)

-- | Idempotently sets a runtime kernel parameter (e.g. @net.ipv4.ip_forward@).
set :: Reporter Report -> Track' (Binary "sysctl") -> Setting -> Op
set r sysctl s =
    withBinary sysctl sysctlcommand cmd $ \apply ->
        op "sysctl-set" nodeps $ \actions ->
            actions
                { help = mconcat ["sets kernel parameter ", s.settingKey, " = ", s.settingValue]
                , ref = mkRef "sysctl-set" s.settingKey
                , up = apply r'
                }
  where
    r' = contramap (RunSysctl cmd) r
    cmd = Set s

data SysctlCommand
    = Set Setting
    deriving (Show)

sysctlcommand :: Command "sysctl" SysctlCommand
sysctlcommand = Command $ \cmd -> case cmd of
    (Set s) ->
        proc
            "sysctl"
            [ "-w"
            , Text.unpack (s.settingKey <> "=" <> s.settingValue)
            ]
