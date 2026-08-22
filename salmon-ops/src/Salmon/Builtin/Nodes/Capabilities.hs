{- | Linux file-capability primitives (@setcap@\/@getcap@) — for granting a
binary just enough privilege (e.g. @CAP_NET_ADMIN@ to create bridge/tap
devices, or @CAP_DAC_OVERRIDE@ for a 9p passthrough export to act on behalf
of any guest uid) to run some operation unprivileged, instead of requiring
the whole calling process to be root.
-}
module Salmon.Builtin.Nodes.Capabilities where

import Salmon.Actions.UpDown (Requirement (..))
import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary (Binary, Command (..), withBinary)
import qualified Salmon.Builtin.Nodes.Binary as Binary
import Salmon.Op.Ref
import Salmon.Op.Track
import Salmon.Reporter

import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text

import GHC.IO.Exception (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Process.ListLike (proc)

-------------------------------------------------------------------------------
data Report
    = RunSetcap !SetcapCommand !Binary.Report
    deriving (Show)

-- | A Linux capability name, e.g. @"cap_net_admin"@ (see @capabilities(7)@).
type Capability = Text

{- | Grants a binary a set of capabilities (@setcap \<caps\>+eip \<path\>@), so
it can perform privileged operations without the whole calling process
running as root. @setcap@ is a set rather than an add — reapplying the same
capability set is already idempotent — but *running* @setcap@ at all needs
@CAP_SETFCAP@ (in practice: root), so this still guards with 'prelim' to
avoid needing that privilege on every re-run once the capabilities are
already in place: the same "does the effect already exist" shape as
"Salmon.Builtin.Nodes.Netfilter".'Salmon.Builtin.Nodes.Netfilter.rule',
just guarding "needs privilege at all" instead of "isn't idempotent".

Capabilities set this way are stored as an extended attribute on the file —
they survive a reboot, but not a package upgrade that reinstalls the
binary (@apt upgrade@ replaces the underlying inode), which is exactly what
'prelim' re-detects and 'up' re-grants the next time this 'Op' runs.
-}
grantCapabilities :: Reporter Report -> Track' (Binary "setcap") -> FilePath -> [Capability] -> Op
grantCapabilities r setcapBin path caps =
    withBinary setcapBin runSetcap (SetCap path capText) $ \apply ->
        op "grant-capabilities" nodeps $ \actions ->
            actions
                { help = Text.pack $ "grants " <> Text.unpack capText <> " to " <> path
                , ref = mkRef "grant-capabilities" (path, capText)
                , prelim = skipIfCapabilitiesGranted path caps
                , up = apply r'
                , down = Binary.untrackedExec runSetcap (RemoveCap path) "" r'
                }
  where
    capText = Text.intercalate "," caps
    r' = contramap (RunSetcap (SetCap path capText)) r

{- | @getcap \<path\>@'s output lists every capability currently granted —
'Skippable' iff all of @caps@ already show up in it.
-}
skipIfCapabilitiesGranted :: FilePath -> [Capability] -> IO Requirement
skipIfCapabilitiesGranted path caps = do
    (code, out, _err) <- readProcessWithExitCode "getcap" [path] ""
    pure $ case code of
        ExitSuccess | all (\c -> Text.unpack c `isInfixOf` out) caps -> Skippable
        _ -> Required

-------------------------------------------------------------------------------
data SetcapCommand
    = SetCap FilePath Text
    | RemoveCap FilePath
    deriving (Show)

runSetcap :: Command "setcap" SetcapCommand
runSetcap = Command go
  where
    go (SetCap path capText) =
        proc "setcap" [Text.unpack capText <> "+eip", path]
    go (RemoveCap path) =
        proc "setcap" ["-r", path]
