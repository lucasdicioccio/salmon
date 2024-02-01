
module Salmon.Builtin.Nodes.Debian.OS where

import Salmon.Op.Track

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary
import Salmon.Builtin.Nodes.Debian.Package

import Data.Text (Text)

type Provider sym = Track' (Binary sym)

installWith :: Text -> Provider sym
installWith x = Track (const $ deb (Package "ssh-client"))

sshClient :: Provider "ssh-keygen"
sshClient = installWith "ssh-client"
