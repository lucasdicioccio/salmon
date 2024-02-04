
module Salmon.Builtin.Nodes.Debian.OS where

import Salmon.Op.Track

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Binary
import Salmon.Builtin.Nodes.Debian.Package

import Data.Text (Text)

type Installer sym = Track' (Binary sym)

installWith :: Text -> Installer sym
installWith x = Track (const $ deb (Package x))

sshClient :: Installer "ssh-keygen"
sshClient = installWith "ssh-client"

openssl :: Installer "openssl"
openssl = installWith "openssl"

git :: Installer "git"
git = installWith "git-core"

bash :: Installer "bash"
bash = installWith "bash"

rsync :: Installer "rsync"
rsync = installWith "rsync"
