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

ssh :: Installer "ssh"
ssh = installWith "ssh-client"

systemctl :: Installer "systemctl"
systemctl = installWith "systemd"

sudo :: Installer "sudo"
sudo = installWith "sudo"

postgres :: Installer "postgres"
postgres = installWith "postgresql-12"

psql :: Installer "psql"
psql = installWith "postgresql-client-12"

pg_ctl :: Installer "pg_ctl"
pg_ctl = installWith "postgresql-client-12"

pg_ctlcluster :: Installer "pg_ctlcluster"
pg_ctlcluster = installWith "postgresql-common"
