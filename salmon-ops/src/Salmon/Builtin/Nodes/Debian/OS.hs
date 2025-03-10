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

podman :: Installer "podman"
podman = installWith "podman"

postgres :: Installer "postgres"
postgres = installWith "postgresql"

psql :: Installer "psql"
psql = installWith "postgresql-client"

pg_ctl :: Installer "pg_ctl"
pg_ctl = installWith "postgresql-client"

pg_ctlcluster :: Installer "pg_ctlcluster"
pg_ctlcluster = installWith "postgresql-common"

curl :: Installer "curl-keygen"
curl = installWith "curl"

useradd :: Installer "useradd"
useradd = installWith "passwd"

groupadd :: Installer "groupadd"
groupadd = installWith "passwd"

ip :: Installer "ip"
ip = installWith "iproute2"

wg :: Installer "wg"
wg = installWith "wireguard"

nft :: Installer "nft"
nft = installWith "netfilter"

debootstrap :: Installer "debootstrap"
debootstrap = installWith "debootstrap"

upx :: Installer "upx"
upx = installWith "upx-ucl"

tar :: Installer "tar"
tar = installWith "tar"

minizinc :: Installer "minizinc"
minizinc = installWith "minizinc"
