{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

-- to-re-export
import Salmon.Op.OpGraph (inject)
import Salmon.Op.Configure (Configure(..))
import Salmon.Op.Track (Track(..), (>*<))
import Data.Aeson (FromJSON, ToJSON)
import Options.Generic
import Options.Applicative

import qualified Data.Text as Text
import Text.Read (readMaybe)

import Salmon.Builtin.Nodes.Filesystem
import Salmon.Builtin.Nodes.Demo as Demo
import Salmon.Builtin.Nodes.Keys as Keys
import Salmon.Builtin.Nodes.Certificates as Certs
import Salmon.Builtin.Nodes.Git as Git
import Salmon.Builtin.Nodes.Debian.Package as Debian
import Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Bash as Bash
import Salmon.Builtin.Extension
import Salmon.Builtin.CommandLine

filesystemExample =
    op "fs-example" (deps [run mkFileContents configObj]) id
  where
    mkFileContents :: Track' (FileContents Text, Directory)
    mkFileContents = Track filecontents >*< Track dir

    configObj = (contents,directory)
    contents = FileContents "./example-dir/file1" "hello world\n"
    directory = Directory "./example-dir"

bashHelloExample =
    op "bash-hello" (deps [go]) id
  where
    go = Bash.run Debian.bash script
    script = ToInstall mkBashScript "./example-script/hello.sh"
    mkBashScript :: Track' FilePath
    mkBashScript = Track $ \path -> filecontents $ FileContents path body
    body :: Text
    body = "echo hello >> hello-world.txt"


tlsCertsExample =
    op "tls-certs" certsinfo id
  where
    domain = Certs.Domain "localhost.example.com"
    key = Certs.Key Certs.RSA4096 "./tls/keys" "signing-key.rsa2048.pem"
    csr = Certs.SigningRequest domain key "./tls/certs/localhost.example.com" "cert.csr"
    ssReq = Certs.SelfSigned "./tls/certs/localhost.example.com/cert.pem" csr
    openssl = Debian.openssl
    certsinfo = deps
      [ Certs.tlsKey openssl key
      , Certs.signingRequest openssl csr
      , Certs.selfSign openssl ssReq
      ]

sshKeysExample =
    op "ssh-keys" keys id
  where
    ssh = Debian.sshClient
    keys = deps
      [ Keys.signKey ssh caKey (Keys.KeyIdentifier "key0") key0
      , Keys.signKey ssh caKey (Keys.KeyIdentifier "key1") key1
      ]
    caKey = Keys.SSHCertificateAuthority $ Keys.SSHKeyPair Keys.ED25519 "./ssh-keys/ca" "certificate-auth"
    key0 = Keys.SSHKeyPair Keys.ED25519 "./ssh-keys/keys" "node1"
    key1 = Keys.SSHKeyPair Keys.ED25519 "./ssh-keys/keys" "node2"

gitRepoExample =
    Git.repo git (Repo "./git-repos/" "ks" remote branch)
  where
    remote = Git.Remote "git@github.com:kitchensink-tech/kitchensink.git"
    branch = Git.Branch "main"
    git = Debian.git

packagesExample =
   Debian.deb (Debian.Package "vlc")

demoOps n =
  [ Demo.collatz [x | x <- [1 .. n], odd x]
  , sshKeysExample
  , tlsCertsExample
  , gitRepoExample
  , packagesExample
  , bashHelloExample
  ]

-------------------------------------------------------------------------------
data Spec
  = Spec { specCollatz :: Int }
  deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec

demo :: Track' Spec
demo = Track $ \(Spec cltz) -> optimizedDeps $ op "demo" (deps $ demoOps cltz) id
  where
optimizedDeps base =
  let
    pkgs = Debian.installAllDebsAtOnce base
  in 
  base `inject` pkgs

-------------------------------------------------------------------------------
data Seed = Seed { collatzNum :: Int }

instance ParseRecord Seed where
  parseRecord =
    Seed <$> option auto ( long "n-collatz" )

configure :: Configure' Seed Spec
configure = Configure $ \(Seed n) -> pure (Spec n)

-------------------------------------------------------------------------------
main :: IO ()
main = do
  -- cmd <- getRecord "demo-program" :: IO (Command Seed)
  let desc = fullDesc <> progDesc "A Salmon program." <> header "demonstration examples"
  let opts = info parseRecord desc
  cmd <- execParser opts
  execCommand configure demo cmd
