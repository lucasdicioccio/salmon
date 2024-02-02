{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Salmon.Actions.Help as Help
import Salmon.Actions.UpDown as UpDown
import Salmon.Actions.Check as Check
import Salmon.Actions.Dot as Dot
import Salmon.Op.Graph
import Salmon.Op.OpGraph
import Salmon.Op.Eval
import Salmon.Op.Actions
import Salmon.Op.Track
import Control.Monad.Identity
import Control.Comonad.Cofree
import Control.Lens ((^..))
import Data.Traversable (mapAccumL)
import Data.Functor.Contravariant (Contravariant(..),(>$<))
import Data.Functor.Contravariant.Divisible (Divisible(..), divided)
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Generic

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Builtin.Nodes.Demo as Demo
import Salmon.Builtin.Nodes.Keys as Keys
import Salmon.Builtin.Nodes.Certificates as Certs
import Salmon.Builtin.Nodes.Git as Git
import Salmon.Builtin.Nodes.Debian.Package as Debian
import Salmon.Builtin.Nodes.Debian.OS as Debian

filesystemExample =
    op "fs-example" (deps [run mkFileContents configObj]) id
  where
    mkFileContents :: Track' (FileContents Text, Directory)
    mkFileContents = Track filecontents >*< Track dir

    configObj = (contents,directory)
    contents = FileContents "./example-dir/file1" "hello world\n"
    directory = Directory "./example-dir"

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

demoOps =
  [ Demo.collatz [1,3,5,7,9,11,13,15]
  , sshKeysExample
  , tlsCertsExample
  , gitRepoExample
  , packagesExample
  ]

optimizedDeps =
  let
    base = op "demo" (deps demoOps) id
    pkgs = Debian.installAllDebsAtOnce base
  in 
  base `inject` pkgs

data Command
  = Run OpCommand
  | Help
  | DAG
  deriving (Generic, Show)

data OpCommand
  = Up
  | Down
  deriving (Generic, Show, Read)

instance ParseRecord Command
instance ParseRecord OpCommand
instance ParseField OpCommand
instance ParseFields OpCommand

-- main
main :: IO ()
main = void $ do
  cmd <- getRecord "demo-program"
  case cmd of
    Run Up -> do
      let gr0 = optimizedDeps
      let nat = pure . runIdentity
      UpDown.upTree nat gr0
    Run Down -> do
      let gr0 = optimizedDeps
      let nat = pure . runIdentity
      UpDown.downTree nat gr0
    Help -> do
      let gr0 = optimizedDeps
      Help.printHelpCograph (runIdentity $ expand gr0)
    DAG -> do
      let gr0 = optimizedDeps
      Dot.printCograph (runIdentity $ expand gr0)
