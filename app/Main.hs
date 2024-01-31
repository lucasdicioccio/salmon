{-# LANGUAGE OverloadedStrings #-}

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

import Salmon.Builtin.Extension
import Salmon.Builtin.Nodes.Filesystem
import Salmon.Builtin.Nodes.Demo as Demo
import Salmon.Builtin.Nodes.Keys as Keys
import Salmon.Builtin.Nodes.Certificates as Certs

tlsCertsExample =
    op "tls-certs" certsinfo id
  where
    domain = Certs.Domain "localhost.example.com"
    key = Certs.Key Certs.RSA4096 "./tls/keys" "signing-key.rsa2048.pem"
    csr = Certs.SigningRequest domain key "./tls/certs/localhost.example.com" "cert.csr"
    ssReq = Certs.SelfSigned "./tls/certs/localhost.example.com/cert.pem" csr
    certsinfo = deps
      [ Certs.tlsKey key
      , Certs.signingRequest csr
      , Certs.selfSign ssReq
      ]

sshKeysExample =
    op "ssh-keys" keys id
  where
    keys = deps
      [ Keys.sshKey (Keys.SSHKeyPair Keys.RSA2048 "./ssh-keys" "example-key-rsa2k")
      , Keys.sshKey (Keys.SSHKeyPair Keys.RSA4096 "./ssh-keys" "example-key-rsa4k")
      , Keys.sshKey (Keys.SSHKeyPair Keys.ED25519 "./ssh-keys" "example-key-ed")
      ]

filesystemExample =
    track mkFileContents "fs-example"  configObj
  where
    mkFileContents :: Track' (FileContents Text, Directory)
    mkFileContents = Track filecontents >*< Track dir

    configObj = (contents,directory)
    contents = FileContents "./example-dir/file1" "hello world\n"
    directory = Directory "./example-dir"

-- main
main :: IO ()
main = void $ do
  let gr0 = Demo.collatz [1,3,5,7,9,11,13,15] `inject` filesystemExample `inject` sshKeysExample `inject` tlsCertsExample
  -- nat style
  let nat = pure . runIdentity
  Help.printHelpTree nat gr0
  UpDown.upTree nat gr0
  -- direct style
  Dot.printCograph (runIdentity $ expand gr0)
