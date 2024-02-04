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
import qualified Salmon.Builtin.Nodes.Demo as Demo
import qualified Salmon.Builtin.Nodes.Cabal as Cabal
import qualified Salmon.Builtin.Nodes.Keys as Keys
import qualified Salmon.Builtin.Nodes.Certificates as Certs
import qualified Salmon.Builtin.Nodes.Git as Git
import qualified Salmon.Builtin.Nodes.Debian.Package as Debian
import qualified Salmon.Builtin.Nodes.Debian.OS as Debian
import qualified Salmon.Builtin.Nodes.Bash as Bash
import Salmon.Builtin.Extension
import qualified Salmon.Builtin.CommandLine as CLI

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
    script = Generated mkBashScript "./example-script/hello.sh"
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
    Git.repo git (Git.Repo "./git-repos/" "ks" remote branch)
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
  , filesystemExample
  , bashHelloExample
  ]

-------------------------------------------------------------------------------
buildOps _ =
  [ microDNS
  , kitchensink
  ]

kitchensink = cabalRepoBuild
  "ks"
  "exe:kitchen-sink"
  "https://github.com/kitchensink-tech/kitchensink.git"
  "main"
  "hs"

microDNS = cabalRepoBuild
  "microdns"
  "microdns"
  "https://github.com/lucasdicioccio/microdns.git"
  "main"
  ""

-- builds a cabal repository
cabalRepoBuild dirname target remote branch subdir = 
    withFile (Git.repofile mkrepo repo subdir) $ \repopath ->
      Cabal.install cabal (Cabal.Cabal repopath target) bindir
  where
    bindir = "/opt/builds/bin"
    repo = Git.Repo "./git-repos/" dirname (Git.Remote remote) (Git.Branch branch)
    git = Debian.git
    cabal = (Track $ \_ -> noop "preinstalled")
    mkrepo = Track $ Git.repo git

-------------------------------------------------------------------------------
data Spec
  = DemoSpec { specCollatz :: Int }
  | BuildSpec { specBuildName :: Text }
  deriving (Generic)
instance FromJSON Spec
instance ToJSON Spec

program :: Track' Spec
program = Track $ \spec -> optimizedDeps $ op "program" (deps $ specOp spec) id
  where
   specOp :: Spec -> [Op]
   specOp (DemoSpec k) =  demoOps k
   specOp (BuildSpec k) =  buildOps k
  
   optimizedDeps :: Op -> Op
   optimizedDeps base = let pkgs = Debian.installAllDebsAtOnce base in base `inject` pkgs

-------------------------------------------------------------------------------
data Seed
  = DemoSeed { seedCollatzNum :: Int }
  | BuildSeed { seedBuildName :: Text }

instance ParseRecord Seed where
  parseRecord =
      combo <**> helper
    where
      combo =
        subparser $ mconcat
          [ command "demo" (info demo (progDesc "runs demo"))
          , command "build" (info builds (progDesc "runs builds"))
          ]
      demo = DemoSeed <$> option auto (long "n-collatz")
      builds = BuildSeed <$> strArgument (Options.Applicative.help "build-name")

configure :: Configure' Seed Spec
configure = Configure $ pure . go 
  where
    go :: Seed -> Spec
    go (DemoSeed k) = DemoSpec k
    go (BuildSeed k) = BuildSpec k

-------------------------------------------------------------------------------
main :: IO ()
main = do
  let desc = fullDesc <> progDesc "A Salmon program." <> header "demonstration examples"
  let opts = info parseRecord desc
  cmd <- execParser opts
  CLI.execCommand configure program cmd
