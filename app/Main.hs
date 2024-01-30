{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Salmon.Actions.Print
import Salmon.Actions.UpDown
import Salmon.Actions.Check
import Salmon.Actions.Dot
import Salmon.Op.Graph
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

-- | Instanciate actions.
type Actions' = Actions Extension

-- | A helper string.
type Help = Text

-- | Our demo extension.
data Extension = Extension {
    help         :: Help
  , ref          :: Ref
  , up           :: IO ()
  , down         :: IO ()
  , check        :: IO ()
  , notify       :: IO ()
}

instance Semigroup Extension where
  a <> b =
    Extension
      (help a <> "|" <> help b)
      (ref a <> ref b)
      (up a <> up b)
      (down b <> down a)
      (check a <> check b)
      (notify b <> check a)

type Op = OpGraph Identity Actions'

evalDeps :: Op -> Cofree Graph Op
evalDeps = runIdentity . expand

nodeps :: Identity (Graph Op)
nodeps = pure $ Vertices []

deps :: [Op] -> Identity (Graph Op)
deps xs = pure $ Vertices  xs

type Track' a = Track Identity Actions' a

track :: Track' a -> ShortHand -> a -> Op
track track name a = op name (deps [inner]) id
  where
    inner :: Op
    inner = run track a

noop :: ShortHand -> Op
noop short =
  OpGraph
    nodeps
    ( Actions
      $ Act
          short
          $ Extension
              noHelp
              ref
              skip
              skip
              skip
              skip
     )
  where
    noHelp :: Help
    noHelp = "no-help"
    ref :: Ref
    ref = dotRef short

    skip :: IO ()
    skip = pure ()

op :: ShortHand -> Identity (Graph Op) -> (Extension -> Extension) -> Op
op short pred f =
  -- complicated implementation to say that we apply the modifier on Extension on top of a noop
  let baseOp = (noop short) { predecessors = pred }
      baseNode = node baseOp
  in baseOp { node = fmap f baseNode }

-- some direct calls

dnsAccount = op "dns-account" nodeps $ \actions -> actions {
    up = print "create a DNS account"
  }

data Key = Key
data DNSName = DNSName
data Server
  = Server
  { server_hostname :: DNSName
  , server_key :: Key
  }

homeServer :: Server -> Op
homeServer srv =
    op "home-server" (deps [ track server "server-track" srv, ksService ]) id
  where
    server :: Track' Server
    server = adapt >$< hostname >*< tkey 
      where
        adapt (Server h k) = (h,k)

    hostname :: Track' DNSName
    hostname = Track $ (\DNSName -> dnsName)
      where
        dnsName =
          op "dns-name" nodeps $ \actions ->
             actions {
               up = print "make a dns name"
             }
    
    tkey :: Track' Key
    tkey = Track $ (\Key -> key)

    openssl =
      op "open-ssl" nodeps $ \actions -> actions {
        up = print "install open-ssl"
      }

    keydir =
      op "keydir" nodeps id

    key =
      op "key" (deps [ keydir, openssl ]) id

    dnsServer =
      op "dns-server" nodeps $ \actions -> actions {
        up = print "upping DNS server"
      }
    
    dnsEntry =
      op "dns-entry" (deps [ dnsServer ]) $ \actions -> actions {
        up = print "adding DNS entry"
      }
    
    certificate =
      op "cert" (deps [dnsEntry, key]) $ \actions -> actions {
        up = print "generating certificate"
      }
    
    ksService =
      op "ks-service" (deps [ certificate ]) $ \actions -> actions {
        up = print "configuring KS service"
      }

cycleOfRef :: Op
cycleOfRef =
    op "cycle-demo" (deps [ n1, n2 , n3 ]) id
  where
   useSameRef = \actions -> actions { ref = dotRef "circular-ref" }
   n1 = op "demo-node-1" (deps [n2]) useSameRef
   n2 = op "demo-node-2" (deps [n3]) id
   n3 = op "demo-node-3" (deps []) useSameRef

collatz :: Op
collatz =
    op "collatzs-line" (deps [segment k | k <- [3,5,7,11,13]]) id
  where
    opname k = "collatz-line-" <> (Text.pack $ show k)
    useRef k = \actions -> actions { ref = dotRef $ opname k }
    depsAtLevel 1 = nodeps
    depsAtLevel n
      | n `mod` 2 == 0 = deps [ segment (n `div` 2) ]
      | otherwise = deps [ segment (3*n+1) ]
    segment k = op (opname k) (depsAtLevel k) (useRef k)

-- main
main :: IO ()
main = void $ do
  let srv = Server DNSName Key
  let gr0 = homeServer srv `inject` dnsAccount `inject` cycleOfRef `inject` collatz
  let nat = pure . runIdentity
  printHelpTree nat gr0
  upTree nat gr0
  printDigraph nat gr0
