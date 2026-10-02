{- | A sub-graph that can only be built once a value is known, where the
value is something another node's @up@ produces.

An 'Op' is built before any @up@ runs. So when what a node needs is picked by
somebody else during the pass -- the address GCP hands out when a reservation
is created, the name servers of a zone, a port a service chose -- there is no
value to build the node from at declaration, and the usual answer is two
passes with a driver carrying the value across.

'deferred' is the other answer: the node that is declared holds a /read/ and
a /recipe/, and its @up@ does the read, builds the sub-graph from what it
found, and runs that as a nested walk. Put it after the node that produces
the value (with 'Salmon.Op.OpGraph.inject') and one pass does both.

It is the shape "SreBox.PostgresTemplate" and "SreBox.PostgresMigrations"
already use for a build that must not be an ordinary dependency, with the
graph a function of something read first. What it costs is the same, and
worth knowing before reaching for it:

* __The sub-graph is opaque.__ @run tree@, @run dag@, @query@, a
  'Salmon.Op.Rewrite.Rewrite', @run serve@'s per-node state and the status
  sink all see one node. Its nodes are not deduplicated against the outer
  graph's and take no part in its ordering or failure containment beyond
  this node failing as a whole.
* __Put only what needs the value in it.__ @down@ tears the whole sub-graph
  down, so a node that is also declared outside it (a project, an API, the
  very node that produces the value) would be taken down here, under
  everything else still standing on it. Declare those outside and 'inject'
  them into this node instead.
* __The read runs on every @up@ and every @down@__, never at declaration.
* __No @check@.__ The node answers 'Salmon.Actions.UpDown.Immaterial': its
  @up@ always runs the nested walk, which asks each inner node's own
  @check@, so a converged sub-graph costs its checks and nothing else. Under
  @run serve@ that means the node parks after its pass and the inner nodes
  are not tended.
-}
module Salmon.Builtin.Nodes.Deferred (
    Deferred (..),
    deferred,
    Report (..),
    Unresolved (..),
    NestedWalkFailed (..),
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Control.Monad.Identity (runIdentity)
import Data.Dynamic (toDyn)
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Salmon.Actions.Dot as Dot
import qualified Salmon.Actions.UpDown as UpDown
import Salmon.Builtin.Extension
import Salmon.Op.Ref (mkRef)
import Salmon.Reporter

data Report
    = -- | the read answered; the sub-graph is about to be walked up.
      ResolvedUp !Text
    | -- | the read answered; the sub-graph is about to be walked down.
      ResolvedDown !Text
    | -- | a @down@ whose read found nothing: there is nothing this node can
      -- name, so nothing is torn down.
      NothingToTearDown !Text
    | Nested !Text !(UpDown.Report Extension)
    deriving (Show)

-- | The read found nothing when the sub-graph was to be brought up.
newtype Unresolved = Unresolved Text
    deriving (Show)

instance Exception Unresolved

-- | The nested walk reported a failure; the outer one learns it from this.
data NestedWalkFailed = NestedWalkFailed !Text !Text
    deriving (Show)

instance Exception NestedWalkFailed

data Deferred a = Deferred
    { deferredName :: Text
    -- ^ identifies the declaration: the 'Salmon.Op.Ref.Ref' key.
    , deferredHelp :: Text
    -- ^ what the sub-graph does, for @run tree@ (which cannot look inside).
    , deferredRead :: IO (Maybe a)
    -- ^ the value, read when the node runs. 'Nothing' when it is not there
    -- (yet): an @up@ then fails, naming 'deferredName', rather than build a
    -- graph from a guess; a @down@ does nothing.
    , deferredGraph :: a -> Op
    -- ^ everything that needs the value, and nothing that does not.
    }

{- | The node. It has no dependencies of its own: 'Salmon.Op.OpGraph.inject'
the node whose @up@ makes 'deferredRead' answer, and whatever else the
sub-graph stands on.

A @down@ whose read finds nothing is a no-op that says so
('NothingToTearDown'), not a failure. The common case is tearing down a
graph that never came up, where there is indeed nothing; the uncommon one is
a value removed behind salmon's back, and then nothing here could name what
to remove anyway. Either way a failure would only block the teardown of the
nodes underneath.
-}
deferred :: Reporter Report -> Deferred a -> Op
deferred r d =
    op "deferred" nodeps $ \actions ->
        actions
            { help = d.deferredHelp
            , notes = ["built and walked at up, from a value read then"]
            , ref = mkRef "deferred" d.deferredName
            , up = do
                found <- d.deferredRead
                case found of
                    Nothing -> throwIO (Unresolved d.deferredName)
                    Just a -> do
                        runReporter r (ResolvedUp d.deferredName)
                        ok <- UpDown.upTree nested (pure . runIdentity) (d.deferredGraph a)
                        unless ok (throwIO (NestedWalkFailed d.deferredName "up"))
            , down = do
                found <- d.deferredRead
                case found of
                    Nothing -> runReporter r (NothingToTearDown d.deferredName)
                    Just a -> do
                        runReporter r (ResolvedDown d.deferredName)
                        ok <- UpDown.downTree nested (pure . runIdentity) (d.deferredGraph a)
                        unless ok (throwIO (NestedWalkFailed d.deferredName "down"))
            , dynamics = [toDyn (Dot.OpaqueNode ("deferred " <> Text.take 40 d.deferredName))]
            }
  where
    nested = contramap (Nested d.deferredName) r
