{- | A line a node says while it works: the per-node logging event.

A node's 'Salmon.Builtin.Extension.up' is an @IO ()@. Whatever it has to say
before it returns — the output of the build it is running, "waiting for ssh",
"step 2 of 3" — has no way out through that type, and the drivers that call
it report only that it started and how it ended. This module is the way out:
a 'Line' names the node by its 'Ref', the 'Channel' it was said on and one
line of text, and 'emit' hands it to whoever is listening /now/, while the
node is still running.

= Why the listener is process-wide

The listener is installed with 'withSink' and found through one process-wide
registry rather than passed to the node. Passing it would mean a new argument
to every @up@ in the tree (or a new 'Salmon.Builtin.Extension.Extension'
field every driver has to thread), for an event most nodes never emit; and a
recipe builds its nodes from a @directive -> Op@ that knows nothing of which
driver will run them. So the driver's side registers ("Salmon.Builtin.CommandLine"
does, for @run up@, @run down@ and @run serve@) and the node's side calls
'emit' or 'say' with nothing in hand but its own 'Ref'.

Three properties that follow, and are relied on:

* With no sink registered, 'emit' does nothing. A library user who never
  registers one loses nothing they had.
* Several sinks can be registered at once and each sees every line (a test
  suite running in one process is the case); a sink that cares about one
  node filters by 'lineRef'.
* Lines are handed to the sinks one at a time, under one lock, so a sink
  need not be thread-safe against other lines. It is /not/ serialised
  against the drivers' own reports, which have their own locks; a sink that
  writes to a handle should write each line in one call, as the reporters in
  "Salmon.Reporter.Tagged" do.

What is said here is public in the same sense report text is: it is printed,
sent to @\/events@ and to @--json@. A node must not say a secret, and a
command whose output can hold one must not be streamed (see
"Salmon.Builtin.Nodes.Binary").
-}
module Salmon.Builtin.NodeLog (
    Channel (..),
    renderChannel,
    Line (..),
    emit,
    say,
    withSink,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO.Unsafe (unsafePerformIO)

import Salmon.Op.Ref (Ref)
import Salmon.Reporter

-- | Where a line came from.
data Channel
    = -- | the standard output of a command the node ran
      Stdout
    | -- | the standard error of a command the node ran
      Stderr
    | -- | the node's own words (see 'say')
      Message
    deriving (Eq, Ord, Show)

-- | @stdout@, @stderr@ or @message@: the spelling on the wire.
renderChannel :: Channel -> Text
renderChannel Stdout = "stdout"
renderChannel Stderr = "stderr"
renderChannel Message = "message"

-- | One line, about one node.
data Line = Line
    { lineRef :: !Ref
    , lineChannel :: !Channel
    , lineText :: !Text
    -- ^ without its line terminator
    }
    deriving (Eq, Show)

data Registry = Registry
    { registryNext :: !Int
    , registrySinks :: !(Map Int (Reporter Line))
    }

{-# NOINLINE registry #-}
registry :: IORef Registry
registry = unsafePerformIO (newIORef (Registry 0 Map.empty))

{-# NOINLINE emitLock #-}
emitLock :: MVar ()
emitLock = unsafePerformIO (newMVar ())

-- | Hand a line to every sink registered right now.
emit :: Line -> IO ()
emit line = do
    sinks <- registrySinks <$> readIORef registry
    if Map.null sinks
        then pure ()
        else withMVar emitLock $ \_ -> mapM_ (\r -> runReporter r line) (Map.elems sinks)

{- | A node's own progress message: @say myRef "waiting for ssh"@. One call
is one line; text holding newlines is split so that every 'Line' is one.
-}
say :: Ref -> Text -> IO ()
say r txt = mapM_ (emit . Line r Message) (if null ls then [""] else ls)
  where
    ls = Text.lines txt

{- | Register a sink for as long as the action runs. Sinks registered by
other callers are left as they are, and this one is removed on the way out
however the action ends.
-}
withSink :: Reporter Line -> IO a -> IO a
withSink sink act = bracket add remove (const act)
  where
    add = atomicModifyIORef' registry $ \reg ->
        (Registry (reg.registryNext + 1) (Map.insert reg.registryNext sink reg.registrySinks), reg.registryNext)
    remove k = atomicModifyIORef' registry $ \reg ->
        (reg{registrySinks = Map.delete k reg.registrySinks}, ())
