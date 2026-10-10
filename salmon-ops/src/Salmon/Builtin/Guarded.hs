{- | Guarding an operation that cannot be undone: a major upgrade, a restart
of the member everything else follows.

The pattern is written here once, as a combinator over a node somebody
already wrote, and is three rules:

1. __It starts only when its preconditions are met.__ They are folded into
   the node's @check@: a node whose effect is not in place and whose
   preconditions are unmet answers 'Salmon.Actions.UpDown.Unknown' ("ran and
   could not tell", the verdict for a transitional state), and
   "Salmon.Actions.Upkeep" keeps asking and starts nothing. They are asked
   again at the top of @up@, which throws 'PreconditionUnmet' rather than
   act, because the one-shot drivers map 'Salmon.Actions.UpDown.Unknown' to
   "run @up@": under @run up@ the operation is refused, reported failed, and
   its dependants are blocked.
2. __A failure parks it.__ The node's
   'Salmon.Op.Supervision.supGiveUpAfter' is set (to one failure unless
   'guardingAttempts' says otherwise), so "Salmon.Actions.Upkeep" stops
   instead of retrying, and only an operator's
   'Salmon.Op.Mailbox.Force' or 'Salmon.Op.Mailbox.Recheck' starts it over.
   A refusal to start (rule 1) is not a failure and is not counted.
3. __Its siblings hold still while it runs.__ 'guardingHolds' names them;
   "Salmon.Actions.Upkeep" pauses each one, waits until they have all
   stopped, runs @up@, and resumes them when the operation completes or is
   parked.

What this does not do, so that nobody assumes it:

* It does not guard 'Salmon.Builtin.Extension.managed'. A node that /is/ a
  running process has no @up@ for a precondition to stand in front of.
* 'Salmon.Op.Mailbox.Force' does not bypass the preconditions: it overrides
  the check, and @up@ asks them again.
* Rules 2 and 3 are "Salmon.Actions.Upkeep"'s. A one-shot pass makes one
  attempt by construction and pauses nobody; there, only an edge orders two
  nodes.
* The park lives in the node's machine, not on disk. See
  @resources\/module-notes.md@ on what that means under @run serve@.
-}
module Salmon.Builtin.Guarded (
    guarded,
    Guarding (..),
    defaultGuarding,

    -- * Re-exported from "Salmon.Op.Guard"
    Precondition (..),
    PreconditionUnmet (..),
    Guard (..),
    guardOf,
    isGuarded,
) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Maybe (isNothing)
import qualified Data.Text as Text

import Salmon.Actions.UpDown (CheckResult (..))
import Salmon.Builtin.Extension (Extension)
import qualified Salmon.Builtin.Extension as Extension
import Salmon.Op.Guard
import Salmon.Op.Ref (Ref)
import Salmon.Op.Supervision (Supervision (..), supervised, supervisionOf)

-- | How an operation is guarded, beyond its preconditions.
data Guarding = Guarding
    { guardingHolds :: [Ref]
    -- ^ the nodes to hold still while the operation runs. See 'Guard'.
    , guardingAttempts :: Int
    -- ^ how many consecutive failures park the node. One, by default: an
    -- operation that cannot be undone is not one to retry on a timer.
    }
    deriving (Show, Eq)

-- | Holds nobody, parks at the first failure.
defaultGuarding :: Guarding
defaultGuarding = Guarding [] 1

{- | Guard a node. Apply it to the one node that is the risky operation, not
to an 'Salmon.Builtin.Extension.Op' through @fmap@ (which reaches its
dependencies too).

The precondition action is run at @check@ and @up@ time, never at graph
build, and what it returns is public. If it throws, the preconditions count
as unmet: a probe that could not be asked is not a reason to start.

The node keeps whatever 'Supervision' it declared, with only
'supGiveUpAfter' replaced.
-}
guarded :: Guarding -> IO Precondition -> Extension -> Extension
guarded g preconditions e =
    e
        { Extension.check = guardedCheck
        , Extension.up = guardedUp
        , Extension.dynamics =
            supervised policy{supGiveUpAfter = Just (max 1 g.guardingAttempts)}
                : toDyn (Guard g.guardingHolds)
                : filter notSupervision (Extension.dynamics e)
        }
  where
    (policy, _) = supervisionOf e

    notSupervision :: Dynamic -> Bool
    notSupervision d = isNothing (fromDynamic d :: Maybe Supervision)

    ask :: IO Precondition
    ask = do
        answer <- try @SomeException preconditions
        case answer of
            Right p -> pure p
            Left err
                | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
                | otherwise -> pure (Unmet ("could not be asked: " <> Text.pack (show err)))

    -- the effect being in place outranks the preconditions: an upgrade that
    -- has happened is done whatever the cluster looks like now.
    guardedCheck :: IO CheckResult
    guardedCheck = do
        verdict <- Extension.check e
        case verdict of
            Success -> pure verdict
            Skipped -> pure verdict
            Completed -> pure verdict
            _ -> do
                p <- ask
                pure $ case p of
                    Met -> verdict
                    Unmet _ -> Unknown

    guardedUp :: IO ()
    guardedUp = do
        p <- ask
        case p of
            Met -> Extension.up e
            Unmet why -> throwIO (PreconditionUnmet why)
