-- https://www.youtube.com/watch?v=qzOQOmmkKEM&feature=emb_logo

module Salmon.Reporter (
    Reporter,
    ReporterM (..),
    silent,
    reportIf,
    reportBoth,
    reportPick,

    -- * common utilities
    reportPrint,
    reportHPrint,
    reportHPut,
    encodeJSON,
    pulls,

    -- * re-exports
    Contravariant (..),
    Divisible (..),
    Decidable (..),
) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (ByteString, hPut)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import System.IO (Handle, hPrint)

type Reporter = ReporterM IO

newtype ReporterM m a = ReporterM {runReporter :: (a -> m ())}

instance Contravariant (ReporterM m) where
    contramap f (ReporterM g) = ReporterM (g . f)

instance (Applicative m) => Divisible (ReporterM m) where
    conquer = silent
    divide = reportSplit

instance (Applicative m) => Decidable (ReporterM m) where
    lose _ = silent
    choose = reportPick

-- | Disable Tracing.
{-# INLINE silent #-}
silent :: (Applicative m) => ReporterM m a
silent = ReporterM (const $ pure ())

{- | Splits a reporter into two chunks that are run sequentially.

This name can be confusing but it has to be thought backwards for Contravariant logging:
We compose a target reporter from two reporters but we split the content of the report.

Note that the split function may actually duplicate inputs (that's how reportBoth works).
-}
{-# INLINEABLE reportSplit #-}
reportSplit :: (Applicative m) => (c -> (a, b)) -> ReporterM m a -> ReporterM m b -> ReporterM m c
reportSplit split (ReporterM f1) (ReporterM f2) = ReporterM (go . split)
  where
    go (b, c) = f1 b *> f2 c

{- | If you are given two reporters and want to pass both.
Composition occurs in sequence.
-}
{-# INLINEABLE reportBoth #-}
reportBoth :: (Applicative m) => ReporterM m a -> ReporterM m a -> ReporterM m a
reportBoth t1 t2 = reportSplit (\x -> (x, x)) t1 t2

{- | Picks a reporter based on the emitted object.
Example logic that can be built is reportIf that silent messages.
-}
{-# INLINEABLE reportPick #-}
reportPick :: (Applicative m) => (c -> Either a b) -> ReporterM m a -> ReporterM m b -> ReporterM m c
reportPick split (ReporterM f1) (ReporterM f2) = ReporterM $ \a ->
    let e = split a
     in either f1 f2 e

-- | Filter by dynamically testing values.
{-# INLINEABLE reportIf #-}
reportIf :: forall m a. (Applicative m) => (a -> Bool) -> ReporterM m a -> ReporterM m a
reportIf predicate t = reportPick f silent t
  where
    f :: a -> Either () a
    f x = if predicate x then Right x else Left ()

-- | A reporter that prints emitted events.
reportPrint :: (MonadIO m, Show a) => ReporterM m a
reportPrint = ReporterM (liftIO . print)

-- | A reporter that prints emitted to some handle.
reportHPrint :: (MonadIO m, Show a) => Handle -> ReporterM m a
reportHPrint handle = ReporterM (liftIO . hPrint handle)

-- | A reporter that puts some ByteString to some handle.
reportHPut :: (MonadIO m) => Handle -> ReporterM m ByteString
reportHPut handle = ReporterM (liftIO . hPut handle)

-- | A conversion encoding values to JSON.
{-# INLINE encodeJSON #-}
encodeJSON :: (ToJSON a) => ReporterM m ByteString -> ReporterM m a
encodeJSON = contramap encode

{- | Pulls a value to complete a report when a report occurs.

This function allows to combines pushed values with pulled values.  Hence,
performing some scheduling between behaviours.
Typical usage would be to annotate a report with a background value, or perform
data augmentation in a pipelines of reports.

Note that if you rely on this function you need to pay attention of the
blocking effect of 'pulls': the reported value c is not forwarded until a
value b is available.
-}
{-# INLINE pulls #-}
pulls :: (Monad m) => (c -> m b) -> ReporterM m b -> ReporterM m c
pulls act (ReporterM f1) = ReporterM $ act >=> f1
