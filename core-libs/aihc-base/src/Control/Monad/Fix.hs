module Control.Monad.Fix
  ( MonadFix (..),
    fix,
  )
where

import Data.Function (fix)
import Prelude (Either (..), Maybe (..), Monad, errorWithoutStackTrace, head, tail, (.))

class (Monad m) => MonadFix m where
  mfix :: (a -> m a) -> m a

-- These instances are the same as in GHC base. Data.List.NonEmpty has the
-- NonEmpty instance, because it imports this module.

instance MonadFix Maybe where
  mfix f = let a = f (unJust a) in a
    where
      unJust (Just x) = x
      unJust Nothing = errorWithoutStackTrace "mfix Maybe: Nothing"

instance MonadFix [] where
  mfix f = case fix (f . head) of
    [] -> []
    (x : _) -> x : mfix (tail . f)

instance MonadFix ((->) r) where
  mfix f r = let a = f a r in a

instance MonadFix (Either e) where
  mfix f = let a = f (unRight a) in a
    where
      unRight (Right x) = x
      unRight (Left _) = errorWithoutStackTrace "mfix Either: Left"
