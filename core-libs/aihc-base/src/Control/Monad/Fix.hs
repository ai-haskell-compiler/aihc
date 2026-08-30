module Control.Monad.Fix
  ( MonadFix (..),
  )
where

import Prelude (Monad)

class (Monad m) => MonadFix m where
  mfix :: (a -> m a) -> m a
