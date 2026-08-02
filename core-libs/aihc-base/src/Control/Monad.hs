module Control.Monad
  ( Monad (..),
    MonadPlus (..),
    (=<<),
  )
where

import Control.Applicative (Alternative (..))
import Prelude (Maybe, Monad (..), (=<<))

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

  mzero = empty
  mplus = (<|>)

instance MonadPlus []

instance MonadPlus Maybe
