module Control.Monad
  ( Monad (..),
    MonadPlus (..),
    ap,
    liftM,
    liftM2,
    (=<<),
  )
where

import Control.Applicative (Alternative (..))
import Prelude (Maybe, Monad (..), (<$>), (=<<))

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap function argument = do
  selected <- function
  selected <$> argument

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM function action = function <$> action

liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 function left right = do
  leftValue <- left
  function leftValue <$> right

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

  mzero = empty
  mplus = (<|>)

instance MonadPlus []

instance MonadPlus Maybe
