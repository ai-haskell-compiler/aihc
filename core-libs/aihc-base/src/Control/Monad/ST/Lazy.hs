module Control.Monad.ST.Lazy
  ( ST,
    strictToLazyST,
  )
where

import GHC.Base (Applicative (..), Functor (..), Monad (..))
import GHC.ST qualified as Strict

newtype ST s a = LazyST (Strict.ST s a)

instance Functor (ST s) where
  fmap function (LazyST action) = LazyST (fmap function action)

instance Applicative (ST s) where
  pure value = LazyST (pure value)
  LazyST function <*> LazyST argument = LazyST (function <*> argument)

instance Monad (ST s) where
  LazyST action >>= continuation =
    LazyST
      ( action >>= \value ->
          case continuation value of
            LazyST result -> result
      )
  LazyST first >> LazyST second = LazyST (first >> second)
  return = pure

strictToLazyST :: Strict.ST s a -> ST s a
strictToLazyST = LazyST
