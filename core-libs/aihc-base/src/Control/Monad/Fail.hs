module Control.Monad.Fail
  ( MonadFail (..),
  )
where

import Prelude (IO, List, Maybe (..), Monad, String, error)

class (Monad m) => MonadFail m where
  fail :: String -> m a

instance MonadFail IO where
  fail = error

instance MonadFail List where
  fail _ = []

instance MonadFail Maybe where
  fail _ = Nothing
