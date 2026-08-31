module Control.Monad.ST.Lazy
  ( ST,
    strictToLazyST,
  )
where

import GHC.IO (ST)

strictToLazyST :: ST s a -> ST s a
strictToLazyST action = action
