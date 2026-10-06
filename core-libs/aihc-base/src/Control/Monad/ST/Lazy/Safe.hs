-- | The lazy state-thread monad. This module has the same exports as
-- "Control.Monad.ST.Lazy".
module Control.Monad.ST.Lazy.Safe
  ( ST,
    runST,
    strictToLazyST,
    lazyToStrictST,
    RealWorld,
    stToIO,
  )
where

import Control.Monad.ST.Lazy (RealWorld, ST, lazyToStrictST, runST, stToIO, strictToLazyST)
