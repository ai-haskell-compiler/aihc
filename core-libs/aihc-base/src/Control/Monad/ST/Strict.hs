-- | The strict state-thread monad. This module has the same exports as
-- "Control.Monad.ST".
module Control.Monad.ST.Strict
  ( ST,
    RealWorld,
    runST,
    stToIO,
  )
where

import Control.Monad.ST (RealWorld, ST, runST, stToIO)
