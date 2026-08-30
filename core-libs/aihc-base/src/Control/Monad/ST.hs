module Control.Monad.ST
  ( ST,
    RealWorld,
    runST,
    stToIO,
  )
where

import GHC.IO (stToIO)
import GHC.Prim (RealWorld)
import GHC.ST (ST, runST)
