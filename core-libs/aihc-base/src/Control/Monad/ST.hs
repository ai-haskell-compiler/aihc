module Control.Monad.ST
  ( ST,
    RealWorld,
    runST,
    fixST,
    stToIO,
  )
where

import GHC.IO (stToIO)
import GHC.Prim (RealWorld)
import GHC.Prim.MonadFix (MonadFix (..))
import GHC.ST (ST, runST)

-- | Give the computation its own result, as a lazy value.
fixST :: (a -> ST s a) -> ST s a
fixST = mfix
