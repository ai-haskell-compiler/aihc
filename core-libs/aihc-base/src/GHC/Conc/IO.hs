{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Conc.IO (registerDelay) where

import GHC.Conc.Sync (TVar (..))
import GHC.IO (IO (..))
import GHC.Prim (newDelayTVar#)
import GHC.Types (Bool (..), Int (..))

registerDelay :: Int -> IO (TVar Bool)
registerDelay (I# delay) =
  IO
    ( \state ->
        case newDelayTVar# delay False True state of
          (# next, variable #) -> (# next, TVar variable #)
    )
