{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Prim.IO
  ( IO (..),
    MaskingState (..),
    stToIO,
    ST (..),
  )
where

import GHC.Prim (RealWorld, State#)
import GHC.Types (IO (..))

newtype ST s a = ST (State# s -> (# State# s, a #))

stToIO :: ST RealWorld a -> IO a
stToIO (ST action) = IO action

-- | State of asynchronous exception masking.
data MaskingState
  = Unmasked
  | MaskedInterruptible
  | MaskedUninterruptible
