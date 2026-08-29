{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO
  ( IO (..),
    MaskingState (..),
  )
where

import GHC.Prim (RealWorld, State#)

newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

-- | State of asynchronous exception masking.
data MaskingState
  = Unmasked
  | MaskedInterruptible
  | MaskedUninterruptible
