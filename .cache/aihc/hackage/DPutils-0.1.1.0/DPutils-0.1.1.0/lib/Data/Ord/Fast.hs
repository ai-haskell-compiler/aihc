
{-# Language MagicHash #-}
{-# Language ForeignFunctionInterface #-}
{-# Language UnliftedFFITypes #-}
{-# Language GHCForeignImportPrim #-}
{-# Language UnboxedTuples #-}
{-# Language CPP #-}

-- | This module provides a small set of function for extremely fast @max@ and
-- @min@ operations that are branchless.
--
-- This should be temporary, since GHC is supposed to get the branchless variants.
--
-- NOTE these do not seem to be faster anyway.

module Data.Ord.Fast where

import GHC.Exts



class FastMinMax x where
  fastmin :: x -> x -> x
  fastmax :: x -> x -> x
  -- | Clamp values to @>=0@.
  clamp :: x -> x

instance FastMinMax Int where
  fastmin (I# x) (I# y) =
    let !xmy = x -# y
        res  = I# ( y +# ( xmy `andI#` uncheckedIShiftRA# xmy 63# ) )
    in  res
  {-# Inline fastmin #-}
  fastmax (I# x) (I# y) =
    let !xmy  = x -# y
        res  = I# ( x -# ( xmy `andI#` uncheckedIShiftRA# xmy 63# ) )
    in  res
  -- FROM: https://ghc.gitlab.haskell.org/ghc/doc/libraries/ghc-bignum-1.0/src/GHC-Num-Primitives.html#maxI%23
  -- NOTE: slightly slower than the above version
  --fastmax (I# x) (I# y)
  --  | isTrue# (x >=# y) = I# x
  --  | True              = I# y
  {-# Inline fastmax #-}
  clamp (I# x) = I# (andI# x (notI# (uncheckedIShiftRA# x 63#)))
  {-# Inline clamp #-}

