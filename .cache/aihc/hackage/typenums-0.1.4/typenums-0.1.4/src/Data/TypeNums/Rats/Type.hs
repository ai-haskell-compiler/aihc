{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}

module Data.TypeNums.Rats.Type(Rat(..)) where

import GHC.TypeLits(Nat)

-- | Type constructor for a rational
data Rat =
  forall k. k :% Nat

