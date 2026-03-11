{-# LANGUAGE Safe #-}
module Data.Invertible.Internal
  ( invert
  , involution
  ) where

import Data.Invertible.Bijection

-- |Invert an isomorphism by exchanging the two arrows (same as 'Control.Invertible.BiArrow.invert').
invert :: Bijection a b c -> Bijection a c b
invert (f :<->: g) = g :<->: f
{-# INLINE invert #-}

-- |Construct an involution (an bijection where the function and inverse are the same) (overlaps with 'Control.Invertible.BiArrow.involve').
involution :: a b b -> Bijection a b b
involution f = f :<->: f
{-# INLINE involution #-}
