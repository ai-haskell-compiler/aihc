{- |
Very simple random number generator
which should be fast and should suffice for generating just noise.
<http://www.softpanorama.org/Algorithms/random_generators.shtml>
-}
module Synthesizer.RandomKnuth (T, cons, ) where

import qualified System.Random as R


newtype T = Cons Int
   deriving Show


{-# INLINE cons #-}
cons :: Int -> T
cons = Cons


{-# INLINE factor #-}
factor :: Int
factor = 40692

{-# INLINE modulus #-}
modulus :: Int
modulus = 2147483399 -- 2^31-249

{-
We have to split the 32 bit integer in order to avoid overflow on multiplication.
'split' must be chosen, such that 'splitRem' is below 2^16.
-}
{-# INLINE split #-}
split :: Int
split = succ $ div modulus factor

{-# INLINE splitRem #-}
splitRem :: Int
splitRem = split * factor - modulus


instance R.RandomGen T where
   {-# INLINE next #-}
   next (Cons s) =
      -- efficient computation of @mod (s*factor) modulus@ without Integer
      let (sHigh, sLow) = divMod s split
      in  (s, Cons $ flip mod modulus $
                   splitRem*sHigh + factor*sLow)
   {-# INLINE split #-}
   split (Cons s) = (Cons (s*s), Cons (13+s))
   {-# INLINE genRange #-}
   genRange _ = (1, pred modulus)
{-
*Main> let s = 10000000000 in (next (Cons s), mod (fromIntegral s * fromIntegral factor) (fromIntegral modulus) :: Integer)
((1410065408,Cons 1920127854),1920127854)
-}
