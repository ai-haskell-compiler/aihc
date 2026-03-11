{- |
Permutations of signals as needed for Fast Fourier transforms.
Most functions are independent of the Signal framework.
We could move them as well to Synthesizer.Basic.
-}
module Synthesizer.Generic.Permutation (
   T,
   apply,
   size,
   transposition,
   skewGrid,
   skewGridInv,
   skewGridCRT,
   skewGridCRTInv,
   multiplicative,
   inverse,
   reverse,
   ) where

import qualified Synthesizer.Basic.NumberTheory as NumberTheory

import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.State.Signal as SigS

import qualified Data.StorableVector.ST.Strict as SVST
import qualified Data.StorableVector as SV

import qualified Algebra.PrincipalIdealDomain as PID

import Prelude hiding (reverse, )



type T = SV.Vector Int

apply ::
   (SigG.Transform sig y) =>
   T -> sig y -> sig y
apply p xs =
   SigG.takeStateMatch xs $
   SigS.map (SigG.index xs) $
   SigS.fromStrictStorableSignal p


size :: T -> Int
size = SV.length


{- |
> inverse (transposition n m) = transposition m n
-}
transposition ::
   Int -> Int -> T
transposition n m =
   fst $ SV.unfoldrN (n*m)
      (\(i,j,k0) -> Just (i,
         case pred k0 of
            0  -> let j1 = j+1 in (j1, j1, m)
            k1 -> (i+n, j, k1)))
      (0,0,m)


{-
In general the inverse of a skewGrid
does not look like even a generalized skewGrid.
E.g. @inverse $ skewGrid 3 4@.
-}
skewGrid ::
   Int -> Int -> T
skewGrid n m =
   let len = n*m
   in  fst $ SV.unfoldrN len
          (\(i0,k0) -> Just (i0,
             let k1 = pred k0
                 i1 = i0+n
             in  if k1==0
                   then (mod (i1+m) len, m)
                   else (mod i1 len, k1)))
          (0,m)

{- |
> inverse (skewGrid n m) == skewGridInv n m

In general the inverse of a skewGrid
cannot be expressed like skewGrid or skewGridCRT.
E.g. @inverse $ skewGrid 3 4@.
-}
skewGridInv ::
   Int -> Int -> T
skewGridInv n m =
   SV.pack $
   map (\k ->
      let Just (i,j) = PID.diophantine k n m
      in  mod i m + mod j n * m) $
   take (n*m) $ iterate (1+) 0

skewGridCRT ::
   Int -> Int -> T
skewGridCRT n m =
   let len = n*m
       (ni,mi) = snd $ PID.extendedGCD n m
   in  fst $ SV.unfoldrN len
          (\(i0,k0) -> Just (i0,
             let k1 = pred k0
                 i1 = i0+ni*n
             in  if k1==0
                   then (mod (i1+mi*m) len, m)
                   else (mod i1 len, k1)))
          (0,m)

skewGridCRTInv ::
   Int -> Int -> T
skewGridCRTInv n m =
   fst $ SV.packN (n*m) $
   map (\k -> mod k m + mod k n * m) $
   iterate (1+) 0


{- |
Beware of 0-based indices stored in the result vector.
-}
multiplicative :: Int -> T
multiplicative ni =
   let n = fromIntegral ni
       gen = NumberTheory.multiplicativeGenerator n
   in  {-
       Since 'gen' is usually 2 or 3,
       the error should occur really only for huge signals.
       -}
       if gen * n > fromIntegral (maxBound :: Int)
         then error "signal too long for Int indexing"
         else fst $ SV.unfoldrN (ni-1)
                 (\x -> Just (x-1, mod (fromInteger gen * x) ni)) 1

{- |
We only need to compute the inverse permutation explicitly,
because not all signal structures support write to arbitrary indices,
thus Generic.Write does not support it.
For strict StorableVector it would be more efficient
to build the vector directly.

It holds:

> inverse . inverse == id
-}
inverse :: T -> T
inverse perm =
   SVST.runSTVector
      (do inv <- SVST.new_ (SV.length perm)
          SigS.sequence_ $
             SigS.zipWith (SVST.write inv)
                (SigS.fromStrictStorableSignal perm)
                (SigS.iterate (1+) 0)
          return inv)

reverse :: T -> T
reverse perm =
   fst $ SV.unfoldrN (SV.length perm)
      (\mn -> Just $
         case mn of
            Nothing -> (SV.head perm, Just $ SV.length perm)
            Just n ->
               let n1 = n-1
               in  (SV.index perm n1, Just n1))
      Nothing
