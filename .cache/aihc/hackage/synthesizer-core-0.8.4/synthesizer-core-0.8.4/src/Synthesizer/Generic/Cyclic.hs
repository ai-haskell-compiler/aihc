module Synthesizer.Generic.Cyclic where

import qualified Synthesizer.Generic.Filter.NonRecursive as FiltNRG
import qualified Synthesizer.Generic.Analysis as AnaG
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.State.Signal as Sig

import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


fromSignal ::
   (SigG.Write sig yv, Additive.C yv) =>
   SigG.LazySize -> Int -> sig yv -> sig yv
fromSignal chunkSize n =
   {- almost Sig.sum -}
   Sig.foldL SigG.mix (SigG.replicate chunkSize n zero) .
   CutG.sliceVertical n

reverse ::
   (SigG.Transform sig y) =>
   sig y -> sig y
reverse sig =
   SigG.switchL sig
      (\y ys -> SigG.cons y (SigG.reverse ys)) sig


{- |
It must hold @n <= CutG.length x@.
-}
reperiodize ::
   (SigG.Transform sig yv, Additive.C yv) =>
   Int -> sig yv -> sig yv
reperiodize n =
   {- Sig.sum -}
   Sig.foldL SigG.mix CutG.empty .
   CutG.sliceVertical n

{- |
length of the input signals must be equal
-}
convolve ::
   (SigG.Transform sig y, Ring.C y) =>
   sig y -> sig y -> sig y
convolve x y =
   reperiodize (CutG.length x) $
   FiltNRG.karatsubaFinite (*) x y



{- |
The size of both input signals must be equal.

Could be optimized by computing only first (length x) elements.
-}
filterNaive ::
   (SigG.Transform sig y, Ring.C y) =>
   sig y -> sig y -> sig y
filterNaive x y =
   SigG.takeStateMatch y $
   SigG.toState $
   SigG.mapTails
      (AnaG.scalarProduct x)
      (SigG.append y y)

convolveNaive ::
   (SigG.Transform sig y, Ring.C y) =>
   sig y -> sig y -> sig y
convolveNaive x y =
   SigG.takeStateMatch y $
   SigG.toState $
   SigG.mapTails
      (AnaG.scalarProduct (SigG.reverse x))
      (SigG.laxTail $ SigG.append y y)


{-
Some small size convolutions using the Karatsuba trick.
We do not use Toom-3 multiplication,
because this requires division by 2 and 6.

In principle we could implement them
by calling the corresponding functions in Filter.NonRecursive
and periodize them afterwards.
However the custom implementations below
allow a litte bit more optimization,
namely sharing of some sums.
-}

type Pair y = (y,y)

{-# INLINE convolvePair #-}
convolvePair ::
   (Ring.C y) =>
   Pair y -> Pair y -> Pair y
convolvePair a b =
   snd $ sumAndConvolvePair a b

{-# INLINE sumAndConvolvePair #-}
sumAndConvolvePair ::
   (Ring.C y) =>
   Pair y -> Pair y -> ((y,y), Pair y)
sumAndConvolvePair (a0,a1) (b0,b1) =
   let sa01 = a0+a1
       sb01 = b0+b1
       ab0ab1 = a0*b0+a1*b1
   in  ((sa01, sb01), (ab0ab1, sa01*sb01-ab0ab1))


type Triple y = (y,y,y)

{-# INLINE convolveTriple #-}
convolveTriple ::
   (Ring.C y) =>
   Triple y -> Triple y -> Triple y
convolveTriple a b =
   snd $ sumAndConvolveTriple a b

{-# INLINE sumAndConvolveTriple #-}
sumAndConvolveTriple ::
   (Ring.C y) =>
   Triple y -> Triple y -> ((y,y), Triple y)
sumAndConvolveTriple (a0,a1,a2) (b0,b1,b2) =
   let ab0 = a0*b0
       dab12 = a1*b1 - a2*b2
       sa01 = a0+a1; sb01 = b0+b1; tab01 = sa01*sb01 - ab0
       sa02 = a0+a2; sb02 = b0+b2; tab02 = sa02*sb02 - ab0
       sa012 = sa01+a2
       sb012 = sb01+b2

       d0 = sa012*sb012 - tab01 - tab02
       d1 = tab01 - dab12
       d2 = tab02 + dab12
   in  ((sa012, sb012), (d0, d1, d2))

{-# INLINE sumAndConvolveTripleAlt #-}
sumAndConvolveTripleAlt ::
   (Ring.C y) =>
   Triple y -> Triple y -> ((y,y), Triple y)
sumAndConvolveTripleAlt (a0,a1,a2) (b0,b1,b2) =
   let ab0 = a0*b0
       ab1 = a1*b1
       ab2 = a2*b2
       sa01 = a0+a1; sb01 = b0+b1
       ab01 = sa01*sb01 - (ab0+ab1)
       sa02 = a0+a2; sb02 = b0+b2
       ab02 = sa02*sb02 - (ab0+ab2)
       sa12 = a1+a2; sb12 = b1+b2
       ab12 = sa12*sb12 - (ab1+ab2)
   in  ((sa01+a2, sb01+b2), (ab0+ab12, ab2+ab01, ab1+ab02))


type Quadruple y = (y,y,y,y)

{-# INLINE convolveQuadruple #-}
convolveQuadruple ::
   (Ring.C y) =>
   Quadruple y -> Quadruple y -> Quadruple y
convolveQuadruple a b =
   snd $ sumAndConvolveQuadruple a b

{-# INLINE sumAndConvolveQuadruple #-}
sumAndConvolveQuadruple ::
   (Ring.C y) =>
   Quadruple y -> Quadruple y -> ((y,y), Quadruple y)
sumAndConvolveQuadruple (a0,a1,a2,a3) (b0,b1,b2,b3) =
   let ab0 = a0*b0
       ab1 = a1*b1
       sa01 = a0+a1; sb01 = b0+b1
       ab01 = sa01*sb01 - (ab0+ab1)
       ab2 = a2*b2
       ab3 = a3*b3
       sa23 = a2+a3; sb23 = b2+b3
       ab23 = sa23*sb23 - (ab2+ab3)
       c0 = ab0  + ab2 - (ab1 + ab3)
       c1 = ab01 + ab23
       ab02 = (a0+a2)*(b0+b2)
       ab13 = (a1+a3)*(b1+b3)
       sa0123 = sa01+sa23
       sb0123 = sb01+sb23
       ab0123 = sa0123*sb0123 - (ab02+ab13)
       d0 = ab13   + c0
       d1 =          c1
       d2 = ab02   - c0
       d3 = ab0123 - c1
   in  ((sa0123, sb0123), (d0, d1, d2, d3))
