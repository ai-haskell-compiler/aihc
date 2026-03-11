{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Synthesizer.Generic.Analysis where

import qualified Synthesizer.Plain.Analysis as Ana
import qualified Synthesizer.State.Analysis as AnaS

import qualified Synthesizer.Generic.Signal as SigG

import qualified Algebra.Algebraic             as Algebraic
import qualified Algebra.Field                 as Field
import qualified Algebra.RealRing              as RealRing
import qualified Algebra.Ring                  as Ring

import qualified Algebra.NormedSpace.Maximum   as NormedMax
import qualified Algebra.NormedSpace.Euclidean as NormedEuc
import qualified Algebra.NormedSpace.Sum       as NormedSum

import NumericPrelude.Numeric
import NumericPrelude.Base


{- * Notions of volume -}

{- |
Volume based on Manhattan norm.
-}
volumeMaximum :: (RealRing.C y, SigG.Read sig y) => sig y -> y
volumeMaximum =
   AnaS.volumeMaximum . SigG.toState

{- |
Volume based on Energy norm.
-}
volumeEuclidean :: (Algebraic.C y, SigG.Read sig y) => sig y -> y
volumeEuclidean =
   AnaS.volumeEuclidean . SigG.toState

volumeEuclideanSqr :: (Field.C y, SigG.Read sig y) => sig y -> y
volumeEuclideanSqr =
   AnaS.volumeEuclideanSqr . SigG.toState

{- |
Volume based on Sum norm.
-}
volumeSum :: (Field.C y, RealRing.C y, SigG.Read sig y) => sig y -> y
volumeSum =
   AnaS.volumeSum . SigG.toState



{- |
Volume based on Manhattan norm.
-}
volumeVectorMaximum ::
   (NormedMax.C y yv, Ord y, SigG.Read sig yv) =>
   sig yv -> y
volumeVectorMaximum =
   AnaS.volumeVectorMaximum . SigG.toState

{- |
Volume based on Energy norm.
-}
volumeVectorEuclidean ::
   (Algebraic.C y, NormedEuc.C y yv, SigG.Read sig yv) =>
   sig yv -> y
volumeVectorEuclidean =
   AnaS.volumeVectorEuclidean . SigG.toState

volumeVectorEuclideanSqr ::
   (Field.C y, NormedEuc.Sqr y yv, SigG.Read sig yv) =>
   sig yv -> y
volumeVectorEuclideanSqr =
   AnaS.volumeVectorEuclideanSqr . SigG.toState

{- |
Volume based on Sum norm.
-}
volumeVectorSum ::
   (NormedSum.C y yv, Field.C y, SigG.Read sig yv) =>
   sig yv -> y
volumeVectorSum =
   AnaS.volumeVectorSum . SigG.toState




{- |
Compute minimum and maximum value of the stream the efficient way.
Input list must be non-empty and finite.
-}
bounds :: (Ord y, SigG.Read sig y) => sig y -> (y,y)
bounds =
   AnaS.bounds . SigG.toState




{- * Miscellaneous -}

{-
histogram:
    length x = sum (histogramDiscrete x)

    units:
    1) histogram (amplify k x) = timestretch k (amplify (1/k) (histogram x))
    2) histogram (timestretch k x) = amplify k (histogram x)
    timestretch: k -> (s -> V) -> (k*s -> V)
    amplify:     k -> (s -> V) -> (s -> k*V)
    histogram:   (a -> b) -> (a^ia*b^ib -> a^ja*b^jb)
    x:           (s -> V)
    1) => (s^ia*(k*V)^ib -> s^ja*(k*V)^jb)
              = (s^ia*V^ib*k -> s^ja*V^jb/k)
       => ib=1, jb=-1
    2) => ((k*s)^ia*V^ib -> (k*s)^ja*V^jb)
              = (s^ia*V^ib -> s^ja*V^jb*k)
       => ia=0, ja=1
    histogram:   (s -> V) -> (V -> s/V)
histogram':
    integral (histogram' x) = integral x
    histogram' (amplify k x) = timestretch k (histogram' x)
    histogram' (timestretch k x) = amplify k (histogram' x)
     -> this does only apply if we slice the area horizontally
        and sum the slice up at each level,
        we must also restrict to the positive values,
        this is not quite the usual histogram
-}

{-
{- |
Input list must be finite.
List is scanned twice, but counting may be faster.
-}
histogramDiscreteArray :: sig Int -> (Int, sig Int)
histogramDiscreteArray [] =
   (error "histogramDiscreteArray: no bounds found", [])
histogramDiscreteArray x =
   let hist =
          accumArray (+) zero
             (bounds x) (attachOne x)
   in  (fst (Array.bounds hist), Array.elems hist)


{- |
Input list must be finite.
If the input signal is empty, the offset is @undefined@.
List is scanned twice, but counting may be faster.
The sum of all histogram values is one less than the length of the signal.
-}
histogramLinearArray :: RealField.C y => sig y -> (Int, sig y)
histogramLinearArray [] =
   (error "histogramLinearArray: no bounds found", [])
histogramLinearArray [x] = (floor x, [])
histogramLinearArray x =
   let (xMin,xMax) = bounds x
       hist =
          accumArray (+) zero
             (floor xMin, floor xMax)
             (meanValues x)
   in  (fst (Array.bounds hist), Array.elems hist)

{- |
Input list must be finite.
If the input signal is empty, the offset is @undefined@.
List is scanned once, counting may be slower.
-}
histogramDiscreteIntMap :: sig Int -> (Int, sig Int)
histogramDiscreteIntMap [] =
   (error "histogramDiscreteIntMap: no bounds found", [])
histogramDiscreteIntMap x =
   let hist = IntMap.fromListWith (+) (attachOne x)
   in  case IntMap.toAscList hist of
          [] -> error "histogramDiscreteIntMap: the list was non-empty before processing ..."
          fAll@((fIndex,fHead):fs) -> (fIndex, fHead :
              concat (zipWith
                 (\(i0,_) (i1,f1) -> replicate (i1-i0-1) zero ++ [f1])
                 fAll fs))

histogramLinearIntMap :: RealField.C y => sig y -> (Int, sig y)
histogramLinearIntMap [] =
   (error "histogramLinearIntMap: no bounds found", [])
histogramLinearIntMap [x] = (floor x, [])
histogramLinearIntMap x =
   let hist = IntMap.fromListWith (+) (meanValues x)
   -- we can rely on the fact that the keys are contiguous
       (startKey:_, elems) = unzip (IntMap.toAscList hist)
   in  (startKey, elems)
   -- This doesn't work, due to a bug in IntMap of GHC-6.4.1
   -- in  (head (IntMap.keys hist), IntMap.elems hist)
-}

{-
The bug in IntMap GHC-6.4.1 is:

*Synthesizer.Plain.Analysis> IntMap.keys $ IntMap.fromList $ [(0,0),(-1,-1::Int)]
[0,-1]
*Synthesizer.Plain.Analysis> IntMap.elems $ IntMap.fromList $ [(0,0),(-1,-1::Int)]
[0,-1]
*Synthesizer.Plain.Analysis> IntMap.assocs $ IntMap.fromList $ [(0,0),(-1,-1::Int)]
[(0,0),(-1,-1)]

The bug has gone in IntMap as shipped with GHC-6.6.
-}

{-
histogramIntMap :: (RealField.C y, SigG.Read sig y) =>
   y -> sig y -> (Int, sig Int)
histogramIntMap binsPerUnit =
   histogramDiscreteIntMap . quantize binsPerUnit

quantize :: (RealField.C y, SigG.Transform sig y) =>
   y -> sig y -> sig Int
quantize binsPerUnit = SigG.map (floor . (binsPerUnit*))

attachOne :: (Sample.C i) => sig i -> sig (i,Int)
attachOne = SigG.map (\i -> (i,one))

meanValues ::
   (RealField.C y, SigG.Read sig y) => sig y -> [(Int,y)]
meanValues x = concatMap spread (zip x (tail x))

spread ::
   (RealField.C y, SigG.Read sig y) => (y,y) -> [(Int,y)]
spread lr0 =
   let (l,r) = sortPair lr0
       (li,lf) = splitFraction l
       (ri,rf) = splitFraction r
       k = recip (r-l)
       nodes =
          (li,k*(1-lf)) :
          zip [li+1 ..] (replicate (ri-li-1) k) ++
          (ri, k*rf) :
          []
   in  if li==ri
         then [(li,one)]
         else nodes
-}

{- |
Requires finite length.
This is identical to the arithmetic mean.
-}
directCurrentOffset ::
   (Field.C y, SigG.Read sig y) => sig y -> y
directCurrentOffset = average


scalarProduct ::
   (Ring.C y, SigG.Read sig y) => sig y -> sig y -> y
scalarProduct xs ys =
   AnaS.scalarProduct (SigG.toState xs) (SigG.toState ys)

{- |
'directCurrentOffset' must be non-zero.
-}
centroid :: (Field.C y, SigG.Read sig y) => sig y -> y
centroid =
   AnaS.centroid . SigG.toState

average :: (Field.C y, SigG.Read sig y) => sig y -> y
average =
   AnaS.average . SigG.toState

rectify :: (RealRing.C y, SigG.Transform sig y) => sig y -> sig y
rectify = SigG.map abs

{- |
Detects zeros (sign changes) in a signal.
This can be used as a simple measure of the portion
of high frequencies or noise in the signal.
It ca be used as voiced\/unvoiced detector in a vocoder.

@zeros x !! n@ is @True@ if and only if
@(x !! n >= 0) \/= (x !! (n+1) >= 0)@.
The result will be one value shorter than the input.
-}
zeros :: (Ord y, Ring.C y, SigG.Transform sig y, SigG.Transform sig Bool) =>
   sig y -> sig Bool
zeros =
   SigG.mapAdjacent (/=) . SigG.map (>=zero)



{- |
Detect thresholds with a hysteresis.
-}
flipFlopHysteresis ::
   (Ord y, SigG.Transform sig y, SigG.Transform sig Ana.BinaryLevel) =>
   (y,y) -> Ana.BinaryLevel -> sig y -> sig Ana.BinaryLevel
flipFlopHysteresis bnds = SigG.scanL (Ana.flipFlopHysteresisStep bnds)

{- |
Almost naive implementation of the chirp transform,
a generalization of the Fourier transform.

More sophisticated algorithms like Rader, Cooley-Tukey, Winograd, Prime-Factor may follow.
-}
chirpTransform ::
   (SigG.Write sig y, Ring.C y) =>
   SigG.LazySize -> y -> sig y -> sig y
chirpTransform size z =
   SigG.fromState size .
   AnaS.chirpTransform z .
   SigG.toState
