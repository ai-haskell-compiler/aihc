{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Synthesizer.State.Analysis (
   volumeMaximum,
   volumeEuclidean,
   volumeEuclideanSqr,
   volumeSum,
   volumeVectorMaximum,
   volumeVectorEuclidean,
   volumeVectorEuclideanSqr,
   volumeVectorSum,
   bounds,
   histogramDiscreteArray,
   histogramLinearArray,
   histogramDiscreteIntMap,
   histogramLinearIntMap,
   histogramIntMap,
   directCurrentOffset,
   scalarProduct,
   centroid,
   centroidRecompute,
   firstMoment,
   average,
   averageRecompute,
   rectify,
   zeros,
   flipFlopHysteresis,
   chirpTransform,
   ) where

import qualified Synthesizer.Plain.Analysis as Ana
import qualified Synthesizer.State.Control as Ctrl
import qualified Synthesizer.State.Signal  as Sig

import qualified Algebra.Algebraic             as Algebraic
import qualified Algebra.RealField             as RealField
import qualified Algebra.Field                 as Field
import qualified Algebra.RealRing              as RealRing
import qualified Algebra.Absolute              as Absolute
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import qualified Algebra.NormedSpace.Maximum   as NormedMax
import qualified Algebra.NormedSpace.Euclidean as NormedEuc
import qualified Algebra.NormedSpace.Sum       as NormedSum

import qualified Data.IntMap as IntMap
import qualified Data.Array as Array

import Data.Array (accumArray)

import NumericPrelude.Numeric
import NumericPrelude.Base


{- * Notions of volume -}

{- |
Volume based on Manhattan norm.
-}
{-# INLINE volumeMaximum #-}
volumeMaximum :: (RealRing.C y) => Sig.T y -> y
volumeMaximum =
   Sig.foldL max zero . rectify
--   maximum . rectify

{- |
Volume based on Energy norm.
-}
{-# INLINE volumeEuclidean #-}
volumeEuclidean :: (Algebraic.C y) => Sig.T y -> y
volumeEuclidean =
   Algebraic.sqrt . volumeEuclideanSqr

{-# INLINE volumeEuclideanSqr #-}
volumeEuclideanSqr :: (Field.C y) => Sig.T y -> y
volumeEuclideanSqr =
   average . Sig.map sqr

{- |
Volume based on Sum norm.
-}
{-# INLINE volumeSum #-}
volumeSum :: (Field.C y, Absolute.C y) => Sig.T y -> y
volumeSum = average . rectify



{- |
Volume based on Manhattan norm.
-}
{-# INLINE volumeVectorMaximum #-}
volumeVectorMaximum :: (NormedMax.C y yv, Ord y) => Sig.T yv -> y
volumeVectorMaximum =
   Sig.foldL max zero . Sig.map NormedMax.norm
--   NormedMax.norm
--   maximum . Sig.map NormedMax.norm

{- |
Volume based on Energy norm.
-}
{-# INLINE volumeVectorEuclidean #-}
volumeVectorEuclidean :: (Algebraic.C y, NormedEuc.C y yv) => Sig.T yv -> y
volumeVectorEuclidean =
   Algebraic.sqrt . volumeVectorEuclideanSqr

{-# INLINE volumeVectorEuclideanSqr #-}
volumeVectorEuclideanSqr :: (Field.C y, NormedEuc.Sqr y yv) => Sig.T yv -> y
volumeVectorEuclideanSqr =
   average . Sig.map NormedEuc.normSqr

{- |
Volume based on Sum norm.
-}
{-# INLINE volumeVectorSum #-}
volumeVectorSum :: (NormedSum.C y yv, Field.C y) => Sig.T yv -> y
volumeVectorSum =
   average . Sig.map NormedSum.norm




{- |
Compute minimum and maximum value of the stream the efficient way.
Input list must be non-empty and finite.
-}
{-# INLINE bounds #-}
bounds :: (Ord y) => Sig.T y -> (y,y)
bounds =
   Sig.switchL
      (error "Analysis.bounds: List must contain at least one element.")
      (\x xs ->
          Sig.foldL (\(minX,maxX) y -> (min y minX, max y maxX)) (x,x) xs)



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

{- |
Input list must be finite.
List is scanned twice, but counting may be faster.
-}
{-# INLINE histogramDiscreteArray #-}
histogramDiscreteArray :: Sig.T Int -> (Int, Sig.T Int)
histogramDiscreteArray =
   withAtLeast1 "histogramDiscreteArray" $ \ x ->
   let hist =
          accumArray (+) zero
             (bounds x) (attachOne x)
   in  (fst (Array.bounds hist), Sig.fromList (Array.elems hist))


{- |
Input list must be finite.
If the input signal is empty, the offset is @undefined@.
List is scanned twice, but counting may be faster.
The sum of all histogram values is one less than the length of the signal.
-}
{-# INLINE histogramLinearArray #-}
histogramLinearArray :: RealField.C y => Sig.T y -> (Int, Sig.T y)
histogramLinearArray =
   withAtLeast2 "histogramLinearArray" $ \ x ->
   let (xMin,xMax) = bounds x
       hist =
          accumArray (+) zero
             (floor xMin, floor xMax)
             (meanValues x)
   in  (fst (Array.bounds hist), Sig.fromList (Array.elems hist))

{- |
Input list must be finite.
If the input signal is empty, the offset is @undefined@.
List is scanned once, counting may be slower.
-}
{-# INLINE histogramDiscreteIntMap #-}
histogramDiscreteIntMap :: Sig.T Int -> (Int, Sig.T Int)
histogramDiscreteIntMap =
   withAtLeast1 "histogramDiscreteIntMap" $ \ x ->
   let hist = IntMap.fromListWith (+) (attachOne x)
   in  case IntMap.toAscList hist of
          [] -> error "histogramDiscreteIntMap: the list was non-empty before processing ..."
          fAll@((fIndex,fHead):fs) -> (fIndex,
              Sig.fromList $
              fHead :
              concat (zipWith
                 (\(i0,_) (i1,f1) -> replicate (i1-i0-1) zero ++ [f1])
                 fAll fs))

{-# INLINE histogramLinearIntMap #-}
histogramLinearIntMap :: RealField.C y => Sig.T y -> (Int, Sig.T y)
histogramLinearIntMap =
   withAtLeast2 "histogramLinearIntMap" $ \ x ->
   let hist = IntMap.fromListWith (+) (meanValues x)
   -- we can rely on the fact that the keys are contiguous
       (startKey:_, elems) = unzip (IntMap.toAscList hist)
   in  (startKey, Sig.fromList elems)
   -- This doesn't work, due to a bug in IntMap of GHC-6.4.1
   -- in  (head (IntMap.keys hist), IntMap.elems hist)

{-# INLINE withAtLeast1 #-}
withAtLeast1 ::
   String ->
   (Sig.T y -> (Int, Sig.T y)) ->
   Sig.T y ->
   (Int, Sig.T y)
withAtLeast1 name f x =
   maybe
      (error (name ++ ": no bounds found"), Sig.empty)
      (const (f x)) $
   Sig.viewL x

{-# INLINE withAtLeast2 #-}
withAtLeast2 :: (RealRing.C y) =>
   String ->
   (Sig.T y -> (Int, Sig.T y)) ->
   Sig.T y ->
   (Int, Sig.T y)
withAtLeast2 name f x =
   maybe
      (error (name ++ ": no bounds found"), Sig.empty)
      (\(y,ys) ->
           if Sig.null ys
             then (floor y, Sig.empty)
             else f x) $
   Sig.viewL x

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

{-# INLINE histogramIntMap #-}
histogramIntMap :: (RealField.C y) => y -> Sig.T y -> (Int, Sig.T Int)
histogramIntMap binsPerUnit =
   histogramDiscreteIntMap . quantize binsPerUnit

{-# INLINE quantize #-}
quantize :: (RealRing.C y) => y -> Sig.T y -> Sig.T Int
quantize binsPerUnit = Sig.map (floor . (binsPerUnit*))

{-# INLINE attachOne #-}
attachOne :: Sig.T i -> [(i,Int)]
attachOne = Sig.toList . Sig.map (\i -> (i,one))

{-# INLINE meanValues #-}
meanValues :: RealField.C y => Sig.T y -> [(Int,y)]
meanValues = concatMap Ana.spread . Sig.toList . Sig.mapAdjacent (,)

{- |
Requires finite length.
This is identical to the arithmetic mean.
-}
{-# INLINE directCurrentOffset #-}
directCurrentOffset :: Field.C y => Sig.T y -> y
directCurrentOffset = average


{-# INLINE scalarProduct #-}
scalarProduct :: Ring.C y => Sig.T y -> Sig.T y -> y
scalarProduct xs ys =
   Sig.sum (Sig.zipWith (*) xs ys)

{- |
'directCurrentOffset' must be non-zero.
-}
{-# INLINE centroid #-}
centroid :: Field.C y => Sig.T y -> y
centroid =
   uncurry (/) .
   Sig.sum .
   Sig.zipWith
      (\k x -> (k*x, x))
      (Sig.iterate (one+) zero)

centroidRecompute :: Field.C y => Sig.T y -> y
centroidRecompute xs =
   firstMoment xs / Sig.sum xs

{-# INLINE firstMoment #-}
firstMoment :: Field.C y => Sig.T y -> y
firstMoment xs =
   scalarProduct (Sig.iterate (one+) zero) xs


{-# INLINE average #-}
average :: Field.C y => Sig.T y -> y
average =
   uncurry (/) .
   Sig.sum .
   Sig.map (flip (,) one)

averageRecompute :: Field.C y => Sig.T y -> y
averageRecompute x =
   Sig.sum x / fromIntegral (Sig.length x)

{-# INLINE rectify #-}
rectify :: Absolute.C y => Sig.T y -> Sig.T y
rectify = Sig.map abs

{- |
Detects zeros (sign changes) in a signal.
This can be used as a simple measure of the portion
of high frequencies or noise in the signal.
It ca be used as voiced\/unvoiced detector in a vocoder.

@zeros x !! n@ is @True@ if and only if
@(x !! n >= 0) \/= (x !! (n+1) >= 0)@.
The result will be one value shorter than the input.
-}
{-# INLINE zeros #-}
zeros :: (Ord y, Additive.C y) => Sig.T y -> Sig.T Bool
zeros =
   Sig.mapAdjacent (/=) . Sig.map (>=zero)



{- |
Detect thresholds with a hysteresis.
-}
{-# INLINE flipFlopHysteresis #-}
flipFlopHysteresis :: (Ord y) =>
   (y,y) -> Ana.BinaryLevel -> Sig.T y -> Sig.T Ana.BinaryLevel
flipFlopHysteresis bnds = Sig.scanL (Ana.flipFlopHysteresisStep bnds)

{- |
Almost naive implementation of the chirp transform,
a generalization of the Fourier transform.

More sophisticated algorithms like Rader, Cooley-Tukey, Winograd, Prime-Factor may follow.
-}
{-# INLINE chirpTransform #-}
chirpTransform :: Ring.C y =>
   y -> Sig.T y -> Sig.T y
chirpTransform z xs =
   Sig.map (scalarProduct xs) $
   Sig.map (\zn -> Ctrl.curveMultiscaleNeutral (*) zn one) $
   Ctrl.curveMultiscaleNeutral (*) z one
