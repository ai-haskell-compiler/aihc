{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Synthesizer.Plain.Analysis (
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
   centroidAlt,
   firstMoment,
   average,
   rectify,
   zeros,

   BinaryLevel(Low, High),
   binaryLevelFromBool,
   binaryLevelToNumber,
   flipFlopHysteresis,
   flipFlopHysteresisStep,
   chirpTransform,
   binarySign,
   deltaSigmaModulation,
   deltaSigmaModulationPositive,

   -- for testing
   spread,
   ) where

import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.Plain.Control as Ctrl
import qualified Synthesizer.Plain.Filter.Recursive.Integration as Integration

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Array as Array
import qualified Data.IntMap as IntMap
import Data.Tuple.HT (sortPair)
import Data.Array (accumArray)
import Data.List (foldl', )

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

import NumericPrelude.Numeric
import NumericPrelude.Base


{- * Notions of volume -}

{- |
Volume based on Manhattan norm.
-}
volumeMaximum :: (RealRing.C y) => Sig.T y -> y
volumeMaximum =
   foldl max zero . rectify
--   maximum . rectify

{- |
Volume based on Energy norm.
-}
volumeEuclidean :: (Algebraic.C y) => Sig.T y -> y
volumeEuclidean =
   Algebraic.sqrt . volumeEuclideanSqr

volumeEuclideanSqr :: (Field.C y) => Sig.T y -> y
volumeEuclideanSqr =
   average . map sqr

{- |
Volume based on Sum norm.
-}
volumeSum :: (Absolute.C y, Field.C y) => Sig.T y -> y
volumeSum = average . rectify



{- |
Volume based on Manhattan norm.
-}
volumeVectorMaximum :: (NormedMax.C y yv, Ord y) => Sig.T yv -> y
volumeVectorMaximum =
   NormedMax.norm
--   maximum . map NormedMax.norm

{- |
Volume based on Energy norm.
-}
volumeVectorEuclidean :: (Algebraic.C y, NormedEuc.C y yv) => Sig.T yv -> y
volumeVectorEuclidean =
   Algebraic.sqrt . volumeVectorEuclideanSqr

volumeVectorEuclideanSqr :: (Field.C y, NormedEuc.Sqr y yv) => Sig.T yv -> y
volumeVectorEuclideanSqr =
   average . map NormedEuc.normSqr

{- |
Volume based on Sum norm.
-}
volumeVectorSum :: (NormedSum.C y yv, Field.C y) => Sig.T yv -> y
volumeVectorSum =
   average . map NormedSum.norm




{- |
Compute minimum and maximum value of the stream the efficient way.
Input list must be non-empty and finite.
-}
bounds :: Ord y => NonEmpty.T Sig.T y -> (y,y)
bounds (NonEmpty.Cons x xs) =
   foldl' (\(minX,maxX) y -> (min y minX, max y maxX)) (x,x) xs




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
histogramDiscreteArray :: NonEmpty.T Sig.T Int -> (Int, Sig.T Int)
histogramDiscreteArray x =
   let hist =
          accumArray (+) zero
             (bounds x) (attachOne $ NonEmpty.flatten x)
   in  (fst (Array.bounds hist), Array.elems hist)


{- |
Input list must be finite.
If the input signal is empty, the offset is @undefined@.
List is scanned twice, but counting may be faster.
The sum of all histogram values is one less than the length of the signal.
-}
histogramLinearArray :: RealField.C y => NonEmpty.T Sig.T y -> (Int, Sig.T y)
histogramLinearArray (NonEmpty.Cons x []) = (floor x, [])
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
histogramDiscreteIntMap :: NonEmpty.T Sig.T Int -> (Int, Sig.T Int)
histogramDiscreteIntMap x =
   let hist = IntMap.fromListWith (+) (attachOne $ NonEmpty.flatten x)
   in  case IntMap.toAscList hist of
          [] -> error "histogramDiscreteIntMap: the list was non-empty before processing ..."
          fAll@((fIndex,fHead):fs) -> (fIndex, fHead :
              concat (zipWith
                 (\(i0,_) (i1,f1) -> replicate (i1-i0-1) zero ++ [f1])
                 fAll fs))

histogramLinearIntMap :: RealField.C y => NonEmpty.T Sig.T y -> (Int, Sig.T y)
histogramLinearIntMap (NonEmpty.Cons x []) = (floor x, [])
histogramLinearIntMap x =
   let hist = IntMap.fromListWith (+) (meanValues x)
   -- we can rely on the fact that the keys are contiguous
       (startKey:_, elems) = unzip (IntMap.toAscList hist)
   in  (startKey, elems)
   -- This doesn't work, due to a bug in IntMap of GHC-6.4.1
   -- in  (head (IntMap.keys hist), IntMap.elems hist)

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

histogramIntMap :: (RealField.C y) => y -> NonEmpty.T Sig.T y -> (Int, Sig.T Int)
histogramIntMap binsPerUnit =
   histogramDiscreteIntMap . quantize binsPerUnit

quantize :: (Functor f, RealField.C y) => y -> f y -> f Int
quantize binsPerUnit = fmap (floor . (binsPerUnit*))

attachOne :: Sig.T i -> Sig.T (i,Int)
attachOne = map (\i -> (i,one))

meanValues :: RealField.C y => NonEmpty.T Sig.T y -> [(Int,y)]
meanValues = concatMap spread . NonEmpty.mapAdjacent (,)

spread :: RealField.C y => (y,y) -> [(Int,y)]
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

{- |
Requires finite length.
This is identical to the arithmetic mean.
-}
directCurrentOffset :: Field.C y => Sig.T y -> y
directCurrentOffset = average


scalarProduct :: Ring.C y => Sig.T y -> Sig.T y -> y
scalarProduct xs ys =
   sum (zipWith (*) xs ys)

{- |
'directCurrentOffset' must be non-zero.
-}
centroid :: Field.C y => Sig.T y -> y
centroid xs =
   firstMoment xs / sum xs

centroidAlt :: Field.C y => Sig.T y -> y
centroidAlt xs =
   sum (scanr (+) zero (tail xs)) / sum xs

firstMoment :: Ring.C y => Sig.T y -> y
firstMoment =
   scalarProduct (iterate (one+) zero)


average :: Field.C y => Sig.T y -> y
average x =
   sum x / fromIntegral (length x)

rectify :: Absolute.C y => Sig.T y -> Sig.T y
rectify = map abs

{- |
Detects zeros (sign changes) in a signal.
This can be used as a simple measure of the portion
of high frequencies or noise in the signal.
It ca be used as voiced\/unvoiced detector in a vocoder.

@zeros x !! n@ is @True@ if and only if
@(x !! n >= 0) \/= (x !! (n+1) >= 0)@.
The result will be one value shorter than the input.
-}
zeros :: (Ord y, Ring.C y) => Sig.T y -> Sig.T Bool
zeros xs =
   let signs = map (>=zero) xs
   in  zipWith (/=) signs (tail signs)



data BinaryLevel = Low | High
   deriving (Eq, Show, Enum)

binaryLevelFromBool :: Bool -> BinaryLevel
binaryLevelFromBool False = Low
binaryLevelFromBool True  = High

binaryLevelToNumber :: Ring.C a => BinaryLevel -> a
binaryLevelToNumber Low  = negate one
binaryLevelToNumber High =        one


{- |
Detect thresholds with a hysteresis.
-}
flipFlopHysteresis :: (Ord y) =>
   (y,y) -> BinaryLevel -> Sig.T y -> Sig.T BinaryLevel
flipFlopHysteresis bnds = scanl (flipFlopHysteresisStep bnds)

flipFlopHysteresisStep :: Ord a => (a, a) -> BinaryLevel -> a -> BinaryLevel
flipFlopHysteresisStep (lower,upper) =
   \state x ->
      binaryLevelFromBool $
         case state of
            High -> not(x<lower)
            Low  -> x>upper

{- |
Almost naive implementation of the chirp transform,
a generalization of the Fourier transform.

More sophisticated algorithms like Rader, Cooley-Tukey, Winograd, Prime-Factor may follow.
-}
chirpTransform :: Ring.C y =>
   y -> Sig.T y -> Sig.T y
chirpTransform z xs =
   map (scalarProduct xs) $
   map (\zn -> Ctrl.curveMultiscaleNeutral (*) zn one) $
   Ctrl.curveMultiscaleNeutral (*) z one


binarySign ::
   (Ord y, Additive.C y) => Sig.T y -> Sig.T BinaryLevel
binarySign =
   map (binaryLevelFromBool . (zero <=))

{- |
A kind of discretization for signals with sample values between -1 and 1.
If you smooth the resulting signal
(after you transformed with 'map binaryLevelToNumber'),
you should obtain an approximation to the input signal.
-}
deltaSigmaModulation ::
   RealRing.C y => Sig.T y -> Sig.T BinaryLevel
deltaSigmaModulation x =
   let y = binarySign (Integration.run (x - (zero : map binaryLevelToNumber y)))
   in  y
{-
   let y = binarySign (Integration.runInit zero (x - map binaryLevelToNumber y))
   in  y
-}

{- |
A kind of discretization for signals with sample values between 0 and a threshold.
We accumulate input values and emit a threshold value
whenever the accumulator exceeds the threshold.
This is intended for generating clicks from input noise.

See also 'deltaSigmaModulation'.
-}
deltaSigmaModulationPositive ::
   RealRing.C y => y -> Sig.T y -> Sig.T y
deltaSigmaModulationPositive threshold x =
   let y =
          map (\xi -> if xi>=threshold then threshold else zero) $
          Integration.run (x - (zero:y))
   in  y
