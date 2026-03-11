module Math.Apportionment (
   largestRemainder,
   largestRemainderScaled,

   highestAveragesScaled,
   dHondtDivisors,
   sainteLagueDivisors,
   ) where

import Control.Functor.HT (outerProduct, )

import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Function.HT (compose2, )
import Data.Tuple.HT (mapSnd, )
import Data.Ord.HT (comparing, )


{- $setup
>>> import Control.Applicative ((<$>))
>>> import qualified Test.QuickCheck as QC
>>>
>>> forAllNonNegatives ::
>>>    (Num a, Ord a, Show a, QC.Arbitrary a) => ([a] -> Bool) -> QC.Property
>>> forAllNonNegatives = QC.forAll $
>>>    (map QC.getNonNegative <$> QC.arbitrary) `QC.suchThat` (\xs -> sum xs > 0)
-}

{- |
Like 'largestRemainder' but result values
are sorted with respect to descending fractional parts of the inputs.
This is an artifact of the used algorithm.
The result still depends on the input order,
especially on the order of numbers with equal fractional part.
-}
_largestRemainderSort :: (RealFrac a) => [a] -> [Int]
_largestRemainderSort xs =
   let (d, intFracs) = fractions xs
       (intUps, intDowns) =
          splitAt d $ map fst $
          List.sortBy (comparing (negate.snd)) intFracs
   in  map (1+) intUps ++ intDowns

-- ToDo: generalize to Traversable

-- ToDo: require NonEmpty

-- ToDo: It would be safer to use Integer

{- |
This function rounds values
such that the sum of the rounded values
matches the rounded sum of the original values.

Also known as Hare-Niemeyer method.
<https://en.wikipedia.org/wiki/Largest_remainder_method>

Input values must be non-negative, otherwise 'properFraction' bites us.

>>> largestRemainder [1,2,3::Rational]
[1,2,3]
>>> largestRemainder [1.1,2.2,3.3,4.4::Rational]
[1,2,3,5]

prop> \xs -> xs == largestRemainder (map fromIntegral xs :: [Rational])
prop> forAllNonNegatives $ \xs -> round (sum xs) == sum (largestRemainder (xs :: [Rational]))
-}
largestRemainder :: (RealFrac a) => [a] -> [Int]
largestRemainder = largestRemainderCore . fractions

{- |
@largestRemainderScaled s xs@
scales and rounds the values in @xs@ such that their sum becomes @s@.

>>> largestRemainderScaled 100 [1,2,3::Rational]
[17,33,50]

That is, it returns integral percentages almost proportional to 1:2:3.

>>> largestRemainderScaled 100 [1,10,100::Rational]
[1,9,90]

prop> forAllNonNegatives $ \xs -> xs == largestRemainderScaled (sum xs) (map fromIntegral xs :: [Rational])
prop> \(QC.Positive s) -> forAllNonNegatives $ \xs -> s == sum (largestRemainderScaled s (xs :: [Rational]))
-}
largestRemainderScaled :: (RealFrac a) => Int -> [a] -> [Int]
largestRemainderScaled s = largestRemainderCore . fractionsScaled s


type Fractions a = (Int, [(Int, a)])

largestRemainderCore :: (RealFrac a) => Fractions a -> [Int]
largestRemainderCore (d, intFracs) =
   let (intUps, intDowns) =
          splitAt d $ map (mapSnd fst) $
          List.sortBy (comparing (negate . snd . snd)) $
          zip [(0::Int) .. ] intFracs
   in  map snd $ List.sortBy (comparing fst) $
       map (mapSnd (1+)) intUps ++ intDowns

fractions :: (RealFrac a) => [a] -> Fractions a
fractions xs =
   let xsum = round $ sum xs
       intFracs = map properFraction xs
       isum = sum $ map fst intFracs
   in  (xsum-isum, intFracs)

fractionsScaled :: (RealFrac a) => Int -> [a] -> Fractions a
fractionsScaled xsum xs =
   let c = fromIntegral xsum / sum xs
       intFracs = map (properFraction . (* c)) xs
       isum = sum $ map fst intFracs
   in  (xsum-isum, intFracs)


{- |
<https://en.wikipedia.org/wiki/Highest_averages_method>

In @highestAveragesScaled divs s xs@,
@divs@ must be an infinite list of strictly increasing positive numbers.
E.g. @highestAveragesScaled dHondtDivisors s xs@ runs the d'Hondt method.

>>> highestAveragesScaled dHondtDivisors 100 [1,2,3::Rational]
[17,33,50]
>>> highestAveragesScaled dHondtDivisors 100 [1,10,100::Rational]
[0,9,91]

>>> highestAveragesScaled sainteLagueDivisors 100 [1,2,3::Rational]
[17,33,50]
>>> highestAveragesScaled sainteLagueDivisors 100 [1,10,100::Rational]
[1,9,90]

prop> forAllNonNegatives $ \xs -> xs == highestAveragesScaled dHondtDivisors (sum xs) (map fromIntegral xs :: [Rational])
prop> forAllNonNegatives $ \xs -> xs == highestAveragesScaled sainteLagueDivisors (sum xs) (map fromIntegral xs :: [Rational])

prop> \(QC.Positive s) -> forAllNonNegatives $ \xs -> s == sum (highestAveragesScaled dHondtDivisors s (xs :: [Rational]))
prop> \(QC.Positive s) -> forAllNonNegatives $ \xs -> s == sum (highestAveragesScaled sainteLagueDivisors s (xs :: [Rational]))
-}
highestAveragesScaled :: (RealFrac a) => [a] -> Int -> [a] -> [Int]
highestAveragesScaled divs s xs =
   let m = Map.fromList $ zip [(0::Int) ..] xs
   in  Map.elems $ flip Map.union (fmap (const 0) m) $
       Map.fromListWith (+) $ map (mapSnd (const 1)) $
       take s $ Fold.foldl (ListHT.mergeBy (compose2 (>=) snd)) [] $
       Map.mapWithKey (map . (,)) $ outerProduct (/) m divs

-- ToDo: use Stream
dHondtDivisors :: Num a => [a]
dHondtDivisors = iterate (1+) 1

sainteLagueDivisors :: Num a => [a]
sainteLagueDivisors = iterate (2+) 1
