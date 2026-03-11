


module Data.Vector.Split
  ( chunksOf
  , splitPlaces
  , splitPlacesBlanks
  , chop
  , divvy
  , module Data.Vector.Split.Internal
  ) where


import           Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V

import           Data.List (unfoldr)

import           Data.Vector.Split.Internal



-- | @'chunksOf' n@ splits a vector into length-n pieces.  The last
--   piece will be shorter if @n@ does not evenly divide the length of
--   the vector.  If @n <= 0@, @'chunksOf' n l@ returns an infinite list
--   of empty vectors.  For example:
--
--   Note that @'chunksOf' n []@ is @[]@, not @[[]]@.  This is
--   intentional, and is consistent with a recursive definition of
--   'chunksOf'; it satisfies the property that
--
--   @chunksOf n xs ++ chunksOf n ys == chunksOf n (xs ++ ys)@
--
--   whenever @n@ evenly divides the length of @xs@.
chunksOf :: Vector v a => Int -> v a -> [v a]
chunksOf i = unfoldr go
  where go v | V.null v = Nothing
             | otherwise = Just (V.splitAt i v)

-- | Split a vector into chunks of the given lengths. For example:
--
-- > splitPlaces [2,3,4] [1..20] == [[1,2],[3,4,5],[6,7,8,9]]
-- > splitPlaces [4,9] [1..10] == [[1,2,3,4],[5,6,7,8,9,10]]
-- > splitPlaces [4,9,3] [1..10] == [[1,2,3,4],[5,6,7,8,9,10]]
--
--   If the input vector is longer than the total of the given lengths,
--   then the remaining elements are dropped. If the vector is shorter
--   than the total of the given lengths, then the result may contain
--   fewer chunks than requested, and the last chunk may be shorter
--   than requested.
splitPlaces :: Vector v a => [Int] -> v a -> [v a]
splitPlaces is v = unfoldr go (is,v)
  where go ([],_)   = Nothing
        go (x:xs,y) | V.null y = Nothing
                    | otherwise = let (l,r) = V.splitAt x y in Just (l,(xs,r))


-- | Split a vector into chunks of the given lengths. Unlike
--   'splitPlaces', the output list will always be the same length as
--   the first input argument. If the input vector is longer than the
--   total of the given lengths, then the remaining elements are
--   dropped. If the vector is shorter than the total of the given
--   lengths, then the last several chunks will be shorter than
--   requested or empty. For example:
--
-- > splitPlacesBlanks [2,3,4] [1..20] == [[1,2],[3,4,5],[6,7,8,9]]
-- > splitPlacesBlanks [4,9] [1..10] == [[1,2,3,4],[5,6,7,8,9,10]]
-- > splitPlacesBlanks [4,9,3] [1..10] == [[1,2,3,4],[5,6,7,8,9,10],[]]
--
--   Notice the empty list in the output of the third example, which
--   differs from the behavior of 'splitPlaces'.
splitPlacesBlanks :: Vector v a => [Int] -> v a -> [v a]
splitPlacesBlanks is v = unfoldr go (is,v)
  where go ([],_)   = Nothing
        go (x:xs,y) = let (l,r) = V.splitAt x y in Just (l,(xs,r))



-- | A useful recursion pattern for processing a list to produce a new
--   list, often used for \"chopping\" up the input list.  Typically
--   chop is called with some function that will consume an initial
--   prefix of the list and produce a value and the rest of the list.
--
--   For example, many common Prelude functions can be implemented in
--   terms of @chop@:
--
-- > group :: (Eq a) => [a] -> [[a]]
-- > group = chop (\ xs@(x:_) -> span (==x) xs)
-- >
-- > words :: String -> [String]
-- > words = filter (not . null) . chop (span (not . isSpace) . dropWhile isSpace)
chop :: Vector v a => (v a -> (b, v a)) -> v a -> [b]
chop f v | V.null v  = []
         | otherwise = b : chop f v'
            where (b, v') = f v

-- | Divides up an input vector into a set of subvectors, according to 'n' and 'm'
--   input specifications you provide. Each subvector will have 'n' items, and the
--   start of each subvector will be offset by 'm' items from the previous one.
--
-- > divvy 5 5 [1..20] == [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]]
--
--   In the case where a source vector's trailing elements do no fill an entire
--   subvector, those trailing elements will be dropped.
--
-- > divvy 5 2 [1..10] == [[1,2,3,4,5],[3,4,5,6,7],[5,6,7,8,9]]
--
--   As an example, you can generate a moving average over a vector of prices:
-- 
-- > type Prices = [Float]
-- > type AveragePrices = [Float]
-- > 
-- > average :: [Float] -> Float
-- > average xs = sum xs / (fromIntegral $ length xs)
-- > 
-- > simpleMovingAverage :: Prices -> AveragePrices
-- > simpleMovingAverage priceList =
-- >   map average divvyedPrices
-- >     where divvyedPrices = divvy 20 1 priceList
divvy :: Vector v a => Int -> Int -> v a -> [v a]
divvy n m v | V.null v = []
            | otherwise = filter (\ws -> n == V.length ws)
                        . chop (\xs -> (V.take n xs, V.drop m xs))
                        $ v
