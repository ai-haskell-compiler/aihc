{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, MultiWayIf, RecordWildCards, ScopedTypeVariables, TupleSections #-}

-- | This package provides combinators to construct many variants of
-- binary search.  Most generally, it provides the binary search over
-- predicate of the form @('Eq' b, 'Monad' m) => a -> m b@. The other
-- searches are derived as special cases of this function.
--
-- 'BinarySearch' assumes two things;
--
-- 1. @b@, the codomain of 'PredicateM' belongs to type class 'Eq'.
--
-- 2. Each value of @b@ form a convex set in the codomain space of the
-- PredicateM. That is, if for certain pair @(left, right) :: (a, a)@
-- satisfies @pred left == val && pred right == val@, then also @pred
-- x == val@ for all @x@ such that @left <= x <= right@.
--
-- __Example 1.__ Find the approximate square root of 3.
--
-- >>> largest  True  $ search positiveExponential divForever (\x -> x^2 < 3000000)
-- Just 1732
-- >>> smallest False $ search positiveExponential divForever (\x -> x^2 < 3000000)
-- Just 1733
-- >>> largest  True  $ search positiveExponential divideForever (\x -> x^2 < (3::Double))
-- Just 1.7320508075688772
-- >>> largest  True  $ search positiveExponential (divideTill 0.125) (\x -> x^2 < (3::Double))
-- Just 1.625
-- >>> smallest False $ search positiveExponential (divideTill 0.125) (\x -> x^2 < (3::Double))
-- Just 1.75
--
-- Pay attention to use the appropriate exponential search combinator to set up the initial search range.
-- For example, the following code works as expected:
--
-- >>> largest  True  $ search nonNegativeExponential divideForever (\x -> x^2 < (0.5::Double))
-- Just 0.7071067811865475
--
-- But this one does not terminate:
--
-- @
-- __> largest  True  $ search positiveExponential divideForever (\x -> x^2 < (0.5::Double))__
-- ... runs forever ...
-- @
--
-- __Example 2.__ Find the range of integers whose quotinent 7 is equal to 6.
--
-- This is an example of how we can 'search' for discete and multi-valued predicate.
--
-- >>> smallest 6 $ search (fromTo 0 100) divForever (\x -> x `div` 7)
-- Just 42
-- >>> largest  6 $ search (fromTo 0 100) divForever (\x -> x `div` 7)
-- Just 48
--
-- __Example 3.__ Find the minimum size of the container that can fit three bars of length 4,
-- and find an actual way to fit them.
--
-- We will solve this using a satisfiability modulo theory (SMT) solver. Since we need to evoke 'IO'
-- to call for the SMT solver,
-- This is a usecase for a monadic binary search.
--
-- >>> import Data.List (isPrefixOf)
-- >>> :{
-- do
--   -- x fits within the container
--   let x ⊂ r = (0 .<= x &&& x .<= r-4)
--   -- x and y does not collide
--   let x ∅ y = (x+4 .<= y )
--   let contain3 :: Integer -> IO (Evidence () String)
--       contain3 r' = do
--         let r = fromInteger r' :: SInteger
--         ret <- show <$> sat (\x y z -> bAnd [x ⊂ r, y ⊂ r, z ⊂ r, x ∅ y, y ∅ z])
--         if "Satisfiable" `isPrefixOf` ret
--           then return $ Evidence ret
--           else return $ CounterEvidence ()
--   Just sz  <- smallest evidence <$> searchM positiveExponential divForever contain3
--   putStrLn $ "Size of the container: " ++ show sz
--   Just msg <- evidenceForSmallest <$> searchM positiveExponential divForever contain3
--   putStrLn msg
-- :}
-- Size of the container: 12
-- Satisfiable. Model:
--   s0 = 0 :: Integer
--   s1 = 4 :: Integer
--   s2 = 8 :: Integer

module Numeric.Search where

import           Control.Applicative
import           Data.Functor.Identity
import           Data.Maybe (fromJust, listToMaybe)
import           Prelude hiding (init, pred)

-- $setup
-- All the doctests in this document assume:
--
-- >>> :set -XFlexibleContexts
-- >>> import Data.SBV


-- * Evidence

-- | The 'Evidence' datatype is similar to 'Either' , but differes in that all 'CounterEvidence' values are
--   equal to each other, and all 'Evidence' values are also
--   equal to each other. The 'Evidence' type is used to binary-searching for some predicate and meanwhile returning evidences for that.
--
-- In other words, 'Evidence' is a 'Bool' with additional information why it is 'True' or 'False'.
--
-- >>> Evidence "He owns the gun" == Evidence "He was at the scene"
-- True
-- >>> Evidence "He loved her" == CounterEvidence "He loved her"
-- False

data Evidence a b = CounterEvidence a | Evidence b
                  deriving (Show, Read, Functor)

instance Eq (Evidence b a) where
  CounterEvidence _ == CounterEvidence _ = True
  Evidence _        == Evidence _        = True
  _                 == _                 = False

instance Ord (Evidence b a) where
  CounterEvidence _ `compare` CounterEvidence _ = EQ
  Evidence _        `compare` Evidence _        = EQ
  CounterEvidence _ `compare` Evidence _        = GT
  Evidence _        `compare` CounterEvidence _ = LT

instance Applicative (Evidence e) where
    pure                     = Evidence
    CounterEvidence  e <*> _ = CounterEvidence e
    Evidence f <*> r         = fmap f r

instance Monad (Evidence e) where
    return                   = Evidence
    CounterEvidence  l >>= _ = CounterEvidence l
    Evidence r >>= k         = k r

-- | 'evidence' = 'Evidence' 'undefined'. We can use this combinator to look up for some 'Evidence',
-- since all 'Evidence's are equal.
evidence :: Evidence a b
evidence = Evidence undefined

-- | 'counterEvidence' = 'CounterEvidence' 'undefined'. We can use this combinator to look up for any 'CounterEvidence',
-- since all 'CounterEvidence's are equal.
counterEvidence :: Evidence a b
counterEvidence = CounterEvidence undefined



-- * Search range


-- | The @Range k lo  k' hi@ represents the search result that @pred x == k@ for @lo <= x <= hi@.
-- The 'Range' type also holds the evidences for the lower and the upper boundary.

data Range b a = Range {loKey :: b, loVal :: a, hiKey :: b, hiVal :: a}
                 deriving (Show, Read, Eq, Ord)

-- | The (possibly infinite) lists of candidates for lower and upper bounds from which the search may be started.
type SearchRange a = ([a], [a])


-- | Set the lower and upper boundary from those available from the candidate lists.
-- From the pair of list, the @initializeSearchM@ tries to find the first @(lo, hi)@
-- such that @pred lo /= pred hi@, by the breadth-first search.

initializeSearchM :: (Monad m, Eq b)=> SearchRange a -> (a -> m b) -> m [Range b a]
initializeSearchM (lo:los,hi:his) pred0 = do
  pLo <- pred0 lo
  pHi <- pred0 hi
  let
    pop (p,x, []) = return (p,x,[])
    pop (p,_, x2:xs) = do
      p2 <- pred0 x2
      return (p2, x2, xs)

    go pez1@(p1,x1,xs1) pez2@(p2,x2,xs2)
        | p1 /= p2             = return [Range p1 x1 p1 x1, Range p2 x2 p2 x2]
        | null xs1 && null xs2 = return [Range p1 x1 p2 x2]
        | otherwise = do
            pez1' <- pop pez1
            pez2' <- pop pez2
            go pez1' pez2'

  go (pLo, lo,los) (pHi, hi, his)
initializeSearchM _ _ = return []


-- | Start binary search from between 'minBound' and 'maxBound'.
minToMax :: Bounded a => SearchRange a
minToMax = ([minBound], [maxBound])

-- | Start binary search from between the given pair of boundaries.
fromTo :: a -> a -> SearchRange a
fromTo x y= ([x], [y])

-- | Exponentially search for lower boundary from @[-1, -2, -4, -8, ...]@, upper boundary from @[0, 1, 2, 4, 8, ...]@.
-- Move on to the binary search once the first @(lo, hi)@ is found
-- such that @pred lo /= pred hi@.
exponential :: Num a => SearchRange a
exponential = (iterate (*2) (-1), 0 : iterate (*2) 1)

-- | Lower boundary is 1, upper boundary is from @[2, 4, 8, 16, ...]@.
positiveExponential :: Num a => SearchRange a
positiveExponential = ([1], iterate (*2) 2)

-- | Lower boundary is 0, search upper boundary is from @[1, 2, 4, 8, ...]@.
nonNegativeExponential :: Num a => SearchRange a
nonNegativeExponential = ([0], iterate (*2) 1)

-- | Lower boundary is @[0.5, 0.25, 0.125, ...]@, upper boundary is from @[1, 2, 4, 8, ...]@.
positiveFractionalExponential :: Fractional a => SearchRange a
positiveFractionalExponential = (iterate (/2) 0.5, iterate (*2) 1)

-- | Lower boundary is from @[-2, -4, -8, -16, ...]@, upper boundary is -1.
negativeExponential :: Num a => SearchRange a
negativeExponential = (iterate (*2) (-2), [-1])

-- | Lower boundary is from @[-1, -2, -4, -8, ...]@, upper boundary is -0.
nonPositiveExponential :: Num a => SearchRange a
nonPositiveExponential = (iterate (*2) (-1), [0])

-- | Lower boundary is @[-1, -2, -4, -8, ...]@, upper boundary is from @[-0.5, -0.25, -0.125, ...]@.
negativeFractionalExponential :: Fractional a => SearchRange a
negativeFractionalExponential = (iterate (*2) (-1), iterate (/2) (-0.5))


-- * Splitters

-- | The type of function that returns a value between @(lo, hi)@ as long as one is available.
type Splitter a = a -> a -> Maybe a

-- | Perform split forever, until we cannot find a mid-value because @hi-lo < 2@.
-- This splitter assumes that the arguments are Integral, and uses the `div` funtion.
--
-- Note that our dividing algorithm always find the mid value for any  @hi-lo >= 2@.
--
-- >>> prove $ \x y -> (y .>= x+2 &&& x+2 .> x) ==> let z = (x+1) `sDiv` 2 + y `sDiv` 2  in x .< z &&& z .< (y::SInt32)
-- Q.E.D.

divForever :: Integral a => Splitter a
divForever lo hi = let mid = (lo+1) `div` 2 + hi `div` 2 in
  if lo == mid || mid == hi then Nothing
  else Just mid

-- | Perform splitting until @hi - lo <= eps@.
divTill :: Integral a => a -> Splitter a
divTill eps lo hi
  | hi - lo <= eps = Nothing
  | otherwise      = divForever lo hi

-- | Perform split forever, until we cannot find a mid-value due to machine precision.
-- This one uses `(/)` operator.
divideForever :: (Eq a,Fractional a) => Splitter a
divideForever lo hi = let mid = lo / 2 + hi / 2 in
  if lo == mid || mid == hi then Nothing
  else Just mid

-- | Perform splitting until @hi - lo <= eps@.
divideTill :: (Ord a, Fractional a) => a -> Splitter a
divideTill eps lo hi
  | hi - lo <= eps = Nothing
  | otherwise      = divideForever lo hi


-- * Searching

-- | Perform search over pure predicates. The monadic version of this is 'searchM'.
search :: (Eq b) =>
          SearchRange a -> Splitter a -> (a -> b) -> [Range b a]
search init0 split0 pred0 = runIdentity $ searchM init0 split0 (Identity . pred0)


-- | Mother of all search variations.
--
-- 'searchM' keeps track of the predicates found, so that it works well with the 'Evidence' type.

searchM :: forall a m b . (Functor m, Monad m, Eq b) =>
           SearchRange a -> Splitter a -> (a -> m b) -> m [Range b a]
searchM init0 split0 pred0 = do
  ranges0 <- initializeSearchM init0 pred0
  go ranges0
    where
      go :: [Range b a] -> m [Range b a]
      go (r1@(Range p0 lo1 p1 hi1):r2@(Range p2 lo2 p3 hi2):rest) = case split0 hi1 lo2 of
        Nothing   -> (r1:) <$> go (r2:rest)
        Just mid1 -> do
          pMid <- pred0 mid1
          if | p1 == pMid -> go $ (Range p0 lo1 pMid mid1) : r2 : rest
             | p2 == pMid -> go $ r1 : (Range pMid mid1 p3 hi2) : rest
             | otherwise  -> go $ r1 : (Range pMid mid1 pMid mid1) : r2 : rest
      go xs = return xs

-- * Postprocess

-- | Look up for the first 'Range' with the given predicate.
lookupRanges :: (Eq b) => b -> [Range b a] -> Maybe (Range b a)
lookupRanges k [] = Nothing
lookupRanges k (r@Range{..}:rs)
  | loKey == k  = Just r
  | otherwise   = lookupRanges k rs

-- | Pick up the smallest @a@ that satisfies @pred a == b@.
smallest :: (Eq b) => b -> [Range b a] -> Maybe a
smallest b rs = loVal <$> lookupRanges b rs

-- | Pick up the largest @a@ that satisfies @pred a == b@.
largest :: (Eq b) => b -> [Range b a] -> Maybe a
largest b rs = hiVal <$> lookupRanges b rs

-- | Get the content of the evidence for the smallest @a@ which satisfies @pred a@.
evidenceForSmallest :: [Range (Evidence b1 b2) a] -> Maybe b2
evidenceForSmallest rs = listToMaybe [e | Evidence e <- map loKey rs]

-- | Get the content of the evidence for the largest @a@ which satisfies @pred a@.
evidenceForLargest :: [Range (Evidence b1 b2) a] -> Maybe b2
evidenceForLargest rs = listToMaybe [e | Evidence e <- map hiKey rs]

-- | Get the content of the counterEvidence for the smallest @a@ which does not satisfy @pred a@.
counterEvidenceForSmallest :: [Range (Evidence b1 b2) a] -> Maybe b1
counterEvidenceForSmallest rs = listToMaybe [e | CounterEvidence e <- map loKey rs]

-- | Get the content of the counterEvidence for the largest @a@ which does not satisfy @pred a@.
counterEvidenceForLargest :: [Range (Evidence b1 b2) a] -> Maybe b1
counterEvidenceForLargest rs = listToMaybe [e | CounterEvidence e <- map hiKey rs]
