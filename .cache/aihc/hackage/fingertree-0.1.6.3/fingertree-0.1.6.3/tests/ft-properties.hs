{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- QuickCheck properties for Data.FingerTree

module Main where

import Data.FingerTree    -- needs to be compiled with -DTESTING for use here

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck hiding ((><))
import Test.QuickCheck.Poly

import Prelude hiding (null, reverse, foldl, foldl1, foldr, foldr1, all)
import qualified Prelude

import Control.Applicative (Applicative(..))
import Control.Monad (ap)
import Data.Foldable (Foldable(foldMap, foldl, foldr), toList, all)
import Data.Functor ((<$>))
import Data.Traversable (traverse)
import Data.List (inits)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))

main :: IO ()
main = defaultMainWithOpts
    [ testProperty "foldr" prop_foldr
    , testProperty "foldl" prop_foldl
    , testProperty "(==)" prop_equals
    , testProperty "compare" prop_compare
    , testProperty "mappend" prop_mappend
    , testCase "empty" test_empty
    , testProperty "singleton" prop_singleton
    , testProperty "(<|)" prop_cons
    , testProperty "(|>)" prop_snoc
    , testProperty "(><)" prop_append
    , testProperty "fromList" prop_fromList
    , testProperty "null" prop_null
    , testProperty "viewl" prop_viewl
    , testProperty "viewr" prop_viewr
    , testCase "search" test_search
    , testProperty "search" prop_search
    , testProperty "split" prop_split
    , testProperty "takeUntil" prop_takeUntil
    , testProperty "dropUntil" prop_dropUntil
    , testProperty "reverse" prop_reverse
    , testProperty "fmap'" prop_fmap'
    , testProperty "fmapWithPos" prop_fmapWithPos
    , testProperty "fmapWithContext" prop_fmapWithContext
    , testProperty "foldlWithPos" prop_foldlWithPos
    , testProperty "foldlWithContext" prop_foldlWithContext
    , testProperty "foldrWithPos" prop_foldrWithPos
    , testProperty "foldrWithContext" prop_foldrWithContext
    , testProperty "traverse'" prop_traverse'
    , testProperty "traverseWithPos" prop_traverseWithPos
    , testProperty "traverseWithContext" prop_traverseWithContext
    ] runner_opts
  where
    runner_opts = mempty { ropt_test_options = Just test_opts }
    test_opts = mempty {
          topt_maximum_generated_tests = Just 500
        , topt_maximum_unsuitable_generated_tests = Just 500
        }

{--------------------------------------------------------------------
  The general plan is to compare each function with a list equivalent.
  Each operation should produce a valid tree representing the same
  sequence as produced by its list counterpart on corresponding inputs.
  (The list versions are often lazier, but these properties ignore
  strictness.)
--------------------------------------------------------------------}

-- utilities for partial conversions

infix 4 ~=

(~=) :: (Eq a, Eq v, Measured v a, Valid a) => FingerTree v a -> [a] -> Bool
s ~= xs = valid s && toList s == xs

-- Partial conversion of an output sequence to a list.
toList' :: (Eq a, Measured [a] a, Valid a) => Seq a -> Maybe [a]
toList' xs
  | valid xs = Just (toList xs)
  | otherwise = Nothing

-- instances

prop_foldr :: Seq A -> Bool
prop_foldr xs =
    foldr f z xs == Prelude.foldr f z (toList xs)
  where
    f = (:)
    z = []

prop_foldl :: Seq A -> Bool
prop_foldl xs =
    foldl f z xs == Prelude.foldl f z (toList xs)
  where
    f = flip (:)
    z = []

prop_equals :: Seq OrdA -> Seq OrdA -> Bool
prop_equals xs ys =
    (xs == ys) == (toList xs == toList ys)

prop_compare :: Seq OrdA -> Seq OrdA -> Bool
prop_compare xs ys =
    compare xs ys == compare (toList xs) (toList ys)

prop_mappend :: Seq A -> Seq A -> Bool
prop_mappend xs ys =
    mappend xs ys ~= toList xs ++ toList ys

-- * Construction

test_empty :: Assertion
test_empty =
    toList' (empty :: Seq A) @?= Just []

prop_singleton :: A -> Bool
prop_singleton x =
    singleton x ~= [x]

prop_cons :: A -> Seq A -> Bool
prop_cons x xs =
    x <| xs ~= x : toList xs

prop_snoc :: Seq A -> A -> Bool
prop_snoc xs x =
    xs |> x ~= toList xs ++ [x]

prop_append :: Seq A -> Seq A -> Bool
prop_append xs ys =
    xs >< ys ~= toList xs ++ toList ys

prop_fromList :: [A] -> Bool
prop_fromList xs =
    fromList xs ~= xs

-- * Deconstruction

prop_null :: Seq A -> Bool
prop_null xs =
    null xs == Prelude.null (toList xs)

-- ** Examining the ends

prop_viewl :: Seq A -> Bool
prop_viewl xs =
    case viewl xs of
    EmptyL ->   Prelude.null (toList xs)
    x :< xs' -> valid xs' && toList xs == x : toList xs'

prop_viewr :: Seq A -> Bool
prop_viewr xs =
    case viewr xs of
    EmptyR ->   Prelude.null (toList xs)
    xs' :> x -> valid xs' && toList xs == toList xs' ++ [x]

-- ** Search

prop_search :: Int -> Seq A -> Bool
prop_search n xs =
    case search p xs of
        Position _ b _ -> Just b == indexFromEnd n (toList xs)
        OnLeft         -> n >= len || null xs
        OnRight        -> n < 0
        Nowhere        -> error "impossible: the predicate is monotonic"
  where
    p vl vr = Prelude.length vl >= len - n && Prelude.length vr <= n

    len = length xs

    indexFromEnd :: Int -> [a] -> Maybe a
    indexFromEnd i = listToMaybe . drop i . Prelude.reverse

test_search :: Assertion
test_search = do
    lookupByIndexFromEnd xs1 1 @?= Just (A 4)
    lookupByIndexFromEnd xs2 1 @?= Just (A 4)
  where
    xs1 = Deep (map A [1..5]) (Four (A 1) (A 2) (A 3) (A 4)) Empty (One (A 5))
    xs2 = Deep (map A [1..5]) (One (A 1)) Empty (Four (A 2) (A 3) (A 4) (A 5))
    lookupByIndexFromEnd xs n =
        let len = length xs
            p vl vr = Prelude.length vl >= len - n && Prelude.length vr <= n
        in case search p xs of
               Position _ x _ -> Just x
               _              -> Nothing

-- ** Splitting

prop_split :: Int -> Seq A -> Bool
prop_split n xs =
    s_front ~= l_front && s_back ~= l_back
  where
    p ys = Prelude.length ys > n
    (s_front, s_back) = split p xs
    (l_front, l_back) = Prelude.splitAt n (toList xs)

prop_takeUntil :: Int -> Seq A -> Bool
prop_takeUntil n xs =
    takeUntil p xs ~= Prelude.take n (toList xs)
  where
    p ys = Prelude.length ys > n

prop_dropUntil :: Int -> Seq A -> Bool
prop_dropUntil n xs =
    dropUntil p xs ~= Prelude.drop n (toList xs)
  where
    p ys = Prelude.length ys > n

-- * Transformation

prop_reverse :: Seq A -> Bool
prop_reverse xs =
    reverse xs ~= Prelude.reverse (toList xs)

-- ** Maps

prop_fmap' :: Seq A -> Bool
prop_fmap' xs =
    fmap' f xs ~= map f (toList xs)
  where
    f = Just

prop_fmapWithPos :: FingerTree MA VA -> Bool
prop_fmapWithPos xs =
    fmapWithPos f xs ~= zipWith f (prefixes xs_list) xs_list
  where
    f = WithPos
    xs_list = toList xs

prop_fmapWithContext :: FingerTree MA VA -> Bool
prop_fmapWithContext xs =
    fmapWithContext f xs ~= zipWith3 f (prefixes xs_list) xs_list (suffixes xs_list)
  where
    f = WithContext
    xs_list = toList xs

-- ** Folds

prop_foldlWithPos :: FingerTree MA VA -> Bool
prop_foldlWithPos xs =
    foldlWithPos f z xs == foldl uncurry_f z (zip (prefixes xs_list) xs_list)
  where
    z = []
    f vxs v x = WithPos v x:vxs
    uncurry_f vxs (v, x) = f vxs v x
    xs_list = toList xs

prop_foldlWithContext :: FingerTree MA VA -> Bool
prop_foldlWithContext xs =
    foldlWithContext f z xs == foldl uncurry_f z (zip3 (prefixes xs_list) xs_list (suffixes xs_list))
  where
    z = []
    f vxs vl x vr = WithContext vl x vr:vxs
    uncurry_f vxs (vl, x, vr) = f vxs vl x vr
    xs_list = toList xs

prop_foldrWithPos :: FingerTree MA VA -> Bool
prop_foldrWithPos xs =
    foldrWithPos f z xs == foldr uncurry_f z (zip (prefixes xs_list) xs_list)
  where
    z = []
    f v x vxs = WithPos v x:vxs
    uncurry_f (v, x) vxs = f v x vxs
    xs_list = toList xs

prop_foldrWithContext :: FingerTree MA VA -> Bool
prop_foldrWithContext xs =
    foldrWithContext f z xs == foldr uncurry_f z (zip3 (prefixes xs_list) xs_list (suffixes xs_list))
  where
    z = []
    f vl x vr vxs = WithContext vl x vr:vxs
    uncurry_f (vl, x, vr) vxs = f vl x vr vxs
    xs_list = toList xs

-- ** Traversals

prop_traverse' :: Seq A -> Bool
prop_traverse' xs =
    evalM (traverse' f xs) ~= evalM (traverse f (toList xs))
  where
    f x = do
        n <- step
        return (n, x)

prop_traverseWithPos :: FingerTree MA VA -> Bool
prop_traverseWithPos xs =
    evalM (traverseWithPos f xs) ~= evalM (traverse (uncurry f) (zip (prefixes xs_list) xs_list))
  where
    f v y = do
        n <- step
        return (WithPos v (n, y))
    xs_list = toList xs

prop_traverseWithContext :: FingerTree MA VA -> Bool
prop_traverseWithContext xs =
    evalM (traverseWithContext f xs) ~= evalM (traverse uncurry_f (zip3 (prefixes xs_list) xs_list (suffixes xs_list)))
  where
    uncurry_f (vl, y, vr) = f vl y vr
    f vl y vr = do
        n <- step
        return (WithContext vl (n, y) vr)
    xs_list = toList xs

-- measure to the left of each value
prefixes :: (Measured v a) => [a] -> [v]
prefixes = scanl (<>) mempty . map measure

-- measure to the right of each value
suffixes :: (Measured v a) => [a] -> [v]
suffixes = tail . scanr (<>) mempty . map measure

------------------------------------------------------------------------
-- QuickCheck
------------------------------------------------------------------------

instance (Arbitrary a, Measured v a) => Arbitrary (FingerTree v a) where
    arbitrary = sized arb
      where
        arb :: (Arbitrary a, Measured v a) => Int -> Gen (FingerTree v a)
        arb 0 = return Empty
        arb 1 = Single <$> arbitrary
        arb n = deep <$> arbitrary <*> arb (n `div` 2) <*> arbitrary

    shrink (Deep _ (One a) Empty (One b)) = [Single a, Single b]
    shrink (Deep _ pr m sf) =
        [deep pr' m sf | pr' <- shrink pr] ++
        [deep pr m' sf | m' <- shrink m] ++
        [deep pr m sf' | sf' <- shrink sf]
    shrink (Single x) = map Single (shrink x)
    shrink Empty = []

instance (Arbitrary a, Measured v a) => Arbitrary (Node v a) where
    arbitrary = oneof [
        node2 <$> arbitrary <*> arbitrary,
        node3 <$> arbitrary <*> arbitrary <*> arbitrary]

    shrink (Node2 _ a b) =
        [node2 a' b | a' <- shrink a] ++
        [node2 a b' | b' <- shrink b]
    shrink (Node3 _ a b c) =
        [node2 a b, node2 a c, node2 b c] ++
        [node3 a' b c | a' <- shrink a] ++
        [node3 a b' c | b' <- shrink b] ++
        [node3 a b c' | c' <- shrink c]

instance Arbitrary a => Arbitrary (Digit a) where
    arbitrary = oneof [
        One <$> arbitrary,
        Two <$> arbitrary <*> arbitrary,
        Three <$> arbitrary <*> arbitrary <*> arbitrary,
        Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary]

    shrink (One a) = map One (shrink a)
    shrink (Two a b) = [One a, One b]
    shrink (Three a b c) = [Two a b, Two a c, Two b c]
    shrink (Four a b c d) = [Three a b c, Three a b d, Three a c d, Three b c d]

------------------------------------------------------------------------
-- Valid trees
------------------------------------------------------------------------

class Valid a where
    valid :: a -> Bool

instance (Measured v a, Eq v, Valid a) => Valid (FingerTree v a) where
    valid Empty = True
    valid (Single x) = valid x
    valid (Deep s pr m sf) =
        s == measure pr `mappend` measure m `mappend` measure sf &&
        valid pr && valid m && valid sf

instance (Measured v a, Eq v, Valid a) => Valid (Node v a) where
    valid node = measure node == foldMap measure node && all valid node

instance Valid a => Valid (Digit a) where
    valid = all valid

instance Valid A where
    valid = const True

instance Valid (a,b) where
    valid = const True

instance Valid (a,b,c) where
    valid = const True

instance Valid (Maybe a) where
    valid = const True

instance Valid [a] where
    valid = const True

------------------------------------------------------------------------
-- Use list of elements as the measure
------------------------------------------------------------------------

type Seq a = FingerTree [a] a

instance Measured [A] A where
    measure x = [x]

instance Measured [OrdA] OrdA where
    measure x = [x]

instance Measured [Maybe a] (Maybe a) where
    measure x = [x]

instance Measured [(a, b)] (a, b) where
    measure x = [x]

------------------------------------------------------------------------
-- A noncommutative monoid as a measure: semidirect product
------------------------------------------------------------------------

data MA = MA Int Int
    deriving (Eq, Show)

instance Semigroup MA where
    MA a x <> MA b y = MA (a*b) (x + a*y)

instance Monoid MA where
    mempty = MA 1 0

instance Valid MA where
    valid = const True

newtype VA = VA Int
    deriving (Eq, Show)

instance Measured MA VA where
    measure (VA x) = MA 3 x

instance Arbitrary VA where
    arbitrary = VA <$> arbitrary
    shrink (VA x) = map VA (shrink x)

instance Valid VA where
    valid = const True

------------------------------------------------------------------------
-- Values with positions and contexts
------------------------------------------------------------------------

data WithPos v a = WithPos v a
    deriving (Eq, Show)

instance Monoid v => Measured v (WithPos v a) where
    measure (WithPos v _) = v

instance (Valid v, Valid a) => Valid (WithPos v a) where
    valid (WithPos v x) = valid v && valid x

data WithContext v a = WithContext v a v
    deriving (Eq, Show)

instance Monoid v => Measured v (WithContext v a) where
    measure (WithContext vl _ vr) = vl

instance (Valid v, Valid a) => Valid (WithContext v a) where
    valid (WithContext vl x vr) = valid vl && valid x && valid vr

------------------------------------------------------------------------
-- Simple counting monad
------------------------------------------------------------------------

newtype M a = M (Int -> (Int, a))

runM :: M a -> Int -> (Int, a)
runM (M m) = m

evalM :: M a -> a
evalM m = snd (runM m 0)

instance Monad M where
    return = pure
    M u >>= f = M $ \ m -> let (n, x) = u m in runM (f x) n

instance Functor M where
    fmap f (M u) = M $ \ m -> let (n, x) = u m in (n, f x)

instance Applicative M where
    pure x = M $ \ n -> (n, x)
    (<*>) = ap

step :: M Int
step = M $ \ n -> (n+1, n)
