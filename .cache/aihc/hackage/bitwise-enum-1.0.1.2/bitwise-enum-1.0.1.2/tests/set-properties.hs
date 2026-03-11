{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions        #-}

import Prelude

import Control.Applicative ((<|>))
import Data.Foldable (foldl', foldr')
import Data.List ((\\), nub, sort)
import Data.Bits
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Function
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Enum.Set.Base as E

main :: IO ()
main = defaultMain
    [ -- Construction
      testProperty "singleton" prop_singleton
    , testProperty "fromFoldable" prop_fromFoldable
      -- Insertion/Deletion
    , testProperty "insert/delete" prop_insertDelete
      -- Query
    , testProperty "member" prop_member
    , testProperty "notMember" prop_notMember
    , testProperty "size" prop_size
    , testProperty "isSubsetOf" prop_isSubsetOf
      -- Combine
    , testProperty "unionInsert" prop_unionInsert
    , testProperty "unionAssoc" prop_unionAssoc
    , testProperty "unionComm" prop_unionComm
    , testProperty "difference" prop_difference
    , testProperty "intersection" prop_intersection
      -- Filter
    , testProperty "filter" prop_filter
    , testProperty "partition" prop_partition
      -- Map
    , testProperty "map" prop_map
    , testProperty "map2" prop_map2
      -- Folds
    , testProperty "foldl" prop_foldl
    , testProperty "foldl'" prop_foldl'
    , testProperty "foldr" prop_foldr
    , testProperty "foldr'" prop_foldr'
    , testProperty "foldl_origin" prop_foldl_origin
    , testProperty "foldl'_origin" prop_foldl'_origin
    , testProperty "foldr_origin" prop_foldr_origin
    , testProperty "foldr'_origin" prop_foldr'_origin
      -- Special folds
    , testProperty "foldMap" prop_foldMap
    , testProperty "traverse" prop_traverse
    , testProperty "any" prop_any
    , testProperty "all" prop_all
      -- Min/Max
    , testProperty "minimum" prop_minimum
    , testProperty "maximum" prop_maximum
    , testProperty "minView" prop_minView
    , testProperty "maxView" prop_maxView
      -- Conversion
    , testProperty "toList" prop_toList
    , testProperty "fromRaw/toRaw" prop_raw
    , testProperty "Read/Show" prop_readShow
    ]

wordSize :: Int
wordSize = finiteBitSize (0 :: Word) - 1

newtype Key = Key Int deriving (Eq, Ord, Show, Read, Real, Integral)

instance Enum Key where
    toEnum x
      | x < 0 || x > wordSize = error $ "Key.toEnum: bad argument " ++ show x
      | otherwise             = Key x
    fromEnum (Key x) = x

instance Num Key where
    (Key x) + (Key y) = Key . min wordSize $ x + y
    (Key x) - (Key y) = Key . max 0 $ x - y
    (Key x) * (Key y) = Key . min wordSize $ x + y
    negate = id
    abs = id
    signum (Key 0) = 0
    signum _       = 1
    fromInteger = toEnum . max 0 . min wordSize . fromInteger

instance Bounded Key where
    minBound = 0
    maxBound = Key $ finiteBitSize (0 :: Word) - 1

instance Arbitrary Key where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink    = shrinkIntegral

instance Function Key where
    function = functionIntegral

instance CoArbitrary Key where
    coarbitrary = coarbitraryIntegral

type ES = E.EnumSet Word Key

fromBits :: w -> E.EnumSet w a
fromBits = unsafeCoerce

toBits :: E.EnumSet w a -> w
toBits = unsafeCoerce

instance Arbitrary w => Arbitrary (E.EnumSet w a) where
    arbitrary = fmap fromBits arbitrary
    shrink = fmap fromBits . shrink . toBits

-- * Construction

prop_singleton :: Key -> Bool
prop_singleton x = E.insert x (E.empty :: ES) == E.singleton x

prop_fromFoldable :: [Key] -> Property
prop_fromFoldable xs =
    (E.fromFoldable xs :: ES) === foldr E.insert E.empty xs

-- * Insertion/Deletion

prop_insertDelete :: Key -> ES -> Property
prop_insertDelete k t = not (E.member k t) ==> E.delete k (E.insert k t) == t

-- * Query

prop_member :: [Key] -> Key -> Bool
prop_member xs n =
    let m = E.fromFoldable xs :: ES
    in all (\k -> k `E.member` m == (k `elem` xs)) (n : xs)

prop_notMember :: [Key] -> Key -> Bool
prop_notMember xs n =
    let m = E.fromFoldable xs :: ES
    in all (\k -> k `E.notMember` m == (k `notElem` xs)) (n : xs)

prop_size :: ES -> Bool
prop_size s = E.size s == length (E.toList s)

-- TODO isSubsetOf

prop_isSubsetOf :: ES -> ES -> Bool
prop_isSubsetOf x y = x `E.isSubsetOf` y == all (`elem` E.toList y) (E.toList x)

-- * Combine

prop_unionInsert :: Key -> ES -> Bool
prop_unionInsert x t = E.union t (E.singleton x) == E.insert x t

prop_unionAssoc :: ES -> ES -> ES -> Bool
prop_unionAssoc x y z = E.union x (E.union y z) == E.union (E.union x y) z

prop_unionComm :: ES -> ES -> Bool
prop_unionComm x y = E.union x y == E.union y x

prop_difference :: [Key] -> [Key] -> Bool
prop_difference xs ys =
    (E.toList :: ES -> [Key])
    (E.difference (E.fromFoldable xs) (E.fromFoldable ys))
    == sort ((\\) (nub xs) (nub ys))

prop_intersection :: ES -> ES -> Bool
prop_intersection x y =
    x `E.intersection` y == E.fromFoldable (filter (`elem` E.toList x) (E.toList y))

-- * Filter

prop_filter :: ES -> Bool
prop_filter s = E.partition odd s == (E.filter odd s, E.filter even s)

prop_partition :: ES -> Bool
prop_partition s = case E.partition odd s of
    (s1,s2) -> all odd (E.toList s1) && all even (E.toList s2)
               && s == s1 `E.union` s2

-- * Map

prop_map :: ES -> Bool
prop_map s = E.map id s == s

prop_map2 :: Fun Key Key -> Fun Key Key -> ES -> Property
prop_map2 f g s =
    E.map (apply f) (E.map (apply g) s) === E.map (apply f . apply g) s

-- * Folds

prop_foldl :: ES -> Bool
prop_foldl s = E.foldl (flip (:)) [] s == foldl (flip (:)) [] (E.toList s)

prop_foldl' :: ES -> Bool
prop_foldl' s = E.foldl' (flip (:)) [] s == foldl' (flip (:)) [] (E.toList s)

prop_foldr :: ES -> Bool
prop_foldr s = E.foldr (:) [] s == E.toList s

prop_foldr' :: ES -> Bool
prop_foldr' s = E.foldr' (:) [] s == E.toList s

prop_foldl_origin :: Fun Key Bool -> ES -> Bool
prop_foldl_origin = originHelper id E.foldl foldl

prop_foldl'_origin :: Fun Key Bool -> ES -> Bool
prop_foldl'_origin = originHelper id E.foldl' foldl'

prop_foldr_origin :: Fun Key Bool -> ES -> Bool
prop_foldr_origin = originHelper flip E.foldr foldr

prop_foldr'_origin :: Fun Key Bool -> ES -> Bool
prop_foldr'_origin = originHelper flip E.foldr' foldr'

-- * Special folds

prop_all :: Fun Key Bool -> ES -> Property
prop_all p s = E.all (apply p) s === all (apply p) (E.toList s)

prop_any :: Fun Key Bool -> ES -> Property
prop_any p s = E.any (apply p) s === any (apply p) (E.toList s)

prop_foldMap :: ES -> Bool
prop_foldMap s = E.foldMap return s == foldMap (: []) (E.toList s)

prop_traverse :: ES -> Bool
prop_traverse s = map E.toList (E.traverse return s) == traverse (: []) (E.toList s)


-- * Min/Max

prop_minimum :: ES -> Property
prop_minimum s = not (E.null s) ==> E.minimum s == minimum (E.toList s)

prop_maximum :: ES -> Property
prop_maximum s = not (E.null s) ==> E.maximum s == maximum (E.toList s)

prop_minView :: ES -> Bool
prop_minView s = case E.minView s of
    Nothing -> E.null s
    Just (m,s') -> m == minimum (E.toList s)
                   && s == E.insert m s' && m `E.notMember` s'

prop_maxView :: ES -> Bool
prop_maxView s = case E.maxView s of
    Nothing -> E.null s
    Just (m,s') -> m == maximum (E.toList s)
                   && s == E.insert m s' && m `E.notMember` s'

-- * Conversion

prop_toList :: [Key] -> Bool
prop_toList xs = sort (nub xs) == E.toList (E.fromFoldable xs :: ES)

prop_raw :: ES -> Bool
prop_raw s = s == E.fromRaw (E.toRaw s)

prop_readShow :: ES -> Bool
prop_readShow s = s == read (show s)

-- * Utility functions

originHelper :: ((Maybe Key -> Key -> Maybe Key) -> f)
             -> (f -> Maybe Key -> ES -> Maybe Key)
             -> (f -> Maybe Key -> [Key] -> Maybe Key)
             -> Fun Key Bool -> ES -> Bool
originHelper reorder fSet fList p s =
    fSet f Nothing s == fList f Nothing (E.toList s)
  where
    f = reorder \acc x -> acc <|> [x | apply p x]
