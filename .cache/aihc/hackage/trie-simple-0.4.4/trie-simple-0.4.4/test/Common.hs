{-# LANGUAGE DeriveFunctor #-}
module Common where

import           Test.QuickCheck

import           Control.Applicative
import           Data.List          (tails)
import           Data.Map           (Map)
import qualified Data.Map.Strict as Map

-- Alphabet-only Char
newtype C = C { unC :: Char }
  deriving (Eq, Ord)

instance Show C where
  showsPrec p (C c) = showsPrec p c
  showList cs = showList (map unC cs)

instance Arbitrary C where
  arbitrary = sized $ \n ->
      let m = sqrti (max 1 n)
      in C <$> elements (take m "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

-- Char which is either '0' or '1', always
newtype B = B { unB :: Char }
  deriving (Eq, Ord)

instance Show B where
    showsPrec p (B b) = showsPrec p b
    showList bs = showList (map unB bs)

instance Arbitrary B where
  -- Distribution is biased
  -- (To increase probability of randomly generated two [B]
  --  coincide.)
  arbitrary = B <$> elements "00001"

sqrti :: Int -> Int
sqrti = floor . (sqrt :: Double -> Double) . fromIntegral

randomPair :: [a] -> Gen (a,a)
randomPair as =
  let mkP [] = []
      mkP [_] = []
      mkP (x:xs) = (,) x <$> xs
      ps = tails as >>= mkP
  in elements ps

data Once a = Once { getDefault :: a, getVariants :: [a] }
  deriving (Functor)

instance Applicative Once where
    pure a = Once a []
    Once f fs <*> Once a as = Once (f a) (fs <*> pure a <|> f <$> as)

shrinkTraversableBy :: (Traversable t) => (a -> [a]) -> t a -> [t a]
shrinkTraversableBy f = getVariants . traverse (\a -> Once a (f a))

shrinkMapBy :: (Ord k) => (a -> [a]) -> Map k a -> [Map k a]
shrinkMapBy f = map Map.fromList . shrinkList f' . Map.toList
  where f' (k, a) = (,) k <$> f a
