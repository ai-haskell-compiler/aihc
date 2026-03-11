{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Map.Syntax.Util where

------------------------------------------------------------------------------
import qualified Data.Map                       as M
import qualified Data.Set                       as Set
import           Test.QuickCheck                (Arbitrary (arbitrary))
import           Test.QuickCheck.Gen            (listOf, elements)
------------------------------------------------------------------------------
import           Data.Map.Syntax
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- |All elements that appear more than once in a list (once each)
dups :: (Eq a,Ord a) => [a] -> Set.Set a
dups xs = let countMap = M.fromListWith (+) (zip xs $ repeat (1::Int))
          in  Set.fromList . map fst . M.toList $ M.filter (>1) countMap

newtype ArbMapSyntax a b = ArbMapSyntax { unArbSyntax :: MapSyntax a b }

------------------------------------------------------------------------------
instance (Arbitrary a, Arbitrary b) => Arbitrary (ArbMapSyntax a b) where
  arbitrary = do
    ks     <- arbitrary
    vs     <- arbitrary
    strats <- listOf $ elements [Replace,Ignore,Error]
    return . ArbMapSyntax $
      mapM_ (\(s, k, v) -> addStrat s k v) (zip3 strats ks vs)


------------------------------------------------------------------------------
-- |An (invalid) show instance - to have something for QuickCheck to print
instance (Show a, Ord a, Show b) => Show (ArbMapSyntax a b) where
  show m = "<MapSyntax> state " ++ show (runMap . unArbSyntax $ m)


------------------------------------------------------------------------------
-- | Some sample MapSyntax's with various degrees of overlap
mkMapABC :: (Char -> Int -> MapSyntax Char Int) -> MapSyntax Char Int
mkMapABC strat = do
  'A' `strat` 1
  'B' `strat` 2
  'C' `strat` 3

mkMapDEF :: (Char -> Int -> MapSyntax Char Int) -> MapSyntax Char Int
mkMapDEF strat = do
  'D' `strat` 10
  'E' `strat` 20
  'F' `strat` 30

mkMapAEF :: (Char -> Int -> MapSyntax Char Int) -> MapSyntax Char Int
mkMapAEF strat = do
  'A' `strat` 100
  'E' `strat` 200
  'F' `strat` 300
