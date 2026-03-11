{-# LANGUAGE PartialTypeSignatures #-}

module Main (main) where

import Prelude hiding (filter)
import Control.Applicative
import Data.Foldable
import Data.Filtrable
import qualified Data.List as List
import Test.SmallCheck
import Test.Tasty
import Test.Tasty.SmallCheck

main :: IO ()
main = defaultMain $ testGroup ""
  [ testGroup "Filtrable"
      [ testProperty "filter" (prop_filter :: _ -> [Maybe Bool] -> _)
      ]
  , testGroup "nub"
      [ testProperty "nub" (prop_nub :: [Int] -> _)
      , testProperty "nubOrd" (prop_nubOrd :: [Int] -> _)
      ] 
  ]

prop_filter :: (Filtrable f, Foldable f) => (a -> Bool) -> f a -> Bool
prop_filter = liftA2 (.) all filter

prop_nub :: (Filtrable f, Traversable f, Eq a) => f a -> Bool
prop_nub = (==) <$> List.nub . toList <*> toList . nub

prop_nubOrd :: (Filtrable f, Traversable f, Ord a) => f a -> Bool
prop_nubOrd = (==) <$> List.nub . toList <*> toList . nubOrd
