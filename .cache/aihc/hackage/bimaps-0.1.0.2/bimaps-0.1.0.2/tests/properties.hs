
module Main where

import           Data.Function (on)
import           Data.List (nubBy,sort)
import           Test.QuickCheck
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH
import           Data.Proxy

import           Data.Bijection.Class as BC
import qualified Data.Bijection.HashMap as BH
import qualified Data.Bijection.Vector.Unboxed as BVU



-- * generic properties

genericNonEmpty p (NonEmpty xs) = not $ BC.null bh
  where ls = makeUnique xs
        bh = fromList ls `asTypeOf` p

genericSize p xs = length ls == size bh
  where ls = makeUnique xs
        bh = fromList ls `asTypeOf` p

genericFromListToList p xs = ls == rs
  where ls = sort $ makeUnique xs
        bh = fromList ls `asTypeOf` p
        rs = sort $ toList bh



-- * Hashmaps

bh = undefined :: BH.BimapHashMap Int Int

prop_HashMap_nonEmpty = genericNonEmpty bh

prop_HashMap_size = genericSize bh

prop_HashMap_fromList_toList = genericFromListToList bh

-- -- * unboxed vectors
-- --
-- -- TODO contiguous range needed
-- 
-- bvu = undefined :: BVU.Bi Int Int
-- 
-- prop_Vector_Unboxed_nonEmpty = genericNonEmpty bvu


-- *

makeUnique :: [(Int,Int)] -> [(Int,Int)]
makeUnique = nubBy ((==) `on` snd) . nubBy ((==) `on` fst)

main :: IO ()
main = $(defaultMainGenerator)

