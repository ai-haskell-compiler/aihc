module Main where

import Test.Hspec
import Test.QuickCheck
import qualified Data.CompactMap as CM
import qualified Data.Map.Strict as M
import Data.List (nubBy)
import Control.Applicative
import Data.Function (on)
import Prelude

listAndOtherkey :: Gen ([(Int,Int)], Int)
listAndOtherkey = do
    lst <- listOfInts
    let keys = map fst lst
    n <- suchThat arbitrary (`notElem` keys)
    return (lst, n)

listOfInts :: Gen [(Int, Int)]
listOfInts = nubBy ((==) `on` fst) <$> listOf ((,) <$> arbitrary <*> arbitrary)

main :: IO ()
main = hspec $
    describe "Data.CompactMap.lookup" $ do
        it "works like in containers when the key doesn't exist" $
            forAll listAndOtherkey $ \(list, k) -> M.lookup k (M.fromList list) == fmap snd (CM.lookup k (CM.fromList list fst))
        it "works like in containers when the key does exist" $
            forAll listOfInts $ \list -> let mp = M.fromList list
                                             cm = CM.fromList list fst
                                         in  all (\(k,_) -> M.lookup k mp == fmap snd (CM.lookup k cm)) list
