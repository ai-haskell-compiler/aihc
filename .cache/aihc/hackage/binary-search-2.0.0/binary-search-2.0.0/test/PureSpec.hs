module PureSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Numeric.Search.Bounded as B
import Numeric.Search.Integer as I
import Numeric.Search.Range

spec :: Spec
spec = do
  describe "Integer search" $ do
    prop "finds n when search for (>= n)" $          
      \n -> I.search (>= n)  ==  (n :: Integer)
    prop "finds (max l n) when search for (>= n) in range (>= l)" $          
      \n l -> I.searchFrom (>= n) l  ==  max l n
    prop "finds n when search for (>=n) in range (<=h), iff n <= h." $  
      \n h -> I.searchTo (>= n) h  ==  if n <= h then Just (n :: Integer) else Nothing

  describe "Range search" $ do
    prop "returns Nothing for always failing predicate." $
      \l h -> searchFromTo (const False) l (h :: Int)  ==  Nothing
    prop "finds n given that n is within the range." $
      \n l h -> searchFromTo (>= n) l h  == 
         let k = max n l in  if k <= h then Just (k::Int) else Nothing

  describe "Bounded search" $ do
    prop "always finds n when searched for (>=n), by default." $
      \n -> B.search (>= n)  ==  Just (n :: Int)
    it "fails when given always failing predicate." $  
      B.search (const False :: Int -> Bool)  `shouldBe` Nothing
    it "finds the lower bound when given an always-holding predicate." $
      B.search (const True :: Int -> Bool)  `shouldBe` Just minBound
    prop "finds (max l n) for lower-bounded search." $ 
      \l n -> B.searchFrom (>= n) l  ==  Just (max l (n::Int))
    prop "finds Nothing for always-failing predicate with a bound." $
      \l -> B.searchFrom (const False) (l::Int)  ==  Nothing
    prop "finds n for upper-bounded search, iff n is within the bound." $ 
      \n h ->  B.searchTo (>= n) h  ==  if n <= h then Just (n::Int) else Nothing
    prop "finds Nothing for always-failing predicate with a bound." $ 
      \h -> B.searchTo (const False) (h::Int)  ==  Nothing

