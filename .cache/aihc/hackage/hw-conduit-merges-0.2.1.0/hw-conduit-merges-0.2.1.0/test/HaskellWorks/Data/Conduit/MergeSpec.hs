{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module HaskellWorks.Data.Conduit.MergeSpec where

import Control.Monad.Identity
import Data.Conduit
import Data.Conduit.List               as CL
import HaskellWorks.Data.Conduit.Merge
import Test.Hspec

comb :: (Ord a, Num a) => a -> a -> ([a], [a], [a])
comb a b
  | a > b     = ([a - b], [b], [])
  | b > a     = ([], [a], [b - a])
  | otherwise = ([], [a], [])

spec :: Spec
spec = describe "HaskellWorks.Data.Conduit.MergeSpec" $ do
  describe "Merges sources" $ do
    it "exact sources" $
      merge [1,2,3] [1,2,3] `shouldBe` JoinValue <$> [1,2,3]

    it "longer right" $
      merge [1,2] [1,2,3] `shouldBe` [ JoinValue 1
                                     , JoinValue 2
                                     , LeftoverR 3
                                     ]

    it "longer left" $
      merge [1,2,3] [1,2] `shouldBe` [ JoinValue 1
                                     , JoinValue 2
                                     , LeftoverL 3
                                     ]

    it "with leftovers" $
      merge [10,20,30] [6,4,20] `shouldBe` [ JoinValue 6
                                           , JoinValue 4
                                           , JoinValue 20
                                           , LeftoverL 30
                                           ]

  describe "multiple" $ do

    it "values" $
      let f a b = ([], [a + b, a + b], [])
          res = mergeComb f [1,2] [10,20]
       in res `shouldBe` [ JoinValue 11, JoinValue 11
                         , JoinValue 22, JoinValue 22
                         ]

    it "leftovers (left)" $
      let f a b = if b > a then ([5,5], ["+" ++ show (a + b)], []) else ([], ["-" ++ show (a + b)], [])
          res = mergeComb f [1,2,10] [10, 10, 10]
       in res `shouldBe` [ JoinValue "+11"                   -- first: (1, 10)
                         , JoinValue "+15", JoinValue "+15"  -- two "5" from leftovers: (5, 10), (5, 10)
                         , LeftoverL 5, LeftoverL 5, LeftoverL 5, LeftoverL 5 -- each prev leftovers
                         , LeftoverL 2, LeftoverL 10 -- the rest of the first list
                         ]

    it "leftovers (right)" $
      let f a b = if a > b then ([], ["+" ++ show (a + b)], [5,5]) else ([], ["-" ++ show (a + b)], [])
          res = mergeComb f [10, 10, 10] [1,2,10]
       in res `shouldBe` [ JoinValue "+11"                   -- first: (10, 1)
                         , JoinValue "+15", JoinValue "+15"  -- two "5" from leftovers: (10, 5), (10, 5)
                         , LeftoverR 5, LeftoverR 5, LeftoverR 5, LeftoverR 5 -- each prev leftovers
                         , LeftoverR 2, LeftoverR 10 -- the rest of the snd list
                         ]

merge :: [Int] -> [Int] -> [JoinResult Int Int Int]
merge as bs = runFor (CL.sourceList as) (CL.sourceList bs)

mergeComb :: (a -> b -> ([a], [v], [b])) -> [a] -> [b] -> [JoinResult a v b]
mergeComb f as bs =
  runIdentity $ runConduit $ joinSources f (CL.sourceList as) (CL.sourceList bs) .| CL.take 10

runFor :: ConduitT () Int Identity () -> ConduitT () Int Identity () -> [JoinResult Int Int Int]
runFor as bs = runIdentity $ runConduit $ joinSources comb as bs .| CL.take 1000
