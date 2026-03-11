{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module HaskellWorks.Data.Conduit.LeftoverSpec where

import Control.Monad.Identity
import Data.Conduit
import Data.Conduit.List               as CL
import HaskellWorks.Data.Conduit.Merge
import Test.Hspec

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

comb :: (Ord a, Enum a, Num a) => a -> a -> ([a], [a], [a])
comb a b
  | a > b     = ([1..a-1], [], [])
  | b > a     = ([], [], [1..b-1])
  | otherwise = ([], [a], [])

spec :: Spec
spec = describe "HaskellWorks.Data.Conduit.LeftoverSpec" $ do
  describe "Merge should preserve leftovers order" $ do
    it "On left hand side" $
      merge [5] [1] `shouldBe` LeftoverL <$> [1,2,3,4]

    it "On right hand side" $
      merge [1] [5] `shouldBe` LeftoverR <$> [1,2,3,4]

merge :: [Int] -> [Int] -> [JoinResult Int Int Int]
merge as bs = runFor (CL.sourceList as) (CL.sourceList bs)

runFor :: ConduitT () Int Identity () -> ConduitT () Int Identity () -> [JoinResult Int Int Int]
runFor as bs = runIdentity $ runConduit $ joinSources comb as bs .| CL.take 1000
