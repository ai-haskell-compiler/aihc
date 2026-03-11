{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Alloy.CallSpec (spec) where

import Control.Concurrent.Async         (forConcurrently)
#if TEST_DIFFERENT_SOLVERS
import Data.Foldable                    (for_)
import Data.List                        ((\\))
#endif
import Data.Map                         (Map)
import Data.Set                         (Set)
import Data.String.Interpolate          (i)
import Test.Hspec
import Text.Show.Pretty                 (ppShow)

import Language.Alloy.Call (
  CallAlloyConfig (..),
#if TEST_DIFFERENT_SOLVERS
  SatSolver (..),
#endif
  defaultCallAlloyConfig,
  existsInstance,
  getInstances,
  getInstancesWith,
  )
import Language.Alloy.Types             (Entry (..), Relation (..))

deriving instance Eq (Relation Set)
deriving instance Eq (Entry Map Set)

deriving instance Show (Relation Set)
deriving instance Show (Entry Map Set)

spec :: Spec
spec = do
  describe "existsInstance" $ do
    it "an empty spec has an instance" $
      existsInstance "" `shouldReturn` True
    it "a conflicting spec has no instance" $
      existsInstance (runPredicate "a > a") `shouldReturn` False
  describe "getInstances" $ do
    it "an empty spec returns a single trivial instance" $ do
      expected <- readFile "test/unit/emptySpecInstance.hs"
      (ppShow <$> getInstances (Just 2) "") `shouldReturn` init expected
    it "a conflicting spec returns no instance" $
      getInstances (Just 1) (runPredicate "a > a") `shouldReturn` []
    it "giving not enough time should return no result" $
      getInstancesWith cfg (runPredicate "a >= a") `shouldReturn` []
#if TEST_DIFFERENT_SOLVERS
    let unsupported = [BerkMin, Glucose41, PLingeling, Spear]
        solvers = [minBound ..] \\ unsupported
    for_ solvers $ \solver ->
      it ("using solver " ++ show solver ++ " generates an instance") $ do
        xs <- length <$> getInstancesWith
          cfg {
            maxInstances = Just 1,
            satSolver = solver,
            timeout = Nothing
            }
          (graph 2)
        xs `shouldBe` 1
#endif
    it "called in parallel does not create issues" $ do
      ys <- forConcurrently [1 .. 6] $ \x ->
        length <$> getInstances (Just $ x ^ (3 :: Integer)) (graph x)
      ys `shouldBe` [0, 6, 27, 64, 125, 216]
    it "called in parallel using too low timeout returns no instances" $ do
      let n = 6
      xs <- forConcurrently [1 .. n] $ \x ->
        length <$> getInstancesWith cfg (graph x)
      xs `shouldBe` replicate (fromInteger n) 0
  where
    runPredicate property =
      "pred a (a: Int) { " ++  property ++ " }\n" ++ "run a"
    cfg = defaultCallAlloyConfig {
      maxInstances = Nothing,
      timeout      = Just 0
      }

graph :: Integer -> String
graph x = [i|
abstract sig Node {
  flow : Node -> lone Int,
  stored : one Int
} {
  stored >= 0
  all n : Node | some flow[n] implies flow[n] >= 0
  no flow[this]
}

fun currentFlow(x, y : one Node) : Int {
  let s = x.stored, f = x.flow[y] | s < f implies s else f
}

pred withFlow[x, y : one Node] {
  currentFlow[x, y] > 0
}

run withFlow for #{x} Int, #{x} Node
|]
