-- |
-- Module      :  DirichletSpec
-- Description :  Unit tests for DirichletSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Oct 20 10:40:06 2020.
module Statistics.Distribution.DirichletSpec
  ( spec,
  )
where

import Control.Monad
import Data.Either
import qualified Data.Vector.Unboxed as V
import Numeric.Log hiding (sum)
import Statistics.Distribution.Dirichlet
import System.Random.Stateful
import Test.Hspec

eps :: Double
eps = 1e-14

dd3 :: DirichletDistribution
dd3 = either error id $ dirichletDistribution $ V.fromList [0.2, 10, 20]

alphas10 :: V.Vector Double
alphas10 = V.fromList [0.1, 0.2, 0.3, 0.4, 0.5, 16, 17, 18, 19, 20]

dd10 :: DirichletDistribution
dd10 = either error id $ dirichletDistribution alphas10

ddSym :: Int -> Double -> DirichletDistribution
ddSym n a = either error id $ dirichletDistribution $ V.replicate n a

-- Extract means.
xbar :: Int -> [V.Vector Double] -> Double
xbar n xss = sum xs / fromIntegral (length xs)
  where
    xs = map (V.! n) xss

spec :: Spec
spec = do
  describe "dirichletDistribution" $ do
    it "only works for valid parameter vectors" $ do
      dirichletDistribution (V.fromList []) `shouldSatisfy` isLeft
      dirichletDistribution (V.fromList [1]) `shouldSatisfy` isLeft
      dirichletDistribution (V.fromList [-3, 3, 192]) `shouldSatisfy` isLeft
      dirichletDistribution (V.fromList [0, 3, 192]) `shouldSatisfy` isLeft
  describe "dirichletDensity" $ do
    it "is correct for some test cases" $ do
      let ddSym2 = ddSym 2 0.5
      let r = dirichletDensity ddSym2 (V.fromList [0.2, 0.8])
      abs (exp (ln r) - 0.7957747154594766) `shouldSatisfy` (< eps)
      let rBound = dirichletDensity ddSym2 (V.fromList [0, 1])
      rBound `shouldBe` 0
      let rOutBound1 = dirichletDensity ddSym2 (V.fromList [0, 1.1])
      rOutBound1 `shouldBe` 0
      let rOutBound2 = dirichletDensity ddSym2 (V.fromList [-0.1, 0.9])
      rOutBound2 `shouldBe` 0
      let r3 = dirichletDensity dd3 (V.fromList [0.3, 0.3, 0.4])
      abs (exp (ln r3) - 0.0001217825570884453) `shouldSatisfy` (< eps)
      let rWrongDim = dirichletDensity dd3 (V.fromList [0.3, 0.7])
      rWrongDim `shouldBe` 0
  describe "dirichletSample" $ do
    it "returns valid value vectors with expected mean" $ do
      g <- newIOGenM $ mkStdGen 0
      let ddSym10 = ddSym 10 10
      xs <- replicateM 1000 (dirichletSample ddSym10 g)
      map V.length xs `shouldBe` replicate 1000 10
      -- print [ xbar i xs | i <- [0..9]]
      [abs (xbar i xs - 0.1) > 0.01 | i <- [0 .. 9]] `shouldBe` replicate 10 False
      xs' <- replicateM 1000 (dirichletSample dd10 g)
      map V.length xs' `shouldBe` replicate 1000 10
      -- print [ xbar i xs | i <- [0..9]]
      let aSum = V.sum alphas10
      [abs (xbar i xs' - (alphas10 V.! i / aSum)) > 0.01 | i <- [0 .. 9]] `shouldBe` replicate 10 False
