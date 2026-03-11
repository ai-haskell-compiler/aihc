-- |
-- Module      :  ELynx.MarkovProcess.RateMatrixSpec
-- Description :  Unit tests for rate matrices
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Apr 17 15:18:02 2020.
module ELynx.MarkovProcess.RateMatrixSpec
  ( spec,
  )
where

import ELynx.MarkovProcess.RateMatrix
  ( exchFromListLower,
    exchFromListUpper,
  )
import Numeric.LinearAlgebra
import Test.Hspec

es :: [Double]
es = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

exMLower :: Matrix R
exMLower =
  (5 >< 5)
    [0, 1, 2, 4, 7, 1, 0, 3, 5, 8, 2, 3, 0, 6, 9, 4, 5, 6, 0, 10, 7, 8, 9, 10, 0]

exMUpper :: Matrix R
exMUpper =
  (5 >< 5)
    [0, 1, 2, 3, 4, 1, 0, 5, 6, 7, 2, 5, 0, 8, 9, 3, 6, 8, 0, 10, 4, 7, 9, 10, 0]

spec :: Spec
spec = do
  describe "exchFromListLower" $
    it "correctly converts to matrix from list" $
      exMLower
        `shouldBe` exchFromListLower 5 es
  describe "exchFromListUpper" $
    it "correctly converts to matrix from list" $
      exMUpper
        `shouldBe` exchFromListUpper 5 es
