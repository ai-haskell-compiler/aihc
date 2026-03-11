{- |
   Module      :  Statistics.Pava.CommonSpec
   Description :  Unit tests for Statistics.Pava.CommonSpec
   Copyright   :  2021 Dominik Schrempf
   License     :  GPL-3.0-or-later

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Mon Jun  8 11:08:48 2020.

-}

module Statistics.Pava.CommonSpec
  ( spec
  )
where

import           Test.Hspec
import qualified Data.Vector.Unboxed           as V
import           Data.Vector.Unboxed            ( Vector )

import           Statistics.Pava.Common

xs :: Vector Int
xs = V.fromList [0, 1, 7, 9]

ys :: Vector Double
ys = V.fromList [19.0, 16.0, 1.0, 0.0]

ysSmooth :: Vector Double
ysSmooth = V.fromList [19.0, 16.0, 13.5, 11.0, 8.5, 6.0, 3.5, 1.0, 0.5, 0.0]

xsWeird :: Vector Int
xsWeird = V.fromList [-2001, 7293, 9128]

ysWeird :: Vector Double
ysWeird = V.fromList [88, 22, 920938]

spec :: Spec
spec = describe "smooth" $ do
  it "correctly smooths various test cases" $ do
    smooth xs ys `shouldBe` ysSmooth
    let val = smooth xsWeird ysWeird
    V.length val `shouldBe` 9128 + 2001 + 1
    val V.! 0 `shouldBe` 88
    val V.! (7293 + 2001) `shouldBe` 22
    V.last val `shouldBe` 920938
  it "shouldn't fail on a singleton vector"
    $          smooth (V.singleton 0) (V.singleton 1.0)
    `shouldBe` V.singleton 1.0
  it "shouldn't fail on an empty vector"
    $          smooth V.empty V.empty
    `shouldBe` V.empty
