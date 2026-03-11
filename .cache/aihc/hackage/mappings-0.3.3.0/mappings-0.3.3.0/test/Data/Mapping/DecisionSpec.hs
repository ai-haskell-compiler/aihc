module Data.Mapping.DecisionSpec where

import Prelude hiding ((&&), (||), not, all)
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Hspec
import Data.Algebra.Boolean ((&&), (||), not, all)
import Data.Mapping
import Data.Mapping.Decision
import Data.Mapping.Piecewise


boolAct ::    Ord a
           => Decision Bool OnBool a Bool
           -> [a]
           -> Bool
boolAct a s = act a (`S.member` S.fromList s)


spec :: Spec
spec = do

  let x = test "x"
  let y = test "y"

  describe "Basic tests of act" $ do

    it "x false on {}" $ do
      boolAct x []    `shouldBe` False
    it "x true on {x}" $ do
      boolAct x ["x"] `shouldBe` True

  describe "Basic tests of mmap" $ do

    it "not x true on {}" $ do
      boolAct (not x) []    `shouldBe` True
    it "not x false on {x}" $ do
      boolAct (not x) ["x"] `shouldBe` False

  describe "Basic tests of merge" $ do

    it "x && y true on {}" $ do
      boolAct (x && y) []         `shouldBe` False
    it "x && y true on {x}" $ do
      boolAct (x && y) ["x"]      `shouldBe` False
    it "x && y true on {y}" $ do
      boolAct (x && y) ["y"]      `shouldBe` False
    it "x && y true on {x,y}" $ do
      boolAct (x && y) ["x", "y"] `shouldBe` True

    it "x || y true on {}" $ do
      boolAct (x || y) []         `shouldBe` False
    it "x || y true on {x}" $ do
      boolAct (x || y) ["x"]      `shouldBe` True
    it "x || y true on {y}" $ do
      boolAct (x || y) ["y"]      `shouldBe` True
    it "x || y true on {x,y}" $ do
      boolAct (x || y) ["x", "y"] `shouldBe` True

    it "y && x true on {}" $ do
      boolAct (y && x) []         `shouldBe` False
    it "y && x true on {x}" $ do
      boolAct (y && x) ["x"]      `shouldBe` False
    it "y && x true on {y}" $ do
      boolAct (y && x) ["y"]      `shouldBe` False
    it "y && x true on {x,y}" $ do
      boolAct (y && x) ["x", "y"] `shouldBe` True

    it "y || x true on {}" $ do
      boolAct (y || x) []         `shouldBe` False
    it "y || x true on {x}" $ do
      boolAct (y || x) ["x"]      `shouldBe` True
    it "y || x true on {y}" $ do
      boolAct (y || x) ["y"]      `shouldBe` True
    it "y || x true on {x,y}" $ do
      boolAct (y || x) ["x", "y"] `shouldBe` True

  describe "Check of listTrue" $ do

    let x0y0 = M.fromList [("x", False), ("y", False)]
    let x0y1 = M.fromList [("x", False), ("y", True)]
    let x1y0 = M.fromList [("x", True), ("y", False)]
    let x1y1 = M.fromList [("x", True), ("y", True)]

    it "Should work on &&" $ do
      S.fromList (listTrue (S.fromList ["x", "y"]) (x && y))
        `shouldBe` S.fromList [x1y1]
    it "Should work on ||" $ do
      S.fromList (listTrue (S.fromList ["x", "y"]) (x || y))
        `shouldBe` S.fromList [x0y1, x1y0, x1y1]
    it "Should work on not (1)" $ do
      S.fromList (listTrue (S.fromList ["x", "y"]) (not x))
        `shouldBe` S.fromList [x0y0, x0y1]
    it "Should work on not (2)" $ do
      S.fromList (listTrue (S.fromList ["x", "y"]) (not y))
        `shouldBe` S.fromList [x0y0, x1y0]

  describe "Properties of independent sets in C_100" $ do

    let l2 = (100,1):[(n,n+1) | n <- [1..99]]
    let l3 = (99,100,1):(100,1,2):[(n,n+1,n+2) | n <- [1..98]]
    let independent = all (\(i,j) -> not (test i && test j)) l2
    let maximal = all (\(i,j,k) -> test i || test j || test k) l3
    let t = independent && maximal

    -- Mentioned in Knuth
    it "should have the right count" $ do
      numberTrue (1::Int) 100 t `shouldBe` 1630580875002

  describe "Decision trees for monomial divisibility" $ do

    let xy2 = M.fromList [("X", 1::Int), ("Y", 2)]
    let x2y = M.fromList [("X", 2), ("Y", 1)]
    let xyz = M.fromList [("X", 1), ("Y", 1), ("Z", 1)]
    let monomials = M.fromList [(xy2, 1::Int), (x2y, 2), (xyz, 3)]
    let f i b = if b then i else 0
    let d = M.foldlWithKey' (\t m i -> merge max t (mmap (f i) . buildAll $ fmap greaterThanOrEqual m)) (cst 0) monomials
    let mapAct m = act d (\a -> M.findWithDefault 0 a $ M.fromList m)

    it "should get the right monomial for w^2y^4" $ do
      mapAct [("W", 2), ("Y", 4)] `shouldBe` 0

    it "should get the right monomial for w^3xy^2" $ do
      mapAct [("W", 3), ("X", 1), ("Y", 2)] `shouldBe` 1

    it "should get the right monomial for w^2x^2y^2" $ do
      mapAct [("W", 2), ("X", 2), ("Y", 2)] `shouldBe` 2

    it "should get the right monomial for w^2x^2y^2z^2" $ do
      mapAct [("W", 2), ("X", 2), ("Y", 2), ("Z", 2)] `shouldBe` 3

    it "should calculate neighbours correctly" $ do
      neighbours d `shouldBe` S.fromList [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)]
