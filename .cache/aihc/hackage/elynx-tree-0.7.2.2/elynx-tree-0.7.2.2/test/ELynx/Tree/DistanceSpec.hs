{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Tree.DistanceSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Aug 30 09:38:50 2019.
module ELynx.Tree.DistanceSpec
  ( spec,
  )
where

import ELynx.Tools.Equality
import ELynx.Tools.InputOutput
import ELynx.Tree
import ELynx.Tree.Arbitrary ()
import Test.Hspec
import Test.QuickCheck

treeFileSimple :: FilePath
treeFileSimple = "data/TreeDist.trees"

getSimpleTrees :: IO (Forest Phylo Name)
getSimpleTrees = parseFileWith (someNewick Standard) treeFileSimple

treeFileMany :: FilePath
treeFileMany = "data/Many.trees"

getManyTrees :: IO (Forest Phylo Name)
getManyTrees = parseFileWith (someNewick Standard) treeFileMany

-- I used treedist from Phylip to get the correct results.
-- See http://evolution.genetics.washington.edu/phylip/doc/treedist.html.
symmetricAnswers :: [Int]
symmetricAnswers =
  [ 6,
    8,
    0,
    0,
    12,
    20,
    18,
    20,
    10,
    2,
    10,
    4,
    4,
    4,
    4,
    4,
    10,
    16,
    8,
    2,
    4,
    0,
    0,
    0,
    10,
    4,
    0,
    0,
    2,
    2,
    0,
    0,
    4,
    0,
    2,
    0,
    8,
    6,
    2,
    6,
    4,
    4,
    8,
    0,
    0,
    4,
    2,
    0,
    10,
    0,
    0,
    10
  ]

branchScoreAnswers :: [Double]
branchScoreAnswers =
  [ 8.567916e-02,
    9.570577e-02,
    1.704571e-02,
    7.603990e-03,
    6.149761e-01,
    3.557070e-01,
    2.329811e-01,
    3.820208e-01,
    1.895421e-02,
    6.302364e-03,
    2.083286e-02,
    1.023777e-03,
    2.138244e-02,
    1.444380e-02,
    1.958628e-02,
    6.089461e-03,
    2.551873e-02,
    8.041220e-02,
    4.123102e-02,
    8.241811e-03,
    2.623805e-02,
    2.109278e-02,
    1.953769e-02,
    4.459926e-03,
    6.594537e-02,
    7.040703e-02,
    8.603133e-03,
    3.878009e-03,
    2.969969e-02,
    2.505262e-02,
    2.095988e-02,
    8.461041e-03,
    5.228005e-02,
    6.001320e-02,
    8.276652e-03,
    6.966115e-03,
    7.701581e-02,
    4.946339e-02,
    2.548024e-02,
    5.800598e-03,
    3.875927e-02,
    2.836737e-02,
    9.059706e-02,
    1.333325e-02,
    5.071356e-02,
    7.433056e-02,
    3.854717e-02,
    3.255993e-02,
    1.581909e-01,
    6.813096e-02,
    8.210513e-02,
    7.664642e-02
  ]

prop_dist_same_tree :: (Num b, Eq b) => (Tree e a -> Tree e a -> Either String b) -> Tree e a -> Bool
prop_dist_same_tree distanceMeasure t = case distanceMeasure t t of
  (Left "bipartitions: Tree contains duplicate leaves.") -> True
  (Left "bipartitionToBranch: Tree contains duplicate leaves.") -> True
  (Left _) -> False
  Right x -> x == 0

each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)

multifurcating :: Tree () Char
multifurcating =
  Node
    ()
    ' '
    [Node () 'A' [], Node () 'B' [], Node () ' ' [Node () 'C' [], Node () 'D' [], Node () 'E' []]]

bifurcatingComp :: Tree () Char
bifurcatingComp =
  Node
    ()
    ' '
    [ Node () ' ' [Node () 'A' [], Node () 'B' []],
      Node () ' ' [Node () 'C' [], Node () ' ' [Node () 'D' [], Node () 'E' []]]
    ]

bifurcatingIncomp :: Tree () Char
bifurcatingIncomp =
  Node
    ()
    ' '
    [ Node () ' ' [Node () 'A' [], Node () 'C' []],
      Node () ' ' [Node () 'B' [], Node () ' ' [Node () 'D' [], Node () 'E' []]]
    ]

incSplitTree1a :: Tree Phylo Name
incSplitTree1a = parseByteStringWith (oneNewick IqTree) "((a,b)0.7,(c,d)1.0);"

incSplitTree1b :: Tree Phylo Name
incSplitTree1b = parseByteStringWith (oneNewick IqTree) "((a,b)0.7,c,d);"

incSplitTree2 :: Tree Phylo Name
incSplitTree2 = parseByteStringWith (oneNewick IqTree) "((a,c)1.0,(b,d)1.0);"

incSplitTree3 :: Tree Phylo Name
incSplitTree3 = parseByteStringWith (oneNewick IqTree) "(((a,b)0.7,c)1.0,(d,e)1.0);"

incSplitTree4 :: Tree Phylo Name
incSplitTree4 = parseByteStringWith (oneNewick IqTree) "(((a,c)1.0,b)1.0,(d,e)1.0);"

-- Compute distances between adjacent pairs of a list of input trees. Use given
-- distance measure.
adjacent :: (a -> a -> b) -> [a] -> [b]
adjacent dist trs = [dist x y | (x, y) <- zip trs (tail trs)]

-- noPL :: Phylo
-- noPL = Phylo Nothing Nothing

spec :: Spec
spec = do
  describe "symmetric" $ do
    it "calculates correct distances for sample trees" $ do
      simpleTrees <- getSimpleTrees
      symmetric (head simpleTrees) (simpleTrees !! 1) `shouldBe` Right 2
      manyTrees <- getManyTrees
      -- Since treedist computes the distance between adjacent pairs, in the
      -- following manner: [tr0, tr1, tr2, tr3] -> [dist tr0 tr1, dist tr2 tr3],
      -- we have to skip some distances.
      each 2 (adjacent symmetric manyTrees)
        `shouldBe` map Right symmetricAnswers
    it "is zero for a collection of random trees" $
      property $
        prop_dist_same_tree
          (symmetric :: Tree Phylo Double -> Tree Phylo Double -> Either String Int)
  describe "incompatibleSplit" $ do
    it "calculates correct distances for sample trees" $ do
      incompatibleSplits multifurcating bifurcatingComp `shouldBe` Right 0
      incompatibleSplits bifurcatingComp multifurcating `shouldBe` Right 0
      -- print $ S.map bpHuman <$> bipartitions bifurcatingIncomp
      -- print $ S.map bpHuman <$> bipartitions multifurcating
      -- print $ S.map mpHuman <$> partitions bifurcatingIncomp
      -- print $ S.map mpHuman <$> partitions multifurcating
      -- print $ toNewick $ first (const noPL) bifurcatingIncomp
      -- print $ toNewick $ first (const noPL) multifurcating
      incompatibleSplits bifurcatingIncomp multifurcating `shouldBe` Right 2
      incompatibleSplits multifurcating bifurcatingIncomp `shouldBe` Right 2
    it "calculates correct distances for sample trees with branch support" $ do
      incompatibleSplits incSplitTree1a incSplitTree2 `shouldBe` Right 2
      incompatibleSplits incSplitTree1b incSplitTree2 `shouldBe` Right 2
      let t1a = either error id $ toSupportTree incSplitTree1a
          t1b = either error id $ toSupportTree incSplitTree1b
          tr2 = either error id $ toSupportTree incSplitTree2
          tr3 = either error id $ toSupportTree incSplitTree3
          tr4 = either error id $ toSupportTree incSplitTree4
      incompatibleSplits (collapse 0.7 t1a) tr2 `shouldBe` Right 2
      incompatibleSplits (collapse 0.71 t1b) tr2 `shouldBe` Right 0
      incompatibleSplits (collapse 0.71 tr3) tr4 `shouldBe` Right 0
    it "is zero for a collection of random trees" $
      property $
        prop_dist_same_tree
          (incompatibleSplits :: Tree Phylo Double -> Tree Phylo Double -> Either String Int)
  describe "branchScore" $ do
    it "calculates correct distances for sample trees" $ do
      manyTrees <- getManyTrees
      let ts = map (either error id . toLengthTree) manyTrees
      let ds = map (either error id) $ each 2 $ adjacent branchScore ts
      ds `shouldSatisfy` nearlyEqListWith 1e-5 branchScoreAnswers
    it "is zero for a collection of random trees" $
      property $
        prop_dist_same_tree
          (branchScore :: Tree Length Double -> Tree Length Double -> Either String Double)
