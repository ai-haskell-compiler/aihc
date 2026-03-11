-- Do not edit! Automatically created with doctest-extract from src/Data/Array/Comfort/Shape.hs
{-# LINE 129 "src/Data/Array/Comfort/Shape.hs" #-}

module DocTest.Data.Array.Comfort.Shape where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 130 "src/Data/Array/Comfort/Shape.hs" #-}
import     qualified Data.Array.Comfort.Shape as Shape
import     qualified Data.IntMap as IntMap
import     qualified Data.IntSet as IntSet
import     qualified Data.Map as Map
import     qualified Data.Set as Set
import     Data.Array.Comfort.Shape ((::+)((::+)))

import     Test.ChasingBottoms.IsBottom (isBottom)
import     Control.DeepSeq (rnf)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Array.Comfort.Shape:345: "
{-# LINE 345 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 345 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices ()
  )
  [ExpectedLine [LineChunk "[()]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:368: "
{-# LINE 368 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 368 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices (Shape.ZeroBased (7::Int))
  )
  [ExpectedLine [LineChunk "[0,1,2,3,4,5,6]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:426: "
{-# LINE 426 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 426 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices (Shape.OneBased (7::Int))
  )
  [ExpectedLine [LineChunk "[1,2,3,4,5,6,7]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:476: "
{-# LINE 476 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 476 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices (Shape.Range (-5) (5::Int))
  )
  [ExpectedLine [LineChunk "[-5,-4,-3,-2,-1,0,1,2,3,4,5]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:478: "
{-# LINE 478 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 478 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices (Shape.Range (-1,-1) (1::Int,1::Int))
  )
  [ExpectedLine [LineChunk "[(-1,-1),(-1,0),(-1,1),(0,-1),(0,0),(0,1),(1,-1),(1,0),(1,1)]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:528: "
{-# LINE 528 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 528 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices (Shape.Shifted (-4) (8::Int))
  )
  [ExpectedLine [LineChunk "[-4,-3,-2,-1,0,1,2,3]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:599: "
{-# LINE 599 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 599 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices (Shape.Enumeration :: Shape.Enumeration Ordering)
  )
  [ExpectedLine [LineChunk "[LT,EQ,GT]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:663: "
{-# LINE 663 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 663 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices (Set.fromList "comfort")
  )
  [ExpectedLine [LineChunk "\"cfmort\""]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:694: "
{-# LINE 694 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 694 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices (IntSet.fromList [3,1,4,1,5,9,2,6,5,3])
  )
  [ExpectedLine [LineChunk "[1,2,3,4,5,6,9]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:727: "
{-# LINE 727 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 727 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ fmap Shape.ZeroBased $ Map.fromList [('b', (0::Int)), ('a', 5), ('c', 2)]
  )
  [ExpectedLine [LineChunk "[('a',0),('a',1),('a',2),('a',3),('a',4),('c',0),('c',1)]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:785: "
{-# LINE 785 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 785 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ IntMap.fromList [(2, Set.fromList "abc"), (0, Set.fromList "a"), (1, Set.fromList "d")]
  )
  [ExpectedLine [LineChunk "[(0,'a'),(1,'d'),(2,'a'),(2,'b'),(2,'c')]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:851: "
{-# LINE 851 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 851 "src/Data/Array/Comfort/Shape.hs" #-}
      
   let sh2 = (Shape.ZeroBased (2::Int), Shape.ZeroBased (2::Int)) in
   let sh3 = (Shape.ZeroBased (3::Int), Shape.ZeroBased (3::Int)) in
   (Shape.offset sh3 $ Shape.indexFromOffset sh2 3,
    Shape.offset (Shape.Deferred sh3) $
      Shape.indexFromOffset (Shape.Deferred sh2) 3)
  )
  [ExpectedLine [LineChunk "(4,3)"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:955: "
{-# LINE 955 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 955 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices (Shape.ZeroBased (3::Int), Shape.ZeroBased (3::Int))
  )
  [ExpectedLine [LineChunk "[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1077: "
{-# LINE 1077 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1077 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.Square $ Shape.ZeroBased (3::Int)
  )
  [ExpectedLine [LineChunk "[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1134: "
{-# LINE 1134 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1134 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.Cube $ Shape.ZeroBased (2::Int)
  )
  [ExpectedLine [LineChunk "[(0,0,0),(0,0,1),(0,1,0),(0,1,1),(1,0,0),(1,0,1),(1,1,0),(1,1,1)]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1196: "
{-# LINE 1196 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1196 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.Triangular Shape.Upper $ Shape.ZeroBased (3::Int)
  )
  [ExpectedLine [LineChunk "[(0,0),(0,1),(0,2),(1,1),(1,2),(2,2)]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1198: "
{-# LINE 1198 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1198 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.Triangular Shape.Lower $ Shape.ZeroBased (3::Int)
  )
  [ExpectedLine [LineChunk "[(0,0),(1,0),(1,1),(2,0),(2,1),(2,2)]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1330: "
{-# LINE 1330 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1330 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.simplexAscending (replicate 3 Shape.AllDistinct) $ Shape.ZeroBased (4::Int)
  )
  [ExpectedLine [LineChunk "[[0,1,2],[0,1,3],[0,2,3],[1,2,3]]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1332: "
{-# LINE 1332 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1332 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.simplexAscending (replicate 3 Shape.SomeRepetitive) $ Shape.ZeroBased (3::Int)
  )
  [ExpectedLine [LineChunk "[[0,0,0],[0,0,1],[0,0,2],[0,1,1],[0,1,2],[0,2,2],[1,1,1],[1,1,2],[1,2,2],[2,2,2]]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1334: "
{-# LINE 1334 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1334 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.simplexAscending [Shape.Repetitive,Shape.Distinct,Shape.Repetitive] $ Shape.ZeroBased (4::Int)
  )
  [ExpectedLine [LineChunk "[[0,0,1],[0,0,2],[0,0,3],[0,1,2],[0,1,3],[0,2,3],[1,1,2],[1,1,3],[1,2,3],[2,2,3]]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1336: "
{-# LINE 1336 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1336 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.simplexAscending [Shape.Repetitive,Shape.Distinct,Shape.Distinct] $ Shape.ZeroBased (4::Int)
  )
  [ExpectedLine [LineChunk "[[0,0,1],[0,0,2],[0,0,3],[0,1,2],[0,1,3],[0,2,3],[1,1,2],[1,1,3],[1,2,3],[2,2,3]]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1339: "
{-# LINE 1339 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1339 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.simplexDescending (replicate 3 Shape.AllDistinct) $ Shape.ZeroBased (4::Int)
  )
  [ExpectedLine [LineChunk "[[2,1,0],[3,1,0],[3,2,0],[3,2,1]]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1341: "
{-# LINE 1341 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1341 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.simplexDescending (replicate 3 Shape.SomeRepetitive) $ Shape.ZeroBased (3::Int)
  )
  [ExpectedLine [LineChunk "[[0,0,0],[1,0,0],[1,1,0],[1,1,1],[2,0,0],[2,1,0],[2,1,1],[2,2,0],[2,2,1],[2,2,2]]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1343: "
{-# LINE 1343 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1343 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.simplexDescending [Shape.Repetitive,Shape.Distinct,Shape.Repetitive] $ Shape.ZeroBased (4::Int)
  )
  [ExpectedLine [LineChunk "[[1,1,0],[2,1,0],[2,2,0],[2,2,1],[3,1,0],[3,2,0],[3,2,1],[3,3,0],[3,3,1],[3,3,2]]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1345: "
{-# LINE 1345 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1345 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices $ Shape.simplexDescending [Shape.Repetitive,Shape.Distinct,Shape.Distinct] $ Shape.ZeroBased (4::Int)
  )
  [ExpectedLine [LineChunk "[[1,1,0],[2,1,0],[2,2,0],[2,2,1],[3,1,0],[3,2,0],[3,2,1],[3,3,0],[3,3,1],[3,3,2]]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1579: "
{-# LINE 1579 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.property(
{-# LINE 1579 "src/Data/Array/Comfort/Shape.hs" #-}
      let shape = Shape.Cyclic (10::Int) in Shape.offset shape (-1) == Shape.offset shape 9
  )
 DocTest.printPrefix "Data.Array.Comfort.Shape:1584: "
{-# LINE 1584 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1584 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices (Shape.Cyclic (7::Int))
  )
  [ExpectedLine [LineChunk "[0,1,2,3,4,5,6]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1628: "
{-# LINE 1628 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1628 "src/Data/Array/Comfort/Shape.hs" #-}
    Shape.indices (Shape.ZeroBased (3::Int) ::+ Shape.Range 'a' 'c')
  )
  [ExpectedLine [LineChunk "[Left 0,Left 1,Left 2,Right 'a',Right 'b',Right 'c']"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1715: "
{-# LINE 1715 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1715 "src/Data/Array/Comfort/Shape.hs" #-}
    rnf (Shape.NestedTuple (Shape.Element 1, Shape.Element 2))
  )
  [ExpectedLine [LineChunk "()"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1717: "
{-# LINE 1717 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1717 "src/Data/Array/Comfort/Shape.hs" #-}
    rnf (Shape.NestedTuple (Shape.Element 1, (Shape.Element 2, Shape.Element 3)))
  )
  [ExpectedLine [LineChunk "()"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1719: "
{-# LINE 1719 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1719 "src/Data/Array/Comfort/Shape.hs" #-}
    isBottom $ rnf (Shape.NestedTuple (Shape.Element undefined, Shape.Element 2))
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1721: "
{-# LINE 1721 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1721 "src/Data/Array/Comfort/Shape.hs" #-}
    isBottom $ rnf (Shape.NestedTuple (Shape.Element undefined, (Shape.Element 2, Shape.Element 3)))
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1723: "
{-# LINE 1723 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1723 "src/Data/Array/Comfort/Shape.hs" #-}
    isBottom $ rnf (Shape.NestedTuple (Shape.Element 1, (Shape.Element undefined, Shape.Element 3)))
  )
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape:1725: "
{-# LINE 1725 "src/Data/Array/Comfort/Shape.hs" #-}
 DocTest.example(
{-# LINE 1725 "src/Data/Array/Comfort/Shape.hs" #-}
    isBottom $ rnf (Shape.NestedTuple (Shape.Element 1, (Shape.Element 2, Shape.Element undefined)))
  )
  [ExpectedLine [LineChunk "True"]]
