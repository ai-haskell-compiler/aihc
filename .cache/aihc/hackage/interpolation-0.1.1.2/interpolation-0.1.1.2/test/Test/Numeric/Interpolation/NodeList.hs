-- Do not edit! Automatically created with doctest-extract from src/Numeric/Interpolation/NodeList.hs
{-# LINE 19 "src/Numeric/Interpolation/NodeList.hs" #-}

module Test.Numeric.Interpolation.NodeList where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 20 "src/Numeric/Interpolation/NodeList.hs" #-}
import     qualified Numeric.Interpolation.NodeList as Nodes
import     qualified Data.Traversable as Trav
import     qualified Data.Foldable as Fold
import     qualified Data.List as List
import     Data.Tuple.HT (mapSnd)
import     Data.Char (ord)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.Interpolation.NodeList:33: "
{-# LINE 33 "src/Numeric/Interpolation/NodeList.hs" #-}
 DocTest.property
{-# LINE 33 "src/Numeric/Interpolation/NodeList.hs" #-}
     (\xs -> map (mapSnd ord) xs == Nodes.toList (fmap ord (Nodes.fromList (xs::[(Integer,Char)]))))
 DocTest.printPrefix "Numeric.Interpolation.NodeList:42: "
{-# LINE 42 "src/Numeric/Interpolation/NodeList.hs" #-}
 DocTest.property
{-# LINE 42 "src/Numeric/Interpolation/NodeList.hs" #-}
     (\xs -> map snd xs == Fold.toList (Nodes.fromList (xs::[(Integer,Char)])))
 DocTest.printPrefix "Numeric.Interpolation.NodeList:51: "
{-# LINE 51 "src/Numeric/Interpolation/NodeList.hs" #-}
 DocTest.property
{-# LINE 51 "src/Numeric/Interpolation/NodeList.hs" #-}
     (\x xs -> let f acc y = (acc+y,acc) in List.mapAccumL f x (map snd xs) == mapSnd Fold.toList (Trav.mapAccumL f x (Nodes.fromList (xs::[(Int,Integer)]))))
 DocTest.printPrefix "Numeric.Interpolation.NodeList:77: "
{-# LINE 77 "src/Numeric/Interpolation/NodeList.hs" #-}
 DocTest.property
{-# LINE 77 "src/Numeric/Interpolation/NodeList.hs" #-}
     (\x y -> Nodes.singleton x y == Nodes.fromList [(x,y)::(Integer,Char)])
 DocTest.printPrefix "Numeric.Interpolation.NodeList:83: "
{-# LINE 83 "src/Numeric/Interpolation/NodeList.hs" #-}
 DocTest.property
{-# LINE 83 "src/Numeric/Interpolation/NodeList.hs" #-}
     (\xs -> xs == Nodes.toList (Nodes.fromList (xs::[(Integer,Char)])))
 DocTest.printPrefix "Numeric.Interpolation.NodeList:92: "
{-# LINE 92 "src/Numeric/Interpolation/NodeList.hs" #-}
 DocTest.example
{-# LINE 92 "src/Numeric/Interpolation/NodeList.hs" #-}
   (Nodes.lookup (Nodes.fromList ([(0,'a'),(2::Int,'b')])) (-1))
  [ExpectedLine [LineChunk "(Nothing,Just (0,'a'))"]]
 DocTest.printPrefix "Numeric.Interpolation.NodeList:94: "
{-# LINE 94 "src/Numeric/Interpolation/NodeList.hs" #-}
 DocTest.example
{-# LINE 94 "src/Numeric/Interpolation/NodeList.hs" #-}
   (Nodes.lookup (Nodes.fromList ([(0,'a'),(2::Int,'b')])) 0)
  [ExpectedLine [LineChunk "(Just (0,'a'),Just (2,'b'))"]]
 DocTest.printPrefix "Numeric.Interpolation.NodeList:96: "
{-# LINE 96 "src/Numeric/Interpolation/NodeList.hs" #-}
 DocTest.example
{-# LINE 96 "src/Numeric/Interpolation/NodeList.hs" #-}
   (Nodes.lookup (Nodes.fromList ([(0,'a'),(2::Int,'b')])) 1)
  [ExpectedLine [LineChunk "(Just (0,'a'),Just (2,'b'))"]]
 DocTest.printPrefix "Numeric.Interpolation.NodeList:98: "
{-# LINE 98 "src/Numeric/Interpolation/NodeList.hs" #-}
 DocTest.example
{-# LINE 98 "src/Numeric/Interpolation/NodeList.hs" #-}
   (Nodes.lookup (Nodes.fromList ([(0,'a'),(2::Int,'b')])) 2)
  [ExpectedLine [LineChunk "(Just (2,'b'),Nothing)"]]
 DocTest.printPrefix "Numeric.Interpolation.NodeList:100: "
{-# LINE 100 "src/Numeric/Interpolation/NodeList.hs" #-}
 DocTest.example
{-# LINE 100 "src/Numeric/Interpolation/NodeList.hs" #-}
   (Nodes.lookup (Nodes.fromList ([(0,'a'),(2::Int,'b')])) 3)
  [ExpectedLine [LineChunk "(Just (2,'b'),Nothing)"]]
 DocTest.printPrefix "Numeric.Interpolation.NodeList:102: "
{-# LINE 102 "src/Numeric/Interpolation/NodeList.hs" #-}
 DocTest.example
{-# LINE 102 "src/Numeric/Interpolation/NodeList.hs" #-}
   (Nodes.lookup (Nodes.fromList ([(0,'a'),(2,'b'),(5::Int,'c')])) 3)
  [ExpectedLine [LineChunk "(Just (2,'b'),Just (5,'c'))"]]
