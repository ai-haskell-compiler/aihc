-- Do not edit! Automatically created with doctest-extract from src/Data/Array/Comfort/Shape/Static.hs
{-# LINE 17 "src/Data/Array/Comfort/Shape/Static.hs" #-}

module DocTest.Data.Array.Comfort.Shape.Static where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 18 "src/Data/Array/Comfort/Shape/Static.hs" #-}
import     qualified Data.Array.Comfort.Shape.Static as Static
import     qualified Data.Array.Comfort.Shape as Shape

import     qualified Type.Data.Num.Unary.Literal as TypeNum
import     qualified Type.Data.Num.Unary as Unary

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Array.Comfort.Shape.Static:29: "
{-# LINE 29 "src/Data/Array/Comfort/Shape/Static.hs" #-}
 DocTest.example
{-# LINE 29 "src/Data/Array/Comfort/Shape/Static.hs" #-}
   (Shape.indices (Static.ZeroBased (Unary.unary TypeNum.u0)))
  [ExpectedLine [LineChunk "[]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape.Static:31: "
{-# LINE 31 "src/Data/Array/Comfort/Shape/Static.hs" #-}
 DocTest.example
{-# LINE 31 "src/Data/Array/Comfort/Shape/Static.hs" #-}
   (Shape.indices (Static.ZeroBased (Unary.unary TypeNum.u1)))
  [ExpectedLine [LineChunk "[i0]"]]
 DocTest.printPrefix "Data.Array.Comfort.Shape.Static:33: "
{-# LINE 33 "src/Data/Array/Comfort/Shape/Static.hs" #-}
 DocTest.example
{-# LINE 33 "src/Data/Array/Comfort/Shape/Static.hs" #-}
   (Shape.indices (Static.ZeroBased (Unary.unary TypeNum.u7)))
  [ExpectedLine [LineChunk "[i0,i1,i2,i3,i4,i5,i6]"]]
