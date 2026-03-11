-- Do not edit! Automatically created with doctest-extract from src/Data/Array/Comfort/Shape/Extra.hs
{-# LINE 24 "src/Data/Array/Comfort/Shape/Extra.hs" #-}

module DocTest.Data.Array.Comfort.Shape.Extra where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 25 "src/Data/Array/Comfort/Shape/Extra.hs" #-}
import     qualified Data.Array.Comfort.Shape.Extra as ShapeExtra
import     qualified Data.Array.Comfort.Shape as Shape

import     qualified Type.Data.Num.Unary.Literal as TypeNum
import     qualified Type.Data.Num.Unary as Unary

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Array.Comfort.Shape.Extra:47: "
{-# LINE 47 "src/Data/Array/Comfort/Shape/Extra.hs" #-}
 DocTest.example
{-# LINE 47 "src/Data/Array/Comfort/Shape/Extra.hs" #-}
   (Shape.indices $ ShapeExtra.Simplex (Unary.unary TypeNum.u3) $ Shape.ZeroBased (4::Int))
  [ExpectedLine [LineChunk "[0!:1!:2!:end,0!:1!:3!:end,0!:2!:3!:end,1!:2!:3!:end]"]]
