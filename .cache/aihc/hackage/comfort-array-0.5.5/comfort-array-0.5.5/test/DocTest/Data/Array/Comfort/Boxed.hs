-- Do not edit! Automatically created with doctest-extract from src/Data/Array/Comfort/Boxed.hs
{-# LINE 62 "src/Data/Array/Comfort/Boxed.hs" #-}

module DocTest.Data.Array.Comfort.Boxed where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 63 "src/Data/Array/Comfort/Boxed.hs" #-}
import     qualified Data.Array.Comfort.Boxed as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     Data.Array.Comfort.Boxed (Array, (!))

import     qualified Test.QuickCheck as QC

type     ShapeInt = Shape.ZeroBased Int

genArray2     :: QC.Gen (Array (ShapeInt,ShapeInt) Char)
genArray2     = do
       xs <- QC.arbitrary
       let n = length xs
       (k,m) <-
          if n == 0
             then QC.elements [(,) 0, flip (,) 0] <*> QC.choose (1,20)
             else fmap (\m -> (div n m, m)) $ QC.choose (1,n)
       return $
          Array.fromList (Shape.ZeroBased k, Shape.ZeroBased m) $ take (k*m) xs

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Array.Comfort.Boxed:201: "
{-# LINE 201 "src/Data/Array/Comfort/Boxed.hs" #-}
 DocTest.property(
{-# LINE 201 "src/Data/Array/Comfort/Boxed.hs" #-}
        
   QC.forAll genArray2 $ \xs ->
   let shape = Array.shape xs in
   Shape.size shape > 0   QC.==>
   QC.forAll (QC.elements $ Shape.indices shape) $ \(ix0,ix1) ->
      Array.pick xs ix0 ! ix1 == xs!(ix0,ix1)
  )
