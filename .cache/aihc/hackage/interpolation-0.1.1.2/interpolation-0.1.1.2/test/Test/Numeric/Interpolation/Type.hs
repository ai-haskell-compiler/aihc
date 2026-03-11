-- Do not edit! Automatically created with doctest-extract from src/Numeric/Interpolation/Type.hs
{-# LINE 16 "src/Numeric/Interpolation/Type.hs" #-}

module Test.Numeric.Interpolation.Type where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 17 "src/Numeric/Interpolation/Type.hs" #-}
import     qualified Numeric.Interpolation.Type as Type

checkOverlap     :: Type.T Double y ny -> [Double] -> Double -> Bool
checkOverlap     typ xs xi =
       let samples = map fst $ Type.sampleBasisFunctions typ xs xi
       in  all (< minimum samples + Type.basisOverlap typ) samples

checkOverlapNotTotal     :: Type.T Double y ny -> [Double] -> Double -> Bool
checkOverlapNotTotal     typ xs xi =
       let samples = map fst $ Type.sampleBasisFunctions typ xs xi
       in  maximum samples - minimum samples < Type.basisOverlap typ

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.Interpolation.Type:46: "
{-# LINE 46 "src/Numeric/Interpolation/Type.hs" #-}
 DocTest.property
{-# LINE 46 "src/Numeric/Interpolation/Type.hs" #-}
     (checkOverlap Type.linear)
 DocTest.printPrefix "Numeric.Interpolation.Type:62: "
{-# LINE 62 "src/Numeric/Interpolation/Type.hs" #-}
 DocTest.property
{-# LINE 62 "src/Numeric/Interpolation/Type.hs" #-}
     (checkOverlap Type.hermite1)
 DocTest.printPrefix "Numeric.Interpolation.Type:81: "
{-# LINE 81 "src/Numeric/Interpolation/Type.hs" #-}
 DocTest.property
{-# LINE 81 "src/Numeric/Interpolation/Type.hs" #-}
     (checkOverlap Type.cubicLinear)
 DocTest.printPrefix "Numeric.Interpolation.Type:97: "
{-# LINE 97 "src/Numeric/Interpolation/Type.hs" #-}
 DocTest.property
{-# LINE 97 "src/Numeric/Interpolation/Type.hs" #-}
     (checkOverlap Type.cubicParabola)
