-- Do not edit! Automatically created with doctest-extract from src/MathObj/Polynomial/Core.hs
{-# LINE 47 "src/MathObj/Polynomial/Core.hs" #-}

module Test.MathObj.Polynomial.Core where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 48 "src/MathObj/Polynomial/Core.hs" #-}
import     qualified MathObj.Polynomial.Core as PolyCore
import     qualified MathObj.Polynomial as Poly
import     qualified Data.List as List
import     qualified Test.QuickCheck as QC
import     Test.QuickCheck ((==>))
import     Data.Tuple.HT (mapPair, mapSnd)
import     NumericPrelude.Numeric
import     NumericPrelude.Base
import     Prelude ()

intPoly     :: [Integer] -> [Integer]
intPoly     = id

ratioPoly     :: [Rational] -> [Rational]
ratioPoly     = id

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.Polynomial.Core:136: "
{-# LINE 136 "src/MathObj/Polynomial/Core.hs" #-}
 DocTest.property
{-# LINE 136 "src/MathObj/Polynomial/Core.hs" #-}
     (\(QC.NonEmpty xs) (QC.NonEmpty ys) -> PolyCore.tensorProduct xs ys == List.transpose (PolyCore.tensorProduct ys (intPoly xs)))
 DocTest.printPrefix "MathObj.Polynomial.Core:161: "
{-# LINE 161 "src/MathObj/Polynomial/Core.hs" #-}
 DocTest.property
{-# LINE 161 "src/MathObj/Polynomial/Core.hs" #-}
     (\xs ys  ->  PolyCore.equal (intPoly $ PolyCore.mul xs ys) (PolyCore.mulShear xs ys))
 DocTest.printPrefix "MathObj.Polynomial.Core:173: "
{-# LINE 173 "src/MathObj/Polynomial/Core.hs" #-}
 DocTest.property
{-# LINE 173 "src/MathObj/Polynomial/Core.hs" #-}
     (\x y -> case (PolyCore.normalize x, PolyCore.normalize y) of (nx, ny) -> not (null (ratioPoly ny)) ==> mapSnd PolyCore.normalize (PolyCore.divMod nx ny) == mapPair (PolyCore.normalize, PolyCore.normalize) (PolyCore.divMod x y))
 DocTest.printPrefix "MathObj.Polynomial.Core:174: "
{-# LINE 174 "src/MathObj/Polynomial/Core.hs" #-}
 DocTest.property
{-# LINE 174 "src/MathObj/Polynomial/Core.hs" #-}
     (\x y -> not (isZero (ratioPoly y)) ==> let z = fst $ PolyCore.divMod (Poly.coeffs x) y in  PolyCore.normalize z == z)
 DocTest.printPrefix "MathObj.Polynomial.Core:175: "
{-# LINE 175 "src/MathObj/Polynomial/Core.hs" #-}
 DocTest.property
{-# LINE 175 "src/MathObj/Polynomial/Core.hs" #-}
     (\x y -> case PolyCore.normalize $ ratioPoly y of ny -> not (null ny) ==> List.length (snd $ PolyCore.divMod x y) < List.length ny)
