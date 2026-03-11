-- Do not edit! Automatically created with doctest-extract from src/MathObj/Polynomial.hs
{-# LINE 84 "src/MathObj/Polynomial.hs" #-}

module Test.MathObj.Polynomial where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 85 "src/MathObj/Polynomial.hs" #-}
import     qualified MathObj.Polynomial as Poly
import     qualified Algebra.IntegralDomain as Integral
import     qualified Algebra.Laws as Laws
import     NumericPrelude.Numeric
import     NumericPrelude.Base
import     Prelude ()

intPoly     :: Poly.T Integer -> Poly.T Integer
intPoly     = id

ratioPoly     :: Poly.T Rational -> Poly.T Rational
ratioPoly     = id

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.Polynomial:100: "
{-# LINE 100 "src/MathObj/Polynomial.hs" #-}
 DocTest.property
{-# LINE 100 "src/MathObj/Polynomial.hs" #-}
     (Laws.identity (+) zero . intPoly)
 DocTest.printPrefix "MathObj.Polynomial:101: "
{-# LINE 101 "src/MathObj/Polynomial.hs" #-}
 DocTest.property
{-# LINE 101 "src/MathObj/Polynomial.hs" #-}
     (Laws.commutative (+) . intPoly)
 DocTest.printPrefix "MathObj.Polynomial:102: "
{-# LINE 102 "src/MathObj/Polynomial.hs" #-}
 DocTest.property
{-# LINE 102 "src/MathObj/Polynomial.hs" #-}
     (Laws.associative (+) . intPoly)
 DocTest.printPrefix "MathObj.Polynomial:103: "
{-# LINE 103 "src/MathObj/Polynomial.hs" #-}
 DocTest.property
{-# LINE 103 "src/MathObj/Polynomial.hs" #-}
     (Laws.identity (*) one . intPoly)
 DocTest.printPrefix "MathObj.Polynomial:104: "
{-# LINE 104 "src/MathObj/Polynomial.hs" #-}
 DocTest.property
{-# LINE 104 "src/MathObj/Polynomial.hs" #-}
     (Laws.commutative (*) . intPoly)
 DocTest.printPrefix "MathObj.Polynomial:105: "
{-# LINE 105 "src/MathObj/Polynomial.hs" #-}
 DocTest.property
{-# LINE 105 "src/MathObj/Polynomial.hs" #-}
     (Laws.associative (*) . intPoly)
 DocTest.printPrefix "MathObj.Polynomial:106: "
{-# LINE 106 "src/MathObj/Polynomial.hs" #-}
 DocTest.property
{-# LINE 106 "src/MathObj/Polynomial.hs" #-}
     (Laws.leftDistributive (*) (+) . intPoly)
 DocTest.printPrefix "MathObj.Polynomial:107: "
{-# LINE 107 "src/MathObj/Polynomial.hs" #-}
 DocTest.property
{-# LINE 107 "src/MathObj/Polynomial.hs" #-}
     (Integral.propInverse . ratioPoly)
