-- Do not edit! Automatically created with doctest-extract from playground/Number/ComplexSquareRoot.hs
{-# LINE 21 "playground/Number/ComplexSquareRoot.hs" #-}

module Test.Number.ComplexSquareRoot where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 22 "playground/Number/ComplexSquareRoot.hs" #-}
import     qualified Number.ComplexSquareRoot as SR
import     qualified Number.Complex as Complex
import     qualified Algebra.Laws as Laws
import     Test.QuickCheck ((==>))
import     NumericPrelude.Numeric
import     NumericPrelude.Base
import     Prelude ()

sr     :: SR.T Rational -> SR.T Rational
sr     = id

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Number.ComplexSquareRoot:42: "
{-# LINE 42 "playground/Number/ComplexSquareRoot.hs" #-}
 DocTest.property
{-# LINE 42 "playground/Number/ComplexSquareRoot.hs" #-}
     (Laws.identity SR.mul SR.one . sr)
 DocTest.printPrefix "Number.ComplexSquareRoot:43: "
{-# LINE 43 "playground/Number/ComplexSquareRoot.hs" #-}
 DocTest.property
{-# LINE 43 "playground/Number/ComplexSquareRoot.hs" #-}
     (Laws.commutative SR.mul . sr)
 DocTest.printPrefix "Number.ComplexSquareRoot:44: "
{-# LINE 44 "playground/Number/ComplexSquareRoot.hs" #-}
 DocTest.property
{-# LINE 44 "playground/Number/ComplexSquareRoot.hs" #-}
     (Laws.associative SR.mul . sr)
 DocTest.printPrefix "Number.ComplexSquareRoot:45: "
{-# LINE 45 "playground/Number/ComplexSquareRoot.hs" #-}
 DocTest.property
{-# LINE 45 "playground/Number/ComplexSquareRoot.hs" #-}
     (Laws.homomorphism SR.fromNumber (\x y -> x * (y :: Complex.T Rational)) SR.mul)
 DocTest.printPrefix "Number.ComplexSquareRoot:46: "
{-# LINE 46 "playground/Number/ComplexSquareRoot.hs" #-}
 DocTest.property
{-# LINE 46 "playground/Number/ComplexSquareRoot.hs" #-}
     (Laws.rightIdentity SR.div SR.one . sr)
 DocTest.printPrefix "Number.ComplexSquareRoot:47: "
{-# LINE 47 "playground/Number/ComplexSquareRoot.hs" #-}
 DocTest.property
{-# LINE 47 "playground/Number/ComplexSquareRoot.hs" #-}
     (\x -> not (isZero x) ==> SR.recip (SR.recip x) == sr x)
 DocTest.printPrefix "Number.ComplexSquareRoot:48: "
{-# LINE 48 "playground/Number/ComplexSquareRoot.hs" #-}
 DocTest.property
{-# LINE 48 "playground/Number/ComplexSquareRoot.hs" #-}
     (\x -> not (isZero x) ==> Laws.inverse SR.mul SR.recip SR.one (sr x))
