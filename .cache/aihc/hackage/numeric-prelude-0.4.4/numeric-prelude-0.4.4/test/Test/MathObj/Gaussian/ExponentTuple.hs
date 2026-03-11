-- Do not edit! Automatically created with doctest-extract from gaussian/MathObj/Gaussian/ExponentTuple.hs
{-# LINE 14 "gaussian/MathObj/Gaussian/ExponentTuple.hs" #-}

module Test.MathObj.Gaussian.ExponentTuple where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 15 "gaussian/MathObj/Gaussian/ExponentTuple.hs" #-}
import     MathObj.Gaussian.ExponentTuple (HoelderConjugates(HoelderConjugates))
import     MathObj.Gaussian.ExponentTuple (YoungConjugates(YoungConjugates))
import     NumericPrelude.Base as P
import     NumericPrelude.Numeric as NP
import     Prelude ()

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.Gaussian.ExponentTuple:26: "
{-# LINE 26 "gaussian/MathObj/Gaussian/ExponentTuple.hs" #-}
 DocTest.property
{-# LINE 26 "gaussian/MathObj/Gaussian/ExponentTuple.hs" #-}
     (\(HoelderConjugates p q)  ->  p>=1 && q>=1 && 1/p + 1/q == 1)
 DocTest.printPrefix "MathObj.Gaussian.ExponentTuple:53: "
{-# LINE 53 "gaussian/MathObj/Gaussian/ExponentTuple.hs" #-}
 DocTest.property
{-# LINE 53 "gaussian/MathObj/Gaussian/ExponentTuple.hs" #-}
     (\(YoungConjugates p q r)  ->  p>=1 && q>=1 && r>=1 && 1/p + 1/q == 1/r + 1)
