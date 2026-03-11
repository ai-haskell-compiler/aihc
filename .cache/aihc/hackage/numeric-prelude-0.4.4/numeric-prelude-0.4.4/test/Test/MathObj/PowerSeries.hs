-- Do not edit! Automatically created with doctest-extract from src/MathObj/PowerSeries.hs
{-# LINE 30 "src/MathObj/PowerSeries.hs" #-}

module Test.MathObj.PowerSeries where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 31 "src/MathObj/PowerSeries.hs" #-}
import     qualified MathObj.PowerSeries.Core as PS
import     qualified MathObj.PowerSeries as PST
import     qualified Test.QuickCheck as QC
import     Test.NumericPrelude.Utility (equalTrunc, (/\))
import     NumericPrelude.Numeric as NP
import     NumericPrelude.Base as P
import     Prelude ()

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.PowerSeries:141: "
{-# LINE 141 "src/MathObj/PowerSeries.hs" #-}
 DocTest.property
{-# LINE 141 "src/MathObj/PowerSeries.hs" #-}
     (QC.choose (1,10) /\ \expon (QC.Positive x) xs -> let xt = x:xs in  equalTrunc 15 (PS.pow (const x) (1 % expon) (PST.coeffs (PST.fromCoeffs xt ^ expon)) ++ repeat zero) (xt ++ repeat zero))
