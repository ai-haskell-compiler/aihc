-- Do not edit! Automatically created with doctest-extract from src/MathObj/PowerSeries/Example.hs
{-# LINE 21 "src/MathObj/PowerSeries/Example.hs" #-}

module Test.MathObj.PowerSeries.Example where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 22 "src/MathObj/PowerSeries/Example.hs" #-}
import     qualified MathObj.PowerSeries.Core as PS
import     qualified MathObj.PowerSeries.Example as PSE
import     Test.NumericPrelude.Utility (equalTrunc)
import     NumericPrelude.Numeric as NP
import     NumericPrelude.Base as P
import     Prelude ()

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.PowerSeries.Example:55: "
{-# LINE 55 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 55 "src/MathObj/PowerSeries/Example.hs" #-}
          (\m n -> equalTrunc 30 (PS.mul (PSE.pow m) (PSE.pow n)) (PSE.pow (m+n)))
 DocTest.printPrefix "MathObj.PowerSeries.Example:66: "
{-# LINE 66 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 66 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 500 PSE.expExpl PSE.expODE)
 DocTest.printPrefix "MathObj.PowerSeries.Example:69: "
{-# LINE 69 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 69 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 500 PSE.sinExpl PSE.sinODE)
 DocTest.printPrefix "MathObj.PowerSeries.Example:72: "
{-# LINE 72 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 72 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 500 PSE.cosExpl PSE.cosODE)
 DocTest.printPrefix "MathObj.PowerSeries.Example:76: "
{-# LINE 76 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 76 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 50 PSE.tanExpl PSE.tanODE)
 DocTest.printPrefix "MathObj.PowerSeries.Example:80: "
{-# LINE 80 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 80 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 50 PSE.tanExpl PSE.tanExplSieve)
 DocTest.printPrefix "MathObj.PowerSeries.Example:87: "
{-# LINE 87 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 87 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 500 PSE.logExpl PSE.logODE)
 DocTest.printPrefix "MathObj.PowerSeries.Example:90: "
{-# LINE 90 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 90 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 500 PSE.atanExpl PSE.atanODE)
 DocTest.printPrefix "MathObj.PowerSeries.Example:94: "
{-# LINE 94 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 94 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 500 PSE.sinhExpl PSE.sinhODE)
 DocTest.printPrefix "MathObj.PowerSeries.Example:97: "
{-# LINE 97 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 97 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 500 PSE.coshExpl PSE.coshODE)
 DocTest.printPrefix "MathObj.PowerSeries.Example:100: "
{-# LINE 100 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 100 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 500 PSE.atanhExpl PSE.atanhODE)
 DocTest.printPrefix "MathObj.PowerSeries.Example:106: "
{-# LINE 106 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 106 "src/MathObj/PowerSeries/Example.hs" #-}
          (\expon -> equalTrunc 50 (PSE.powODE expon) (PSE.powExpl expon))
 DocTest.printPrefix "MathObj.PowerSeries.Example:112: "
{-# LINE 112 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 112 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 100 PSE.sqrtExpl PSE.sqrtODE)
 DocTest.printPrefix "MathObj.PowerSeries.Example:149: "
{-# LINE 149 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 149 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 50 PSE.tanODE PSE.tanODESieve)
 DocTest.printPrefix "MathObj.PowerSeries.Example:165: "
{-# LINE 165 "src/MathObj/PowerSeries/Example.hs" #-}
 DocTest.property
{-# LINE 165 "src/MathObj/PowerSeries/Example.hs" #-}
          (equalTrunc 50 PSE.asinODE (snd $ PS.inv PSE.sinODE))
