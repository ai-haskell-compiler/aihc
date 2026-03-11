-- Do not edit! Automatically created with doctest-extract from src/MathObj/PowerSeries/Core.hs
{-# LINE 23 "src/MathObj/PowerSeries/Core.hs" #-}

module Test.MathObj.PowerSeries.Core where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 24 "src/MathObj/PowerSeries/Core.hs" #-}
import     qualified MathObj.PowerSeries.Core as PS
import     qualified MathObj.PowerSeries.Example as PSE
import     Test.NumericPrelude.Utility (equalTrunc, (/\))
import     qualified Test.QuickCheck as QC
import     NumericPrelude.Numeric as NP
import     NumericPrelude.Base as P
import     Prelude ()
import     Control.Applicative (liftA3)

checkHoles     ::
       Int -> ([Rational] -> [Rational]) ->
       Rational -> [Rational] -> QC.Property
checkHoles     trunc f x xs =
       QC.choose (1,10) /\ \expon ->
       equalTrunc trunc
          (f (PS.insertHoles expon (x:xs)) ++ repeat zero)
          (PS.insertHoles expon (f (x:xs)) ++ repeat zero)

genInvertible     :: QC.Gen [Rational]
genInvertible     =
       liftA3 (\x0 x1 xs -> x0:x1:xs)
          QC.arbitrary (fmap QC.getNonZero QC.arbitrary) QC.arbitrary

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.PowerSeries.Core:108: "
{-# LINE 108 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 108 "src/MathObj/PowerSeries/Core.hs" #-}
     (QC.choose (1,10) /\ \m -> QC.choose (1,10) /\ \n xs -> equalTrunc 100 (PS.insertHoles m $ PS.insertHoles n xs) (PS.insertHoles (m*n) xs))
 DocTest.printPrefix "MathObj.PowerSeries.Core:190: "
{-# LINE 190 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 190 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 50 PSE.sqrtExpl (PS.sqrt (\1 -> 1) [1,1]))
 DocTest.printPrefix "MathObj.PowerSeries.Core:191: "
{-# LINE 191 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 191 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 500 (1:1:repeat 0) (PS.sqrt (\1 -> 1) (PS.mul [1,1] [1,1])))
 DocTest.printPrefix "MathObj.PowerSeries.Core:192: "
{-# LINE 192 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 192 "src/MathObj/PowerSeries/Core.hs" #-}
     (checkHoles 50 (PS.sqrt (\1 -> 1)) 1)
 DocTest.printPrefix "MathObj.PowerSeries.Core:217: "
{-# LINE 217 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 217 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 100 (PSE.powExpl (-1/3)) (PS.pow (\1 -> 1) (-1/3) [1,1]))
 DocTest.printPrefix "MathObj.PowerSeries.Core:218: "
{-# LINE 218 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 218 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 50 (PSE.powExpl (-1/3)) (PS.exp (\0 -> 1) (PS.scale (-1/3) PSE.log)))
 DocTest.printPrefix "MathObj.PowerSeries.Core:219: "
{-# LINE 219 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 219 "src/MathObj/PowerSeries/Core.hs" #-}
     (checkHoles 30 (PS.pow (\1 -> 1) (1/3)) 1)
 DocTest.printPrefix "MathObj.PowerSeries.Core:220: "
{-# LINE 220 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 220 "src/MathObj/PowerSeries/Core.hs" #-}
     (checkHoles 30 (PS.pow (\1 -> 1) (2/5)) 1)
 DocTest.printPrefix "MathObj.PowerSeries.Core:237: "
{-# LINE 237 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 237 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 500 PSE.expExpl (PS.exp (\0 -> 1) [0,1]))
 DocTest.printPrefix "MathObj.PowerSeries.Core:238: "
{-# LINE 238 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 238 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 100 (1:1:repeat 0) (PS.exp (\0 -> 1) PSE.log))
 DocTest.printPrefix "MathObj.PowerSeries.Core:239: "
{-# LINE 239 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 239 "src/MathObj/PowerSeries/Core.hs" #-}
     (checkHoles 30 (PS.exp (\0 -> 1)) 0)
 DocTest.printPrefix "MathObj.PowerSeries.Core:259: "
{-# LINE 259 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 259 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 500 PSE.sinExpl (PS.sin (\0 -> (0,1)) [0,1]))
 DocTest.printPrefix "MathObj.PowerSeries.Core:260: "
{-# LINE 260 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 260 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 50 (0:1:repeat 0) (PS.sin (\0 -> (0,1)) PSE.asin))
 DocTest.printPrefix "MathObj.PowerSeries.Core:261: "
{-# LINE 261 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 261 "src/MathObj/PowerSeries/Core.hs" #-}
     (checkHoles 20 (PS.sin (\0 -> (0,1))) 0)
 DocTest.printPrefix "MathObj.PowerSeries.Core:266: "
{-# LINE 266 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 266 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 500 PSE.cosExpl (PS.cos (\0 -> (0,1)) [0,1]))
 DocTest.printPrefix "MathObj.PowerSeries.Core:267: "
{-# LINE 267 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 267 "src/MathObj/PowerSeries/Core.hs" #-}
     (checkHoles 20 (PS.cos (\0 -> (0,1))) 0)
 DocTest.printPrefix "MathObj.PowerSeries.Core:273: "
{-# LINE 273 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 273 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 50 PSE.tanExpl (PS.tan (\0 -> (0,1)) [0,1]))
 DocTest.printPrefix "MathObj.PowerSeries.Core:274: "
{-# LINE 274 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 274 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 50 (0:1:repeat 0) (PS.tan (\0 -> (0,1)) PSE.atan))
 DocTest.printPrefix "MathObj.PowerSeries.Core:275: "
{-# LINE 275 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 275 "src/MathObj/PowerSeries/Core.hs" #-}
     (checkHoles 20 (PS.tan (\0 -> (0,1))) 0)
 DocTest.printPrefix "MathObj.PowerSeries.Core:289: "
{-# LINE 289 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 289 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 500 PSE.logExpl (PS.log (\1 -> 0) [1,1]))
 DocTest.printPrefix "MathObj.PowerSeries.Core:290: "
{-# LINE 290 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 290 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 100 (0:1:repeat 0) (PS.log (\1 -> 0) PSE.exp))
 DocTest.printPrefix "MathObj.PowerSeries.Core:291: "
{-# LINE 291 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 291 "src/MathObj/PowerSeries/Core.hs" #-}
     (checkHoles 30 (PS.log (\1 -> 0)) 1)
 DocTest.printPrefix "MathObj.PowerSeries.Core:303: "
{-# LINE 303 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 303 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 500 PSE.atan (PS.atan (\0 -> 0) [0,1]))
 DocTest.printPrefix "MathObj.PowerSeries.Core:304: "
{-# LINE 304 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 304 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 50 (0:1:repeat 0) (PS.atan (\0 -> 0) PSE.tan))
 DocTest.printPrefix "MathObj.PowerSeries.Core:305: "
{-# LINE 305 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 305 "src/MathObj/PowerSeries/Core.hs" #-}
     (checkHoles 20 (PS.atan (\0 -> 0)) 0)
 DocTest.printPrefix "MathObj.PowerSeries.Core:313: "
{-# LINE 313 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 313 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 100 (0:1:repeat 0) (PS.asin (\1 -> 1) (\0 -> 0) PSE.sin))
 DocTest.printPrefix "MathObj.PowerSeries.Core:314: "
{-# LINE 314 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 314 "src/MathObj/PowerSeries/Core.hs" #-}
     (equalTrunc 50 PSE.asin (PS.asin (\1 -> 1) (\0 -> 0) [0,1]))
 DocTest.printPrefix "MathObj.PowerSeries.Core:315: "
{-# LINE 315 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 315 "src/MathObj/PowerSeries/Core.hs" #-}
     (checkHoles 30 (PS.asin (\1 -> 1) (\0 -> 0)) 0)
 DocTest.printPrefix "MathObj.PowerSeries.Core:383: "
{-# LINE 383 "src/MathObj/PowerSeries/Core.hs" #-}
 DocTest.property
{-# LINE 383 "src/MathObj/PowerSeries/Core.hs" #-}
     (genInvertible /\ \xs -> let (y,ys) = PS.inv xs; (z,zs) = PS.invDiff xs in y==z && equalTrunc 15 ys zs)
