-- Do not edit! Automatically created with doctest-extract from gaussian/MathObj/Gaussian/Bell.hs
{-# LINE 30 "gaussian/MathObj/Gaussian/Bell.hs" #-}

module Test.MathObj.Gaussian.Bell where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 31 "gaussian/MathObj/Gaussian/Bell.hs" #-}
import     qualified MathObj.Gaussian.Bell as G
import     qualified Algebra.ZeroTestable as ZeroTestable
import     qualified Algebra.Laws as Laws
import     qualified Number.Complex as Complex
import     Number.Complex ((+:))
import     NumericPrelude.Base as P
import     NumericPrelude.Numeric as NP
import     Prelude ()
import     qualified Test.QuickCheck as QC
import     Data.Function.HT (Id, nest)

asRational     :: Id (G.T Rational)
asRational     = id

withRational     :: Id (G.T Rational -> a)
withRational     = id

isConstant     :: ZeroTestable.C a => G.T a -> Bool
isConstant     (G.Cons _amp _a b c) = isZero b && isZero c

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.Gaussian.Bell:108: "
{-# LINE 108 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 108 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (Laws.identity G.multiply G.constant . asRational)
 DocTest.printPrefix "MathObj.Gaussian.Bell:109: "
{-# LINE 109 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 109 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (Laws.commutative G.multiply . asRational)
 DocTest.printPrefix "MathObj.Gaussian.Bell:110: "
{-# LINE 110 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 110 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (Laws.associative G.multiply . asRational)
 DocTest.printPrefix "MathObj.Gaussian.Bell:152: "
{-# LINE 152 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 152 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (Laws.commutative G.convolve . asRational)
 DocTest.printPrefix "MathObj.Gaussian.Bell:153: "
{-# LINE 153 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 153 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (Laws.associative G.convolve . asRational)
 DocTest.printPrefix "MathObj.Gaussian.Bell:161: "
{-# LINE 161 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 161 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (isConstant . G.convolve G.constant . asRational)
 DocTest.printPrefix "MathObj.Gaussian.Bell:149: "
{-# LINE 149 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.example
{-# LINE 149 "gaussian/MathObj/Gaussian/Bell.hs" #-}
   (let x=G.Cons 2 (1+:3) (4+:5) (7::Rational); y=G.Cons 7 (1+:4) (3+:2) (5::Rational) in G.convolve x y)
  [ExpectedLine [LineChunk "Cons {amp = 7 % 6, c0 = 13 % 6 +: 55 % 8, c1 = 41 % 12 +: 13 % 4, c2 = 35 % 12}"]]
 DocTest.printPrefix "MathObj.Gaussian.Bell:200: "
{-# LINE 200 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 200 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x y -> G.convolve x y == G.convolveByTranslation x y)
 DocTest.printPrefix "MathObj.Gaussian.Bell:217: "
{-# LINE 217 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 217 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x y -> G.convolve x y == G.convolveByFourier x y)
 DocTest.printPrefix "MathObj.Gaussian.Bell:225: "
{-# LINE 225 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 225 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x y -> G.fourier (G.convolve x y) == G.multiply (G.fourier x) (G.fourier y))
 DocTest.printPrefix "MathObj.Gaussian.Bell:226: "
{-# LINE 226 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 226 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x -> nest 2 G.fourier x == G.reverse x)
 DocTest.printPrefix "MathObj.Gaussian.Bell:227: "
{-# LINE 227 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 227 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (G.fourier G.unit == (asRational G.unit))
 DocTest.printPrefix "MathObj.Gaussian.Bell:228: "
{-# LINE 228 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 228 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x a -> G.fourier (G.translate a x) == G.modulate a (G.fourier x))
 DocTest.printPrefix "MathObj.Gaussian.Bell:229: "
{-# LINE 229 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 229 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.fourier (G.dilate a x) == G.amplify a (G.shrink a (G.fourier x)))
 DocTest.printPrefix "MathObj.Gaussian.Bell:244: "
{-# LINE 244 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 244 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x -> G.fourier x == G.fourierByTranslation x)
 DocTest.printPrefix "MathObj.Gaussian.Bell:312: "
{-# LINE 312 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 312 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x a b -> G.translate a (G.translate b x) == G.translate (a+b) x)
 DocTest.printPrefix "MathObj.Gaussian.Bell:326: "
{-# LINE 326 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 326 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x a b -> G.translateComplex a (G.translateComplex b x) == G.translateComplex (a+b) x)
 DocTest.printPrefix "MathObj.Gaussian.Bell:327: "
{-# LINE 327 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 327 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x a -> G.translateComplex (Complex.fromReal a) x == G.translate a x)
 DocTest.printPrefix "MathObj.Gaussian.Bell:341: "
{-# LINE 341 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 341 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x a b -> G.modulate a (G.modulate b x) == G.modulate (a+b) x)
 DocTest.printPrefix "MathObj.Gaussian.Bell:342: "
{-# LINE 342 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 342 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x a b -> G.modulate b (G.translate a x) == G.turn (a*b) (G.translate a (G.modulate b x)))
 DocTest.printPrefix "MathObj.Gaussian.Bell:361: "
{-# LINE 361 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 361 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x -> nest 2 G.reverse x == x)
 DocTest.printPrefix "MathObj.Gaussian.Bell:369: "
{-# LINE 369 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 369 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x (QC.Positive a) (QC.Positive b) -> G.dilate a (G.dilate b x) == G.dilate (a*b) x)
 DocTest.printPrefix "MathObj.Gaussian.Bell:370: "
{-# LINE 370 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 370 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.shrink a x == G.dilate (recip a) x)
 DocTest.printPrefix "MathObj.Gaussian.Bell:381: "
{-# LINE 381 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 381 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.dilate a (G.shrink a x) == x)
 DocTest.printPrefix "MathObj.Gaussian.Bell:382: "
{-# LINE 382 "gaussian/MathObj/Gaussian/Bell.hs" #-}
 DocTest.property
{-# LINE 382 "gaussian/MathObj/Gaussian/Bell.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.shrink a (G.dilate a x) == x)
