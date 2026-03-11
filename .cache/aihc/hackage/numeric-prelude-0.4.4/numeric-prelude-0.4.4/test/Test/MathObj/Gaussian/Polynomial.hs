-- Do not edit! Automatically created with doctest-extract from gaussian/MathObj/Gaussian/Polynomial.hs
{-# LINE 60 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}

{-# OPTIONS_GHC -XRebindableSyntax #-}

module Test.MathObj.Gaussian.Polynomial where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 63 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
import     qualified MathObj.Gaussian.Polynomial as G
import     qualified MathObj.Gaussian.Bell as Bell
import     qualified MathObj.Polynomial as Poly
import     qualified Algebra.Laws as Laws
import     qualified Number.Complex as Complex
import     Number.Complex ((+:))
import     NumericPrelude.Base as P
import     NumericPrelude.Numeric as NP
import     qualified Test.QuickCheck as QC
import     Data.Function.HT (Id, nest)
import     Data.Tuple.HT (mapSnd)

asRational     :: Id (G.T Rational)
asRational     = id

withRational     :: Id (G.T Rational -> a)
withRational     = id

mulLinear2i     :: Id (G.T Rational)
mulLinear2i     x =
       x{G.polynomial = Poly.fromCoeffs [0, 0+:2] * G.polynomial x}

rotateQuarter     :: Int -> Id (G.T Rational)
rotateQuarter     n =
       G.scaleComplex (negate Complex.imaginaryUnit ^ fromIntegral n)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:185: "
{-# LINE 185 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 185 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (QC.forAll (QC.choose (0,3)) $ \n -> G.eigenfunctionDifferential n == asRational (G.eigenfunctionIterative n))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:193: "
{-# LINE 193 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 193 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
          (G.eigenfunction0  ==  asRational (G.eigenfunctionDifferential 0))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:198: "
{-# LINE 198 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 198 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
          (G.eigenfunction1  ==  asRational (G.eigenfunctionDifferential 1))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:203: "
{-# LINE 203 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 203 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
          (G.eigenfunction2  ==  asRational (G.eigenfunctionDifferential 2))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:208: "
{-# LINE 208 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 208 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
          (G.eigenfunction3  ==  asRational (G.eigenfunctionDifferential 3))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:215: "
{-# LINE 215 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 215 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (QC.forAll (QC.choose (0,15)) $ \n -> let x = G.eigenfunctionDifferential n in G.fourier x  ==  rotateQuarter n x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:224: "
{-# LINE 224 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 224 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (QC.forAll (QC.choose (0,15)) $ \n -> let x = G.eigenfunctionIterative n in G.fourier x  ==  rotateQuarter n x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:246: "
{-# LINE 246 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 246 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ Laws.identity G.multiply G.constant)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:247: "
{-# LINE 247 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 247 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ Laws.commutative G.multiply)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:248: "
{-# LINE 248 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 248 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ Laws.associative G.multiply)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:258: "
{-# LINE 258 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 258 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ Laws.commutative G.convolve)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:259: "
{-# LINE 259 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 259 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ Laws.associative G.convolve)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:301: "
{-# LINE 301 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 301 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x y -> G.fourier (G.convolve x y) == G.multiply (G.fourier x) (G.fourier y))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:302: "
{-# LINE 302 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 302 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x -> nest 2 G.fourier x == G.reverse x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:303: "
{-# LINE 303 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 303 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x a -> G.fourier (G.translate a x) == G.modulate a (G.fourier x))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:304: "
{-# LINE 304 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 304 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.fourier (G.dilate a x) == G.amplify a (G.shrink a (G.fourier x)))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:305: "
{-# LINE 305 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 305 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x -> G.fourier (G.differentiate x) == mulLinear2i (G.fourier x))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:323: "
{-# LINE 323 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 323 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x y -> G.convolve (G.differentiate x) y == G.convolve x (G.differentiate y))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:348: "
{-# LINE 348 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 348 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x -> G.integrate (G.differentiate x) == (zero, x))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:349: "
{-# LINE 349 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 349 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x@(G.Cons b p) -> let (xoff,xint) = G.integrate x in G.differentiate xint == G.Cons b (p + Poly.const xoff))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:345: "
{-# LINE 345 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.example
{-# LINE 345 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
   (snd $ G.integrate $ G.differentiate $ G.Cons Bell.unit (Poly.fromCoeffs [7,7,7,7 :: Complex.T Rational]))
  [ExpectedLine [LineChunk "Cons {bell = Cons {amp = 1 % 1, c0 = 0 % 1 +: 0 % 1, c1 = 0 % 1 +: 0 % 1, c2 = 1 % 1}, polynomial = Polynomial.fromCoeffs [7 % 1 +: 0 % 1,7 % 1 +: 0 % 1,7 % 1 +: 0 % 1,7 % 1 +: 0 % 1]}"]]
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:409: "
{-# LINE 409 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 409 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x a b -> G.translate a (G.translate b x) == G.translate (a+b) x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:416: "
{-# LINE 416 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 416 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x a b -> G.translateComplex a (G.translateComplex b x) == G.translateComplex (a+b) x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:417: "
{-# LINE 417 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 417 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x a -> G.translateComplex (Complex.fromReal a) x == G.translate a x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:426: "
{-# LINE 426 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 426 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x a b -> G.modulate a (G.modulate b x) == G.modulate (a+b) x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:427: "
{-# LINE 427 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 427 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x a b -> G.modulate b (G.translate a x) == G.turn (a*b) (G.translate a (G.modulate b x)))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:442: "
{-# LINE 442 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 442 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x -> nest 2 G.reverse x == x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:451: "
{-# LINE 451 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 451 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x (QC.Positive a) (QC.Positive b) -> G.dilate a (G.dilate b x) == G.dilate (a*b) x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:452: "
{-# LINE 452 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 452 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.shrink a x == G.dilate (recip a) x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:461: "
{-# LINE 461 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 461 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.dilate a (G.shrink a x) == x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:462: "
{-# LINE 462 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 462 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.shrink a (G.dilate a x) == x)
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:490: "
{-# LINE 490 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 490 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x (QC.NonZero unit) d -> G.approximateByBells unit (G.translateComplex d x) == map (mapSnd (Bell.translateComplex d)) (G.approximateByBells unit x))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:491: "
{-# LINE 491 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 491 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x (QC.NonZero unit) (QC.NonZero d) -> G.approximateByBells unit (G.dilate d x) == map (mapSnd (Bell.dilate d)) (G.approximateByBells (unit/d) x))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:492: "
{-# LINE 492 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 492 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (withRational $ \x (QC.NonZero unit) (QC.NonZero d) -> G.approximateByBells unit (G.shrink d x) == map (mapSnd (Bell.shrink d)) (G.approximateByBells (unit*d) x))
 DocTest.printPrefix "MathObj.Gaussian.Polynomial:512: "
{-# LINE 512 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
 DocTest.property
{-# LINE 512 "gaussian/MathObj/Gaussian/Polynomial.hs" #-}
     (\(QC.NonZero unit) d s p0 -> let p = Poly.fromCoeffs $ take 10 p0 in G.approximateByBellsAtOnce unit d s p == G.approximateByBellsByTranslation unit d (s::Rational) p)
