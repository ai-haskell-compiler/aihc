{-# LANGUAGE RebindableSyntax #-}
{-
Reciprocal of variance of a Gaussian bell curve.
We describe the curve only in terms of its variance
thus we represent a bell curve at the coordinate origin
neglecting its amplitude.

We could also define the amplitude as @root 4 c@,
thus preserving L2 norm being one,
but then @dilate@ and @shrink@ also include an amplification.

We could do some projective geometry in the exponent
in order to also have zero variance,
which corresponds to the dirac impulse.
-}
module MathObj.Gaussian.Example where

import qualified MathObj.Gaussian.Polynomial as PolyBell
import qualified MathObj.Gaussian.Bell as Bell
import qualified MathObj.Gaussian.Variance as Var

import qualified MathObj.Polynomial as Poly

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Algebraic      as Algebraic
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring

import qualified Number.Complex as Complex
import qualified Number.Root as Root

import Algebra.Transcendental (pi, )
import Algebra.Algebraic (root, )
import Algebra.Ring ((*), (^), )

import Number.Complex ((+:), )

import qualified Numerics.Function as Func
import qualified Numerics.Fourier as Fourier
import qualified Numerics.Integration as Integ
import qualified Numerics.Differentiation as Diff

import qualified Graphics.Gnuplot.Simple as GP

import Control.Applicative (liftA2, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import qualified Prelude as P


curve0 :: Var.T Double
curve0 = curve0a

curve0a :: Var.T Double
curve0a = Var.Cons 1.4 3.3

curve0b :: Var.T Double
curve0b = Var.Cons 2.2 1.7

variance0 :: (Double, Double)
variance0 =
   (Var.variance curve0,
    (Integ.rectangular 1000 (-2,2) $ liftA2 (*) (^2) (Var.evaluate curve0)) /
    (Integ.rectangular 1000 (-2,2) $ Var.evaluate curve0))

norm10 :: (Double, Double, Double)
norm10 =
   (Integ.rectangular 1000 (-2,2) $ Var.evaluate curve0,
    Var.norm1 curve0,
    Root.toNumber (Var.norm1Root curve0))

norm20 :: (Double, Double, Double)
norm20 =
   (sqrt $ Integ.rectangular 1000 (-2,2) $ (^2) . Var.evaluate curve0,
    Var.norm2 curve0,
    Root.toNumber (Var.norm2Root curve0))

norm30 :: (Double, Double, Double)
norm30 =
   (root 3 $ Integ.rectangular 1000 (-2,2) $ (^3) . Var.evaluate curve0,
    Var.normP 3 curve0,
    Root.toNumber (Var.normPRoot 3 curve0))

fourier0 :: IO ()
fourier0 =
   GP.plotFuncs []
      (GP.linearScale 100 (-2,2))
      [Var.evaluate $ Var.fourier curve0,
       Fourier.analysisTransformOneReal 100 (-2,2) $ Var.evaluate curve0]

multiply0 :: IO ()
multiply0 =
   GP.plotFuncs []
      (GP.linearScale 100 (-1,1))
      [Var.evaluate $ Var.multiply curve0a curve0b,
       liftA2 (*) (Var.evaluate curve0a) (Var.evaluate curve0b)]

convolve0 :: IO ()
convolve0 =
   GP.plotFuncs []
      (GP.linearScale 100 (-2,2))
      [Var.evaluate $ Var.convolve curve0a curve0b,
       Integ.convolve 1000 (-3,3) (Var.evaluate curve0a) (Var.evaluate curve0b)]


curve1 :: Bell.T Double
curve1 = curve1a

curve1a :: Bell.T Double
curve1a = Bell.Cons 1.4 (0.1+:0.3) ((-0.2)+:1.4) 2.3

curve1b :: Bell.T Double
curve1b = Bell.Cons 2.2 ((-0.3)+:2.1) (0.2+:(-0.4)) 1.7

variance1 :: (Double, Double)
variance1 =
   (Bell.variance curve1,
    (Integ.rectangular 1000 (-2,2) $
        liftA2 (*) (^2)
           (Complex.magnitudeSqr .
            Func.translateRight
               (Complex.real (Bell.c1 curve1) / (2 * Bell.c2 curve1))
               (Bell.evaluate curve1))) /
    (Integ.rectangular 1000 (-2,2) $ Complex.magnitude . Bell.evaluate curve1))

{- the norm depends on too much things
norm0vs1 :: (Double, Double)
norm0vs1 =
   ((Integ.rectangular 1000 (-5,5) $ Var.evaluate curve0)
         * exp (- Complex.real (Bell.c0 curve1)),
    Integ.rectangular 1000 (-5,5) $ Complex.magnitude . Bell.evaluate curve1)
-}

fourier1 :: IO ()
fourier1 =
   GP.plotFuncs []
      (GP.linearScale 100 (-5,5))
      [Complex.real . (Bell.evaluate $ Bell.fourier curve1),
       fourierAnalysisReal 100 (-2,2) $ Bell.evaluate curve1]


curve2 :: PolyBell.T Double
curve2 =
   PolyBell.Cons
--      Bell.unit
--      (Bell.Cons 1.4 (0.1+:0.3) 0 1.2)
--      (Bell.Cons 1.4 (0.1+:0.3) ((-0.2)+:1.4) 1)
      curve1
--      (Poly.fromCoeffs [one])
--      (Poly.fromCoeffs [zero,one])
--      (Poly.fromCoeffs [zero,zero,one])
--      (Poly.fromCoeffs [0,Complex.imaginaryUnit])
      (Poly.fromCoeffs [1.4+:(-0.1),0.8+:(0.1),(-1.1)+:0.3])

differentiate2 :: IO ()
differentiate2 =
   GP.plotFuncs []
      (GP.linearScale 100 (-2,2))
      [Complex.real . (PolyBell.evaluateSqRt $ PolyBell.differentiate curve2),
       ((/ sqrt pi) . ) $ Diff.diff (1e-5) $ Complex.real . PolyBell.evaluateSqRt curve2]

fourier2 :: IO ()
fourier2 =
   GP.plotFuncs []
      (GP.linearScale 100 (-5,5))
      [Complex.real . (PolyBell.evaluateSqRt $ PolyBell.fourier curve2),
       fourierAnalysisReal 100 (-2,2) $ PolyBell.evaluateSqRt curve2]



fourierAnalysisReal ::
   (P.Floating a) =>
   Integer -> (a, a) -> (a -> Complex.T a) -> a -> a
fourierAnalysisReal n rng f =
   liftA2 (P.-)
      (Fourier.analysisTransformOneReal n rng (Complex.real . f))
      (Fourier.analysisTransformOneImag n rng (Complex.imag . f))


{- |
Try to approximate @\x -> exp (-x^2) * x@
by a difference of translated Gaussian bells.

exp(-x^2) * x
  ==  exp(-(a+b*x+c*x^2)) - exp(-(a-b*x+c*x^2))
  ==  exp(-(a+c*x^2)) * (exp(-b*x) - exp(b*x))
  ==  exp(-(a+c*x^2)) * 2*sinh (b*x)

It holds
  lim (\b x -> sinh (b*x) / b)  =  id
-}
diffApprox :: IO ()
diffApprox =
   let amp = (2*b)^- (-2)
       a = 0
       {-
       amp = 1
       a = log (2 * abs b)
       -}
       b = -0.1
       c = 1
       ac = Complex.fromReal a
       bc = Complex.fromReal b
   in  GP.plotFuncs []
          (GP.linearScale 100 (-2,2::Double))
          [Complex.real .
           (PolyBell.evaluateSqRt $
              PolyBell.Cons Bell.unit (Poly.fromCoeffs [zero,one])),
           Complex.real .
           liftA2 (-)
             (PolyBell.evaluateSqRt $
                PolyBell.Cons (Bell.Cons amp ac bc c) (Poly.fromCoeffs [one]))
             (PolyBell.evaluateSqRt $
                PolyBell.Cons (Bell.Cons amp ac (-bc) c) (Poly.fromCoeffs [one]))]


polyApprox :: IO ()
polyApprox =
   GP.plotFuncs []
      (GP.linearScale 100 (-2,2::Double))
      [Complex.real .
         PolyBell.evaluateSqRt curve2,
       Complex.real . sum .
         mapM (\(amp,b) -> \x -> amp * Bell.evaluateSqRt b x)
         (PolyBell.approximateByBells 0.1 curve2)]
