{-# LANGUAGE NoImplicitPrelude #-}
{- |
Special interpolations defined in terms of our custom Interpolation class.
-}
module Synthesizer.Interpolation.Custom (
   T,
   constant,
   linear,
   cubic,
   piecewise,
   piecewiseConstant,
   piecewiseLinear,
   piecewiseCubic,
   function,
   ) where

import qualified Synthesizer.State.Signal  as Sig
import qualified Synthesizer.Plain.Control as Ctrl
import qualified Synthesizer.Interpolation.Class as Interpol

import Synthesizer.Interpolation (
   T, cons, getNode, fromPrefixReader,
   constant,
   )

import qualified Algebra.Field     as Field

import Synthesizer.Interpolation.Class ((+.*), )

import qualified Control.Applicative.HT as App

import NumericPrelude.Numeric
import NumericPrelude.Base



{-| Consider the signal to be piecewise linear. -}
{-# INLINE linear #-}
linear :: (Interpol.C t y) => T t y
linear =
   fromPrefixReader "linear" 0
      (App.lift2
          (\x0 x1 phase -> Interpol.combine2 phase (x0,x1))
          getNode getNode)

{-|
Consider the signal to be piecewise cubic,
with smooth connections at the nodes.
It uses a cubic curve which has node values
x0 at 0 and x1 at 1 and derivatives
(x1-xm1)/2 and (x2-x0)/2, respectively.
You can see how it works
if you evaluate the expression for t=0 and t=1
as well as the derivative at these points.
-}
{-# INLINE cubic #-}
cubic :: (Field.C t, Interpol.C t y) => T t y
cubic =
   fromPrefixReader "cubicAlt" 1 $ App.lift4
      (\xm1 x0 x1 x2 t ->
       let (am1, a0, a1) = cubicHalf    t
           ( b2, b1, b0) = cubicHalf (1-t)
       in  Interpol.scale (am1,xm1)
             +.* (a0+b0,x0)
             +.* (a1+b1,x1)
             +.* (b2,x2))
      getNode getNode getNode getNode

{- |
See 'cubicHalfModule'.
-}
{-# INLINE cubicHalf #-}
cubicHalf :: (Field.C t) => t -> (t,t,t)
cubicHalf t =
   let c = (t-1)^2
       ct2 = c*t/2
   in  (-ct2, c*(1+2*t), ct2)


{-** Interpolation based on piecewise defined functions -}

{- |
List of functions must be non-empty.
-}
{-# INLINE piecewise #-}
piecewise :: (Interpol.C t y) =>
   Int -> [t -> t] -> T t y
piecewise center ps =
   cons (length ps) (center-1) $
   \t ->
      combineMany
         "Interpolation.element: list of functions empty"
         "Interpolation.element: list of samples empty" $
            Sig.map ($ t) $ Sig.fromList $ reverse ps

{-# INLINE piecewiseConstant #-}
piecewiseConstant :: (Interpol.C t y) => T t y
piecewiseConstant =
   piecewise 1 [const 1]

{-# INLINE piecewiseLinear #-}
piecewiseLinear :: (Interpol.C t y) => T t y
piecewiseLinear =
   piecewise 1 [id, (1-)]

{-# INLINE piecewiseCubic #-}
piecewiseCubic :: (Field.C t, Interpol.C t y) => T t y
piecewiseCubic =
   piecewise 2 $
      Ctrl.cubicFunc (0,(0,0))    (1,(0,1/2)) :
      Ctrl.cubicFunc (0,(0,1/2))  (1,(1,0)) :
      Ctrl.cubicFunc (0,(1,0))    (1,(0,-1/2)) :
      Ctrl.cubicFunc (0,(0,-1/2)) (1,(0,0)) :
      []

{-
GNUPlot.plotList [] $ take 100 $ interpolate (Zero 0) piecewiseCubic (-2.3 :: Double) (repeat 0.1) [2,1,2::Double]
-}


{-** Interpolation based on arbitrary functions -}

{- | with this wrapper you can use the collection of interpolating functions from Donadio's DSP library -}
{-# INLINE function #-}
function :: (Interpol.C t y) =>
      (Int,Int)   {- ^ @(left extent, right extent)@, e.g. @(1,1)@ for linear hat -}
   -> (t -> t)
   -> T t y
function (left,right) f =
   let len = left+right
       ps  = Sig.take len $ Sig.iterate pred (pred right)
       -- ps = Sig.reverse $ Sig.take len $ Sig.iterate succ (-left)
   in  cons len left $
       \t ->
          combineMany
             "Interpolation.function: empty function domain"
             "Interpolation.function: list of samples empty" $
             Sig.map (\x -> f (t + fromIntegral x)) ps
{-
GNUPlot.plotList [] $ take 300 $ interpolate (Zero 0) (function (1,1) (\x -> exp (-6*x*x))) (-2.3 :: Double) (repeat 0.03) [2,1,2::Double]
-}

combineMany ::
   (Interpol.C a v) =>
   String -> String ->
   Sig.T a -> Sig.T v -> v
combineMany msgCoefficients msgSamples ct xt =
   Sig.switchL (error msgCoefficients)
      (\c cs ->
         Sig.switchL (error msgSamples)
            (curry (Interpol.combineMany (c,cs)))
            xt)
      ct
