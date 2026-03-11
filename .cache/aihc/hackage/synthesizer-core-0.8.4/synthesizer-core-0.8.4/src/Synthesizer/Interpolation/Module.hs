{-# LANGUAGE NoImplicitPrelude #-}
{- |
Special interpolations defined in terms of Module operations.
-}
module Synthesizer.Interpolation.Module (
   T,
   constant,
   linear,
   cubic,
   cubicAlt,
   piecewise,
   piecewiseConstant,
   piecewiseLinear,
   piecewiseCubic,
   function,
   ) where

import qualified Synthesizer.State.Signal  as Sig
import qualified Synthesizer.Plain.Control as Ctrl

import qualified Synthesizer.Interpolation.Core as Core

import Synthesizer.Interpolation (
   T, cons, getNode, fromPrefixReader,
   constant,
   )

import qualified Algebra.Module    as Module
import qualified Algebra.Field     as Field

import qualified Control.Applicative.HT as App

import NumericPrelude.Numeric
import NumericPrelude.Base


{-| Consider the signal to be piecewise linear. -}
{-# INLINE linear #-}
linear :: (Module.C t y) => T t y
linear =
   fromPrefixReader "linear" 0
      (App.lift2 Core.linear getNode getNode)

{- |
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
cubic :: (Field.C t, Module.C t y) => T t y
cubic =
   fromPrefixReader "cubic" 1
      (App.lift4 Core.cubic getNode getNode getNode getNode)

{-# INLINE cubicAlt #-}
cubicAlt :: (Field.C t, Module.C t y) => T t y
cubicAlt =
   fromPrefixReader "cubicAlt" 1
      (App.lift4 Core.cubicAlt getNode getNode getNode getNode)



{-** Interpolation based on piecewise defined functions -}

{-# INLINE piecewise #-}
piecewise :: (Module.C t y) =>
   Int -> [t -> t] -> T t y
piecewise center ps =
   cons (length ps) (center-1)
      (\t -> Sig.linearComb (Sig.fromList (map ($ t) (reverse ps))))

{-# INLINE piecewiseConstant #-}
piecewiseConstant :: (Module.C t y) => T t y
piecewiseConstant =
   piecewise 1 [const 1]

{-# INLINE piecewiseLinear #-}
piecewiseLinear :: (Module.C t y) => T t y
piecewiseLinear =
   piecewise 1 [id, (1-)]

{-# INLINE piecewiseCubic #-}
piecewiseCubic :: (Field.C t, Module.C t y) => T t y
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
function :: (Module.C t y) =>
      (Int,Int)   {- ^ @(left extent, right extent)@, e.g. @(1,1)@ for linear hat -}
   -> (t -> t)
   -> T t y
function (left,right) f =
   let len = left+right
       ps  = Sig.take len $ Sig.iterate pred (pred right)
       -- ps = Sig.reverse $ Sig.take len $ Sig.iterate succ (-left)
   in  cons len left
          (\t -> Sig.linearComb $
                   Sig.map (\x -> f (t + fromIntegral x)) ps)
{-
GNUPlot.plotList [] $ take 300 $ interpolate (Zero 0) (function (1,1) (\x -> exp (-6*x*x))) (-2.3 :: Double) (repeat 0.03) [2,1,2::Double]
-}
