{-# LANGUAGE NoImplicitPrelude #-}
{- |
These are pieces that can be assembled to a control curve.
This was formerly part of the @Control@ module
but because of the overlap with immediate control curve generators
I created a new module.
-}
module Synthesizer.Generic.Piece (
   T, run,
   step, linear, exponential,
   cosine, halfSine, cubic,
   FlatPosition(..),
   ) where

import qualified Synthesizer.Piecewise as Piecewise
import Synthesizer.Piecewise (FlatPosition (FlatLeft, FlatRight))

import qualified Synthesizer.Generic.Control as Ctrl
import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.Generic.Signal as SigG
import Synthesizer.Generic.Displacement (raise, )

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.RealField             as RealField
import qualified Algebra.Field                 as Field

import NumericPrelude.Numeric
import NumericPrelude.Base



{-# INLINE run #-}
run :: (RealField.C a, CutG.Transform (sig a)) =>
   SigG.LazySize ->
   Piecewise.T a a (SigG.LazySize -> a -> sig a) ->
   sig a
run lazySize xs =
   SigG.concat $ zipWith
      (\(n, t) (Piecewise.PieceData c yi0 yi1 d) ->
           SigG.take n $ Piecewise.computePiece c yi0 yi1 d lazySize t)
      (Piecewise.splitDurations $ map Piecewise.pieceDur xs)
      xs


type T sig a =
   Piecewise.Piece a a
      (SigG.LazySize -> a {- fractional start time -} -> sig a)


{-# INLINE step #-}
step :: (SigG.Write sig a) => T sig a
step =
   Piecewise.pieceFromFunction $ \ y0 _y1 _d lazySize _t0 ->
      Ctrl.constant lazySize y0

{-# INLINE linear #-}
linear :: (Field.C a, SigG.Write sig a) => T sig a
linear =
   Piecewise.pieceFromFunction $ \ y0 y1 d lazySize t0 ->
      let s = (y1-y0)/d
      in  Ctrl.linear lazySize s (y0-t0*s)

{-# INLINE exponential #-}
exponential :: (Trans.C a, SigG.Write sig a) => a -> T sig a
exponential saturation =
   Piecewise.pieceFromFunction $ \ y0 y1 d lazySize t0 ->
      let y0' = y0-saturation
          y1' = y1-saturation
          yd  = y0'/y1'
      in  raise saturation
             (Ctrl.exponential lazySize (d / log yd) (y0' * yd**(t0/d)))

{-# INLINE cosine #-}
cosine :: (Trans.C a, SigG.Write sig a) => T sig a
cosine =
   Piecewise.pieceFromFunction $ \ y0 y1 d lazySize t0 ->
      SigG.map
         (\y -> ((1+y)*y0+(1-y)*y1)/2)
         (Ctrl.cosine lazySize t0 (t0+d))


{- |
> Graphics.Gnuplot.Simple.plotList [] $ Sig.toList $ run $ 1 |# (10.9, halfSine FlatRight) #| 2
-}
{-# INLINE halfSine #-}
halfSine :: (Trans.C a, SigG.Write sig a) => FlatPosition -> T sig a
halfSine FlatLeft =
   Piecewise.pieceFromFunction $ \ y0 y1 d lazySize t0 ->
      SigG.map
         (\y -> y*y0 + (1-y)*y1)
         (Ctrl.cosine lazySize t0 (t0+2*d))
halfSine FlatRight =
   Piecewise.pieceFromFunction $ \ y0 y1 d lazySize t0 ->
      SigG.map
         (\y -> (1+y)*y0 - y*y1)
         (Ctrl.cosine lazySize (t0-d) (t0+d))


{-# INLINE cubic #-}
cubic :: (Field.C a, SigG.Write sig a) => a -> a -> T sig a
cubic yd0 yd1 =
   Piecewise.pieceFromFunction $ \ y0 y1 d lazySize t0 ->
      Ctrl.cubicHermite lazySize (t0,(y0,yd0)) (t0+d,(y1,yd1))
