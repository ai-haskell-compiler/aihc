{-# LANGUAGE NoImplicitPrelude #-}
{- |
See "Synthesizer.Generic.Piece".
-}
module Synthesizer.State.Piece (
   T, run,
   step, linear, exponential,
   cosine, halfSine, cubic,
   FlatPosition(..),
   ) where

import qualified Synthesizer.Piecewise as Piecewise
import Synthesizer.Piecewise (FlatPosition (FlatLeft, FlatRight))

import qualified Synthesizer.State.Control as Ctrl
import qualified Synthesizer.State.Signal as Sig
import Synthesizer.State.Displacement (raise)

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.RealRing              as RealRing
import qualified Algebra.Field                 as Field

import NumericPrelude.Numeric
import NumericPrelude.Base



{-# INLINE run #-}
run :: (RealRing.C a) => Piecewise.T a a (a -> Sig.T a) -> Sig.T a
run xs =
   Sig.concat $ zipWith
      (\(n, t) (Piecewise.PieceData c yi0 yi1 d) ->
           Sig.take n $ Piecewise.computePiece c yi0 yi1 d t)
      (Piecewise.splitDurations $ map Piecewise.pieceDur xs)
      xs


type T a = Piecewise.Piece a a (a {- fractional start time -} -> Sig.T a)


{-# INLINE step #-}
step :: T a
step =
   Piecewise.pieceFromFunction $ \ y0 _y1 _d _t0 ->
      Ctrl.constant y0

{-# INLINE linear #-}
linear :: (Field.C a) => T a
linear =
   Piecewise.pieceFromFunction $ \ y0 y1 d t0 ->
      let s = (y1-y0)/d in Ctrl.linear s (y0-t0*s)

{-# INLINE exponential #-}
exponential :: (Trans.C a) => a -> T a
exponential saturation =
   Piecewise.pieceFromFunction $ \ y0 y1 d t0 ->
      let y0' = y0-saturation
          y1' = y1-saturation
          yd  = y0'/y1'
      in  raise saturation
             (Ctrl.exponential (d / log yd) (y0' * yd**(t0/d)))

{-# INLINE cosine #-}
cosine :: (Trans.C a) => T a
cosine =
   Piecewise.pieceFromFunction $ \ y0 y1 d t0 ->
      Sig.map
         (\y -> ((1+y)*y0+(1-y)*y1)/2)
         (Ctrl.cosine t0 (t0+d))


{- |
> Graphics.Gnuplot.Simple.plotList [] $ Sig.toList $ Piece.run $ 1 |# (10.9, Piece.halfSine FlatRight) #| 2
-}
{-# INLINE halfSine #-}
halfSine :: (Trans.C a) => FlatPosition -> T a
halfSine FlatLeft =
   Piecewise.pieceFromFunction $ \ y0 y1 d t0 ->
      Sig.map
         (\y -> y*y0 + (1-y)*y1)
         (Ctrl.cosine t0 (t0+2*d))
halfSine FlatRight =
   Piecewise.pieceFromFunction $ \ y0 y1 d t0 ->
      Sig.map
         (\y -> (1+y)*y0 - y*y1)
         (Ctrl.cosine (t0-d) (t0+d))


{-# INLINE cubic #-}
cubic :: (Field.C a) => a -> a -> T a
cubic yd0 yd1 =
   Piecewise.pieceFromFunction $ \ y0 y1 d t0 ->
      Ctrl.cubicHermite (t0,(y0,yd0)) (t0+d,(y1,yd1))
