module Synthesizer.Dimensional.RateAmplitude.Piece (
   {- * Piecewise -}
   step, linear, exponential, cosine, halfSine, cubic,
   T, Sequence, run, runVolume, runState, runStateVolume,
   (-|#), ( #|-), (=|#), ( #|=), (|#), ( #|),  -- spaces before # for Haddock
   Piece.FlatPosition(..),
   ) where

import qualified Synthesizer.Generic.Piece as Piece
import qualified Synthesizer.Generic.Signal as SigG

import qualified Synthesizer.Piecewise as Piecewise
import Synthesizer.Piecewise ((-|#), ( #|-), (=|#), ( #|=), (|#), ( #|), )

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Process as Proc
import Synthesizer.Dimensional.Process
          (toTimeScalar, toGradientScalar, DimensionGradient, )
-- import Synthesizer.Dimensional.Process (($:), ($#), )

import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Synthesizer.State.Signal as Sig

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

-- import Number.DimensionTerm ((&*&))

-- import qualified Algebra.Module             as Module
import qualified Algebra.Transcendental     as Trans
import qualified Algebra.RealRing          as RealRing
import qualified Algebra.Field              as Field
-- import qualified Algebra.Absolute               as Absolute
-- import qualified Algebra.Ring               as Ring
-- import qualified Algebra.Additive           as Additive

-- import Control.Monad.Fix (mfix, )
import Control.Monad (liftM3, )

import NumericPrelude.Numeric (zero, )
import NumericPrelude.Base
import Prelude ()



type T s u v sig q =
   Piecewise.Piece
      (DN.T u q) (DN.T v q)
      (DN.T v q -> SigG.LazySize -> q ->
       Proc.T s u q (SigA.T (Rate.Phantom s) (Amp.Flat q) (sig q)))

type Sequence s u v sig q =
   Piecewise.T
      (DN.T u q) (DN.T v q)
      (DN.T v q -> SigG.LazySize -> q ->
       Proc.T s u q (SigA.T (Rate.Phantom s) (Amp.Flat q) (sig q)))


{- |
Since this function looks for the maximum node value,
and since the signal parameter inference phase must be completed before signal processing,
infinite descriptions cannot be used here.
-}
{-# INLINE run #-}
run :: (Trans.C q, RealRing.C q, Dim.C u, Dim.C v, SigG.Write sig q) =>
   DN.T u q ->
   Sequence s u v sig q ->
   Proc.T s u q (SigA.T (Rate.Phantom s) (Amp.Dimensional v q) (sig q))
run lazySize cs =
   runVolume lazySize cs $
   maximum $
   map (\c -> max (DN.abs (Piecewise.pieceY0 c))
                  (DN.abs (Piecewise.pieceY1 c))) cs


{-# INLINE runVolume #-}
runVolume ::
   (Trans.C q, RealRing.C q, Dim.C u, Dim.C v, SigG.Write sig q) =>
   DN.T u q ->
   Sequence s u v sig q ->
   DN.T v q ->
   Proc.T s u q (SigA.T (Rate.Phantom s) (Amp.Dimensional v q) (sig q))
runVolume lazySize' cs amplitude =
   -- it would be nice if we could re-use Piece.run
   do ts0 <- mapM (toTimeScalar . Piecewise.pieceDur) cs
      lazySize <-
         Proc.intFromTime "Dimensional.Piece.runVolume" lazySize'
      fmap (SigA.fromBody amplitude . SigG.concat) $
         sequence $ zipWith
            (\(n,t) (Piecewise.PieceData c yi0 yi1 d) ->
                 fmap (SigG.take n . SigA.body) $
                 Piecewise.computePiece c yi0 yi1 d amplitude (SigG.LazySize lazySize) t)
            (Piecewise.splitDurations ts0)
            cs


{-# INLINE runState #-}
runState :: (Trans.C q, RealRing.C q, Dim.C u, Dim.C v) =>
   Sequence s u v Sig.T q ->
   Proc.T s u q (SigA.R s v q q)
runState = run zero


{-# INLINE runStateVolume #-}
runStateVolume ::
   (Trans.C q, RealRing.C q, Dim.C u, Dim.C v) =>
   Sequence s u v Sig.T q ->
   DN.T v q ->
   Proc.T s u q (SigA.R s v q q)
runStateVolume = runVolume zero


{-# INLINE toAmpScalar #-}
toAmpScalar ::
   (Field.C a, Dim.C u) =>
   DN.T u a -> DN.T u a -> a
toAmpScalar amp y =
   DN.divToScalar y amp

{-# INLINE make #-}
make :: (Field.C q, Dim.C u, Dim.C v, SigG.Write sig q) =>
   Piece.T sig q -> T s u v sig q
make piece =
   Piecewise.pieceFromFunction $ \ y0 y1 d amplitude lazySize t0 ->
      flip fmap (toTimeScalar d) (\d' ->
         SigA.flatFromBody $
         Piecewise.computePiece piece
            (toAmpScalar amplitude y0)
            (toAmpScalar amplitude y1)
            d' lazySize t0)

{-# INLINE step #-}
step :: (Field.C q, Dim.C u, Dim.C v, SigG.Write sig q) => T s u v sig q
step =
   make Piece.step

{-# INLINE linear #-}
linear :: (Field.C q, Dim.C u, Dim.C v, SigG.Write sig q) => T s u v sig q
linear =
   make Piece.linear

{-# INLINE exponential #-}
exponential :: (Trans.C q, Dim.C u, Dim.C v, SigG.Write sig q) =>
   DN.T v q -> T s u v sig q
exponential saturation =
   Piecewise.pieceFromFunction $ \ y0 y1 d amplitude lazySize t0 ->
      flip fmap (toTimeScalar d) (\d' ->
         SigA.flatFromBody $
         Piecewise.computePiece
            (Piece.exponential (toAmpScalar amplitude saturation))
            (toAmpScalar amplitude y0)
            (toAmpScalar amplitude y1)
            d' lazySize t0)

{-# INLINE cosine #-}
cosine :: (Trans.C q, Dim.C u, Dim.C v, SigG.Write sig q) => T s u v sig q
cosine =
   make Piece.cosine

{-# INLINE halfSine #-}
halfSine :: (Trans.C q, Dim.C u, Dim.C v, SigG.Write sig q) =>
   Piece.FlatPosition -> T s u v sig q
halfSine pos =
   make (Piece.halfSine pos)

{-# INLINE cubic #-}
cubic :: (Field.C q, Dim.C u, Dim.C v, SigG.Write sig q) =>
   DN.T (DimensionGradient u v) q ->
   DN.T (DimensionGradient u v) q ->
   T s u v sig q
cubic yd0 yd1 =
   Piecewise.pieceFromFunction $ \ y0 y1 d amplitude lazySize t0 ->
      liftM3 (\d' yd0' yd1' ->
         SigA.flatFromBody $
            Piecewise.computePiece
               (Piece.cubic yd0' yd1')
               (toAmpScalar amplitude y0)
               (toAmpScalar amplitude y1)
               d' lazySize t0)
         (toTimeScalar d)
         (toGradientScalar amplitude yd0)
         (toGradientScalar amplitude yd1)
