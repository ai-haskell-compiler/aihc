{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes


Control curves which can be used
as envelopes, for controlling filter parameters and so on.
-}
module Synthesizer.Dimensional.Rate.Control
   ({- * Primitives -}
    constant, linear, exponential, exponential2, )
   where

import qualified Synthesizer.Dimensional.Signal.Private as SigA

import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Synthesizer.State.Control as Ctrl
import qualified Synthesizer.State.Signal  as Sig

import qualified Synthesizer.Dimensional.Process as Proc

-- import Synthesizer.Dimensional.Process (($#), )

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

-- import Number.DimensionTerm ((&*&))

import qualified Algebra.Transcendental     as Trans
import qualified Algebra.Field              as Field
-- import qualified Algebra.Absolute               as Absolute
import qualified Algebra.Ring               as Ring
-- import qualified Algebra.Additive           as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


type Signal s y = SigA.T (Rate.Phantom s) (Amp.Flat y) (Sig.T y)


{-# INLINE constant #-}
constant :: (Ring.C y, Dim.C u) =>
   Proc.T s u t (Signal s y)
constant = Proc.pure $ SigA.flatFromBody $ Ctrl.constant one

{- |
Caution: This control curve can contain samples
with an absolute value greater than 1.
The linear curve starts with zero.
-}
{-# INLINE linear #-}
linear ::
   (Field.C q, Dim.C u) =>
      DN.T u q {-^ distance until curve reaches one -}
   -> Proc.T s u q (Signal s q)
linear dist =
   fmap
      (SigA.flatFromBody . Ctrl.linearMultiscaleNeutral . recip)
      (Proc.toTimeScalar dist)

{-# INLINE exponential #-}
exponential :: (Trans.C q, Dim.C u) =>
      DN.T u q {-^ time where the function reaches 1\/e of the initial value -}
   -> Proc.T s u q (Signal s q)
exponential time =
   fmap
      (SigA.flatFromBody . Ctrl.exponentialMultiscaleNeutral)
      (Proc.toTimeScalar time)

{-
  take 1000 $ show (run (fixSampleRate 100 (exponential 0.1 1)) :: SigDouble)
-}

{-# INLINE exponential2 #-}
exponential2 :: (Trans.C q, Dim.C u) =>
      DN.T u q {-^ half life, time where the function reaches 1\/2 of the initial value -}
   -> Proc.T s u q (Signal s q)
exponential2 time =
   fmap
      (SigA.flatFromBody . Ctrl.exponential2MultiscaleNeutral)
      (Proc.toTimeScalar time)
