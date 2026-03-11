{- |
Copyright   :  (c) Henning Thielemann 2008-2010
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Causal.Displacement (
   mix, mixVolume,
   fanoutAndMixMulti, fanoutAndMixMultiVolume,
   raise, distort,
   ) where

import qualified Synthesizer.Dimensional.Map.Displacement as Disp

import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Sample as Sample

import qualified Synthesizer.Dimensional.Causal.Process as CausalD

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Field          as Field
import qualified Algebra.Absolute       as Absolute
-- import qualified Algebra.Ring           as Ring
-- import qualified Algebra.Additive       as Additive

-- import Algebra.Module ((*>))

import NumericPrelude.Base
-- import NumericPrelude.Numeric
import Prelude ()


type DNS v y yv = Sample.Dimensional v y yv


-- * Mixing

{-# INLINE mix #-}
mix :: (Absolute.C y, Field.C y, Module.C y yv, Dim.C v) =>
   Proc.T s u t (CausalD.T s (DNS v y yv, DNS v y yv) (DNS v y yv))
mix = Proc.pure $ Disp.mix

{-# INLINE mixVolume #-}
mixVolume ::
   (Field.C y, Module.C y yv, Dim.C v) =>
   DN.T v y ->
   Proc.T s u t (CausalD.T s (DNS v y yv, DNS v y yv) (DNS v y yv))
mixVolume = Proc.pure . Disp.mixVolume


{-# INLINE fanoutAndMixMulti #-}
fanoutAndMixMulti ::
   (RealField.C y, Module.C y yv, Dim.C v) =>
   [Proc.T s u t (CausalD.T s sample (DNS v y yv))] ->
   Proc.T s u t (CausalD.T s sample (DNS v y yv))
fanoutAndMixMulti =
   fmap Disp.fanoutAndMixMulti . sequence

{-# INLINE fanoutAndMixMultiVolume #-}
fanoutAndMixMultiVolume ::
   (Field.C y, Module.C y yv, Dim.C v) =>
   DN.T v y ->
   [Proc.T s u t (CausalD.T s sample (DNS v y yv))] ->
   Proc.T s u t (CausalD.T s sample (DNS v y yv))
fanoutAndMixMultiVolume amp =
   fmap (Disp.fanoutAndMixMultiVolume amp) . sequence


-- * Miscellaneous

{-# INLINE raise #-}
raise :: (Field.C y, Module.C y yv, Dim.C v) =>
   DN.T v y ->
   yv ->
   Proc.T s u t (CausalD.T s (DNS v y yv) (DNS v y yv))
raise y yv = Proc.pure (Disp.raise y yv)

{-# INLINE distort #-}
distort :: (Field.C y, Module.C y yv, Dim.C v) =>
   (yv -> yv) ->
   Proc.T s u t (CausalD.T s (DNS v y y, DNS v y yv) (DNS v y yv))
distort =
   Proc.pure . Disp.distort
