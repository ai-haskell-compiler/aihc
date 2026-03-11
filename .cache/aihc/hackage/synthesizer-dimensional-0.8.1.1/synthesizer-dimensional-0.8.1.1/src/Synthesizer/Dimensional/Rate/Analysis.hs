{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Rate.Analysis (
    centroid,
    length,
  ) where

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Synthesizer.State.Analysis as Ana
import qualified Synthesizer.State.Signal   as Sig

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import Number.DimensionTerm ((*&))

import qualified Algebra.Field               as Field
-- import qualified Algebra.Absolute                as Absolute
-- import qualified Algebra.Ring                as Ring


import NumericPrelude.Base ((.), )
import NumericPrelude.Numeric
import Prelude ()


type SignalRate u t amp yv =
   SigA.T (Rate.Actual (DN.T (Dim.Recip u) t)) amp (Sig.T yv)


{-# INLINE centroid #-}
centroid :: (Field.C q, Dim.C u) =>
   SignalRate u q amp q -> DN.T u q
centroid = makePhysicalLength Ana.centroid

{-# INLINE length #-}
length :: (Field.C t, Dim.C u) =>
   SignalRate u t amp yv -> DN.T u t
length = makePhysicalLength (fromIntegral . Sig.length)

{-# INLINE makePhysicalLength #-}
makePhysicalLength :: (Field.C t, Dim.C u) =>
   (Sig.T y -> t) ->
   SignalRate u t amp y -> DN.T u t
makePhysicalLength f x =
   f (SigA.body x)  *&  DN.unrecip (SigA.actualSampleRate x)
