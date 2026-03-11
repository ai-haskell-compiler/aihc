module Synthesizer.Dimensional.Map.Displacement (
   mix, mixVolume,
   fanoutAndMixMulti, fanoutAndMixMultiVolume,
   raise, distort,
   mapLinear, mapExponential, mapLinearDimension,
   ) where

import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Sample as Sample

import qualified Synthesizer.Dimensional.Arrow as ArrowD

import Control.Arrow (Arrow, arr, (<<<), (^<<), (&&&), )

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim
import Number.DimensionTerm ((&*&))

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.RealField      as RealField
import qualified Algebra.Field          as Field
import qualified Algebra.Absolute       as Absolute
import qualified Algebra.Ring           as Ring
-- import qualified Algebra.Additive       as Additive

-- import Algebra.Module ((*>))

import Control.Monad.Trans.Reader (Reader, runReader, asks, )
import Control.Applicative (liftA2, )

import NumericPrelude.Base
import NumericPrelude.Numeric
import Prelude ()


type DNS v y yv = Sample.Dimensional v y yv
type Context v y = Reader (DN.T v y)


-- * Mixing

{- |
Mix two signals.
In contrast to 'zipWith' the result has the length of the longer signal.
-}
{-# INLINE mix #-}
mix ::
   (Absolute.C y, Field.C y, Module.C y yv, Dim.C v, Arrow arrow) =>
   ArrowD.T arrow (DNS v y yv, DNS v y yv) (DNS v y yv)
mix =
   fromAmplitudeReader $ \(Amp.Numeric amp0, Amp.Numeric amp1) ->
      (DN.abs amp0 + DN.abs amp1, mixCore amp0 amp1)

{-# INLINE mixVolume #-}
mixVolume ::
   (Field.C y, Module.C y yv, Dim.C v, Arrow arrow) =>
   DN.T v y ->
   ArrowD.T arrow (DNS v y yv, DNS v y yv) (DNS v y yv)
mixVolume amp =
   fromAmplitudeReader $ \(Amp.Numeric amp0, Amp.Numeric amp1) ->
      (amp, mixCore amp0 amp1)

{-# INLINE mixCore #-}
mixCore ::
   (Field.C y, Module.C y yv, Dim.C v, Arrow arrow) =>
   DN.T v y -> DN.T v y ->
   Context v y (arrow (yv,yv) yv)
mixCore amp0 amp1 =
   liftA2
      (\toSamp0 toSamp1 ->
         arr (\(y0,y1) -> toSamp0 y0 + toSamp1 y1))
      (toAmplitudeVector amp0)
      (toAmplitudeVector amp1)


{- |
Mix one or more signals.
-}
{-# INLINE fanoutAndMixMulti #-}
fanoutAndMixMulti ::
   (RealField.C y, Module.C y yv, Dim.C v, Arrow arrow) =>
   [ArrowD.T arrow sample (DNS v y yv)] ->
   ArrowD.T arrow sample (DNS v y yv)
fanoutAndMixMulti cs =
   fromAmplitudeReader $ \ampIn ->
      let ampCs = map (\(ArrowD.Cons f) -> f ampIn) cs
      in  (maximum (map (\(_, Amp.Numeric amp) -> amp) ampCs),
           fanoutAndMixMultiCore ampCs)

{- |
Mix zero or more signals.
-}
{-# INLINE fanoutAndMixMultiVolume #-}
fanoutAndMixMultiVolume ::
   (Field.C y, Module.C y yv, Dim.C v, Arrow arrow) =>
   DN.T v y ->
   [ArrowD.T arrow sample (DNS v y yv)] ->
   ArrowD.T arrow sample (DNS v y yv)
fanoutAndMixMultiVolume amp cs =
   fromAmplitudeReader $ \ampIn ->
      (amp, fanoutAndMixMultiCore $
               map (\(ArrowD.Cons f) -> f ampIn) cs)

{-# INLINE fanoutAndMixMultiCore #-}
fanoutAndMixMultiCore ::
   (Field.C y, Module.C y yv, Dim.C v, Arrow arrow) =>
   [(arrow yvIn yv, Amp.Dimensional v y)] ->
   Context v y (arrow yvIn yv)
fanoutAndMixMultiCore cs =
   foldr
      (\(c, Amp.Numeric ampX) ->
         liftA2
            (\toSamp rest ->
               uncurry (+) ^<< (toSamp ^<< c) &&& rest)
            (toAmplitudeVector ampX))
      (return $ arr (const zero)) cs


-- * Miscellaneous

{- |
Add a number to all of the signal values.
This is useful for adjusting the center of a modulation.
-}
{-# INLINE raise #-}
raise ::
   (Field.C y, Module.C y yv, Dim.C v, Arrow arrow) =>
   DN.T v y ->
   yv ->
   ArrowD.T arrow (DNS v y yv) (DNS v y yv)
raise y' yv =
   fromAmplitudeReader $ \(Amp.Numeric amp) ->
      (amp, fmap (\toSamp -> arr (toSamp yv +)) (toAmplitudeVector y'))

{- |
Distort the signal using a flat function.
The first signal gives the scaling of the function.
If the scaling is @c@ and the input sample is @y@,
then @c * f(y/c)@ is emitted.
This way we can use an (efficient) flat function
and have a simple, yet dimension conform, way of controlling the distortion.
E.g. if the distortion function is @tanh@
then the value @c@ controls the saturation level.
-}
{-# INLINE distort #-}
distort ::
   (Field.C y, Module.C y yv, Dim.C v, Arrow arrow) =>
   (yv -> yv) ->
   ArrowD.T arrow (DNS v y y, DNS v y yv) (DNS v y yv)
distort f =
   fromAmplitudeReader $ \(Amp.Numeric ampCtrl, Amp.Numeric ampIn) ->
      (ampIn,
       fmap (\toSamp ->
          arr (\(c,y) ->
             let c' = toSamp c
             in  c' *> f (recip c' *> y)))
          (toAmplitudeScalar ampCtrl))



{- |
Map a control curve without amplitude unit
by a linear (affine) function with a unit.
This is a combination of 'raise' and 'amplify'.

It is not quite correct in the sense,
that it does not produce low-level sample values in the range (-1,1).
Instead it generates values around 1.
-}
{-# INLINE mapLinear #-}
mapLinear ::
   (Flat.C y flat, Ring.C y, Dim.C u, Arrow arrow) =>
   y ->
   DN.T u y ->
   ArrowD.T arrow (Sample.T flat y) (DNS u y y)
mapLinear depth center =
   ArrowD.Cons (\Amp.Flat ->
      (arr (\x -> one+x*depth), Amp.Numeric center))
   <<<
   ArrowD.canonicalizeFlat

{-# INLINE mapExponential #-}
mapExponential ::
   (Flat.C y flat, Trans.C y, Dim.C u, Arrow arrow) =>
   y ->
   DN.T u q ->
   ArrowD.T arrow (Sample.T flat y) (DNS u q y)
mapExponential depth center =
   {-
   X86 processors only have (logBase 2) and (2**).
   Thus on those machines computing with respect to base 2
   can be more efficient and more precise.
   -}
   let logDepth = log depth
   in  ArrowD.Cons (\Amp.Flat ->
          (arr (exp . (logDepth*)), Amp.Numeric center))
   <<<
   ArrowD.canonicalizeFlat

{-# INLINE mapLinearDimension #-}
mapLinearDimension ::
   (Field.C y, Absolute.C y, Dim.C u, Dim.C v, Arrow arrow) =>
      DN.T v y              {- ^ range: one is mapped to @center + range * ampX@ -}
   -> DN.T (Dim.Mul v u) y  {- ^ center: zero is mapped to @center@ -}
   -> ArrowD.T arrow (DNS u y y) (DNS (Dim.Mul v u) y y)
mapLinearDimension range center =
   ArrowD.Cons $ \(Amp.Numeric ampIn) ->
      let absRange  = DN.abs range &*& ampIn
          absCenter = DN.abs center
          ampOut = absRange + absCenter
          rng = DN.divToScalar absRange  ampOut
          cnt = DN.divToScalar absCenter ampOut
      in  (arr (\y -> cnt + rng*y), Amp.Numeric ampOut)


-- auxiliary functions

{-# INLINE toAmplitudeScalar #-}
toAmplitudeScalar ::
   (Field.C y, Dim.C u) =>
   DN.T u y -> Context u y (y -> y)
toAmplitudeScalar ampIn =
   asks (\ampOut -> (DN.divToScalar ampIn ampOut *))

{-# INLINE toAmplitudeVector #-}
toAmplitudeVector ::
   (Module.C y yv, Field.C y, Dim.C u) =>
   DN.T u y -> Context u y (yv -> yv)
toAmplitudeVector ampIn =
   asks (\ampOut -> (DN.divToScalar ampIn ampOut *> ))

{-# INLINE fromAmplitudeReader #-}
fromAmplitudeReader ::
   (Sample.Amplitude sampleIn ->
     (ampOut,
      Reader ampOut (arrow (Sample.Displacement sampleIn) yvOut))) ->
   ArrowD.T arrow sampleIn (Sample.Numeric ampOut yvOut)
fromAmplitudeReader f =
   ArrowD.Cons $ \ampIn ->
      let (ampOut, rd) = f ampIn
      in  (runReader rd ampOut, Amp.Numeric ampOut)
