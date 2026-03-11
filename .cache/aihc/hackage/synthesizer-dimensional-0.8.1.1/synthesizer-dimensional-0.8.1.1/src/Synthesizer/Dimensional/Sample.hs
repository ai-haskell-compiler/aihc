{-# LANGUAGE TypeFamilies #-}
module Synthesizer.Dimensional.Sample where

import qualified Synthesizer.Dimensional.Amplitude as Amp

{- |
The constructor is only needed for 'arr',
which is a kind of a hack.
-}
data T amp yv = Cons amp yv

cons :: Amp.C amp => amp -> yv -> T amp yv
cons = Cons

type Dimensional v y yv = T (Amp.Dimensional v y) yv
type Numeric     amp yv = T (Amp.Numeric amp) yv
type Flat     y = T (Amp.Flat y) y
type Abstract y = T Amp.Abstract y


{- |
When you define additional instances,
take care that displacements and amplitudes cannot be brought out of order!
-}
type family Amplitude sample
type instance Amplitude (T amp yv) = amp
type instance Amplitude (sample0, sample1) =
   (Amplitude sample0, Amplitude sample1)
type instance Amplitude (sample0, sample1, sample2) =
   (Amplitude sample0, Amplitude sample1, Amplitude sample2)

type family Displacement sample
type instance Displacement (T amp yv) = yv
type instance Displacement (sample0, sample1) =
   (Displacement sample0, Displacement sample1)
type instance Displacement (sample0, sample1, sample2) =
   (Displacement sample0, Displacement sample1, Displacement sample2)


class Build sample where
   build :: Amplitude sample -> Displacement sample -> sample

instance Build (T amp yv) where
   {-# INLINE build #-}
   build = Cons

instance
   (Build sample0, Build sample1) =>
      Build (sample0, sample1) where
   {-# INLINE build #-}
   build (amp0,amp1) (yv0,yv1) =
      (build amp0 yv0, build amp1 yv1)

instance
   (Build sample0, Build sample1, Build sample2) =>
      Build (sample0, sample1, sample2) where
   {-# INLINE build #-}
   build (amp0,amp1,amp2) (yv0,yv1,yv2) =
      (build amp0 yv0, build amp1 yv1, build amp2 yv2)


class Inspect sample where
   {- method names are chosen analogously to the type functions -}
   amplitude :: sample -> Amplitude sample
   displacement :: sample -> Displacement sample

instance Inspect (T amp yv) where
   {-# INLINE amplitude #-}
   {-# INLINE displacement #-}
   amplitude (Cons amp _) = amp
   displacement (Cons _ yv) = yv

instance
   (Inspect sample0, Inspect sample1) =>
      Inspect (sample0, sample1) where
   {-# INLINE amplitude #-}
   {-# INLINE displacement #-}
   amplitude (sample0, sample1) =
      (amplitude sample0, amplitude sample1)
   displacement (sample0, sample1) =
      (displacement sample0, displacement sample1)

instance
   (Inspect sample0, Inspect sample1, Inspect sample2) =>
      Inspect (sample0, sample1, sample2) where
   {-# INLINE amplitude #-}
   {-# INLINE displacement #-}
   amplitude (sample0, sample1, sample2) =
      (amplitude sample0, amplitude sample1, amplitude sample2)
   displacement (sample0, sample1, sample2) =
      (displacement sample0, displacement sample1, displacement sample2)
