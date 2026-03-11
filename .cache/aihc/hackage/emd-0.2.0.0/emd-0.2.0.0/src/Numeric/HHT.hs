{-# LANGUAGE BangPatterns                             #-}
{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE MultiWayIf                               #-}
{-# LANGUAGE RecordWildCards                          #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeFamilies                             #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

-- |
-- Module      : Numeric.HHT
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Hilbert-Huang transform in pure Haskell.
--
-- The main data type is 'HHT', which can be generated using 'hht' or
-- 'hhtEmd'.  See "Numeric.EMD" for information on why this module uses
-- "sized vectors", and how to convert unsized vectors to sized vectors.
--
-- @since 0.1.2.0

module Numeric.HHT (
  -- * Hilbert-Huang Transform
    HHT(..), HHTLine(..)
  , hhtEmd
  , hht
  , ihhtEmd
  , ihht
  -- ** Hilbert-Huang Spectrum
  , hhtSpectrum, hhtSparseSpectrum, hhtDenseSpectrum
  -- ** Properties of spectrum
  , meanMarginal, marginal, instantaneousEnergy, degreeOfStationarity
  , expectedFreq, dominantFreq
  , foldFreq
  -- ** Options
  , EMDOpts(..), defaultEO, BoundaryHandler(..), defaultSifter, SplineEnd(..)
  -- * Hilbert transforms (internal usage)
  , hilbert
  , hilbertIm
  , hilbertPolar
  , hilbertMagFreq
  ) where

import           Control.DeepSeq
import           Data.Complex
import           Data.Finite
import           Data.Fixed
import           Data.Foldable
import           Data.Proxy
import           Data.Semigroup
import           GHC.Generics              (Generic)
import           GHC.TypeNats
import           Numeric.EMD
import           Numeric.HHT.Internal.FFT
import qualified Data.Binary               as Bi
import qualified Data.Map                  as M
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Generic.Sized as SVG
import qualified Data.Vector.Sized         as SV
import qualified Math.FFT.Base             as FFT

-- | A Hilbert Trasnform of a given IMF, given as a "skeleton line".
data HHTLine v n a = HHTLine
    { -- | IMF HHT Magnitude as a time series.
      --
      -- It may be useful to "zip" this vector with 'hlFreqs'.  To do this,
      -- use a function like 'SVG.init' or 'SVG.tail' to make these two
      -- vectors contain the same length, or 'weaken'/'shift' to make
      -- indices in 'hlFreqs' usable as indices in 'hlMags'.
      --
      -- Prior to v0.1.9.0, this was a length-n vector, just like
      -- 'hlFreqs'.  To get the same behavior, use 'SVG.init' on this new
      -- field's value.
      hlMags      :: !(SVG.Vector v (n + 1) a)
      -- | IMF HHT instantaneous frequency as a time series (between 0 and 1).
      --
      -- In reality, these frequencies are the frequencies "in between"
      -- each step in 'hlMags'.
    , hlFreqs     :: !(SVG.Vector v n a)
      -- | Initial phase of skeleton line (between -pi and pi)
      --
      -- @since 0.1.9.0
    , hlInitPhase :: !a
    }
  deriving (Show, Eq, Ord, Generic)

-- | @since 0.1.3.0
instance (VG.Vector v a, KnownNat n, Bi.Binary (v a), Bi.Binary a) => Bi.Binary (HHTLine v n a)

-- | @since 0.1.5.0
instance (NFData (v a), NFData a) => NFData (HHTLine v n a)

-- | A Hilbert-Huang Transform.  An @'HHT' v n a@ is a Hilbert-Huang
-- transform of an @n@-item time series of items of type @a@ represented
-- using vector @v@.
--
-- Create using 'hht' or 'hhtEmd'.
data HHT v n a = HHT
    { -- | Skeleton lines corresponding to each IMF
      hhtLines    :: [HHTLine v n a]
      -- | Residual from EMD
      --
      -- @since 0.1.9.0
    , hhtResidual :: SVG.Vector v (n + 1) a
    }
  deriving (Show, Eq, Ord, Generic)

-- | @since 0.1.3.0
instance (VG.Vector v a, KnownNat n, Bi.Binary (v a), Bi.Binary a) => Bi.Binary (HHT v n a)

-- | @since 0.1.5.0
instance (NFData (v a), NFData a) => NFData (HHT v n a)

-- | Directly compute the Hilbert-Huang transform of a given time series.
-- Essentially is a composition of 'hhtEmd' and 'emd'.  See 'hhtEmd' for
-- a more flexible version.
hht :: forall v n a. (VG.Vector v a, VG.Vector v (Complex a), KnownNat n, FFT.FFTWReal a)
    => EMDOpts v (n + 1) a
    -> SVG.Vector v (n + 1) a
    -> HHT v n a
hht eo = hhtEmd . emd eo

-- | Compute the Hilbert-Huang transform from a given Empirical Mode
-- Decomposition.
hhtEmd
    :: forall v n a. (VG.Vector v a, VG.Vector v (Complex a), KnownNat n, FFT.FFTWReal a)
    => EMD v (n + 1) a
    -> HHT v n a
hhtEmd EMD{..} = HHT (map go emdIMFs) emdResidual
  where
    go i = HHTLine m f φ0
      where
        (m, (f, φ0)) = hilbertMagFreq i

-- | Invert a Hilbert-Huang transform back to an Empirical Mode
-- Decomposition
--
-- @since 0.1.9.0
ihhtEmd
    :: (VG.Vector v a, Floating a)
    => HHT v n a
    -> EMD v (n + 1) a
ihhtEmd HHT{..} = EMD (map go hhtLines) hhtResidual
  where
    go HHTLine{..} = SVG.zipWith (\m θ -> m * cos θ) hlMags θs
      where
        θs = SVG.scanl' (+) hlInitPhase ((* (2 * pi)) `SVG.map` hlFreqs)

-- | Construct a time series correpsonding to its hilbert-huang transform.
--
-- @since 0.1.9.0
ihht
    :: (VG.Vector v a, Floating a)
    => HHT v n a
    -> SVG.Vector v (n + 1) a
ihht = iemd . ihhtEmd

-- | Fold and collapse a Hilbert-Huang transform along the frequency axis
-- at each step in time along some monoid.
--
-- @since 0.1.8.0
foldFreq
    :: forall v u n a b c. (VG.Vector v a, VG.Vector u c, KnownNat n, Monoid b)
    => (a -> a -> b)  -- ^ Combining function, taking frequency, then magnitude
    -> (b -> c)       -- ^ Projecting function
    -> HHT v n a
    -> SVG.Vector u n c
foldFreq f g = pullBack
             . foldl' (SV.zipWith (<>)) (SV.replicate mempty)
             . map split
             . hhtLines
  where
    split :: HHTLine v n a -> SV.Vector n b
    split HHTLine{..} = SVG.generate $ \i ->
      f (hlFreqs `SVG.index` i) (hlMags `SVG.index` weaken i)
    {-# INLINE split #-}
    pullBack :: SV.Vector n b -> SVG.Vector u n c
    pullBack v = SVG.generate $ \i -> g (v `SV.index` i)
    {-# INLINE pullBack #-}
{-# INLINE foldFreq #-}

newtype SumMap k a = SumMap { getSumMap :: M.Map k a }

instance (Ord k, Num a) => Semigroup (SumMap k a) where
    SumMap x <> SumMap y = SumMap $ M.unionWith (+) x y

instance (Ord k, Num a) => Monoid (SumMap k a) where
    mempty = SumMap M.empty

-- | Compute the full Hilbert-Huang Transform spectrum.  At each timestep
-- is a sparse map of frequency components and their respective magnitudes.
-- Frequencies not in the map are considered to be zero.
--
-- Takes a "binning" function to allow you to specify how specific you want
-- your frequencies to be.
--
-- See 'hhtSparseSpectrum' for a sparser version, and 'hhtDenseSpectrum'
-- for a denser version.
hhtSpectrum
    :: forall v n a k. (VG.Vector v a, KnownNat n, Ord k, Num a)
    => (a -> k)     -- ^ binning function.  takes rev/tick freq between 0 and 1.
    -> HHT v n a
    -> SV.Vector n (M.Map k a)
hhtSpectrum f = foldFreq (\k -> SumMap . M.singleton (f k)) getSumMap

-- | A sparser vesion of 'hhtSpectrum'.  Compute the full Hilbert-Huang
-- Transform spectrum.  Returns a /sparse/ matrix representing the power at
-- each time step (the @'Finite' n@) and frequency (the @k@).
--
-- Takes a "binning" function to allow you to specify how specific you want
-- your frequencies to be.
--
-- @since 0.1.4.0
hhtSparseSpectrum
    :: forall v n a k. (VG.Vector v a, KnownNat n, Ord k, Num a)
    => (a -> k)     -- ^ binning function.  takes rev/tick freq between 0 and 1.
    -> HHT v n a
    -> M.Map (Finite n, k) a
hhtSparseSpectrum f = M.unionsWith (+) . concatMap go . hhtLines
  where
    go :: HHTLine v n a -> [M.Map (Finite n, k) a]
    go HHTLine{..} = flip fmap (finites @n) $ \i ->
      M.singleton (i, f $ hlFreqs `SVG.index` i) $
        hlMags `SVG.index` weaken i

-- | A denser version of 'hhtSpectrum'.  Compute the full  Hilbert-Huang
-- Transform spectrum, returning a dense matrix (as a vector of vectors)
-- representing the power at each time step and each frequency.
--
-- Takes a "binning" function that maps a frequency to one of @m@ discrete
-- slots, for accumulation in the dense matrix.
--
-- @since 0.1.4.0
hhtDenseSpectrum
    :: forall v n m a. (VG.Vector v a, KnownNat n, KnownNat m, Num a)
    => (a -> Finite m)     -- ^ binning function.  takes rev/tick freq between 0 and 1.
    -> HHT v n a
    -> SV.Vector n (SV.Vector m a)
hhtDenseSpectrum f h = SV.generate $ \i -> SV.generate $ \j ->
    M.findWithDefault 0 (i, j) ss
  where
    ss = hhtSparseSpectrum f h

-- | Compute the marginal spectrum given a Hilbert-Huang Transform. It
-- provides the "total power" over the entire time series for each
-- frequency component.  See 'meanMarginal' for a version that averages
-- over the length of the time series, making it more close in nature to
-- the purpose of a Fourier Transform.
--
-- A binning function is accepted to allow you to specify how specific you
-- want your frequencies to be.
marginal
    :: forall v n a k. (VG.Vector v a, KnownNat n, Ord k, Num a)
    => (a -> k)     -- ^ binning function.  takes rev/tick freq between 0 and 1.
    -> HHT v n a
    -> M.Map k a
marginal f = M.unionsWith (+) . concatMap go . hhtLines
  where
    go :: HHTLine v n a -> [M.Map k a]
    go HHTLine{..} = flip fmap (finites @n) $ \i ->
      M.singleton (f $ hlFreqs `SVG.index` i) (hlMags `SVG.index` weaken i)

-- | Compute the mean marginal spectrum given a Hilbert-Huang Transform. It
-- is similar to a Fourier Transform; it provides the "total power" over
-- the entire time series for each frequency component, averaged over the
-- length of the time series.
--
-- A binning function is accepted to allow you to specify how specific you
-- want your frequencies to be.
--
-- @since 0.1.8.0
meanMarginal
    :: forall v n a k. (VG.Vector v a, KnownNat n, Ord k, Fractional a)
    => (a -> k)     -- ^ binning function.  takes rev/tick freq between 0 and 1.
    -> HHT v n a
    -> M.Map k a
meanMarginal f = fmap (/ n) . marginal f
  where
    n = fromIntegral $ natVal (Proxy @n)

-- | Returns the "expected value" of frequency at each time step,
-- calculated as a weighted average of all contributions at every frequency
-- at that time step.
--
-- @since 0.1.4.0
expectedFreq
    :: forall v n a. (VG.Vector v a, KnownNat n, Fractional a)
    => HHT v n a
    -> SVG.Vector v n a
expectedFreq = foldFreq (\x y -> (Sum (x * y), Sum y)) (\(Sum x, Sum y) -> x / y)

-- | Returns the dominant frequency (frequency with largest magnitude
-- contribution) at each time step.
--
-- @since 0.1.4.0
dominantFreq
    :: forall v n a. (VG.Vector v a, KnownNat n, Ord a)
    => HHT v n a
    -> SVG.Vector v n a
dominantFreq = foldFreq comb proj
  where
    comb :: a -> a -> Maybe (Max (Arg a a))
    comb x y = Just $ Max $ Arg y x
    proj :: Maybe (Max (Arg a a)) -> a
    proj Nothing = errorWithoutStackTrace
      "Numeric.HHT.dominantFreq: HHT was formed with no Intrinsic Mode Functions"
    proj (Just (Max (Arg _ x))) = x

-- | Compute the instantaneous energy of the time series at every step via
-- the Hilbert-Huang Transform.
instantaneousEnergy
    :: forall v n a. (VG.Vector v a, KnownNat n, Num a)
    => HHT v n a
    -> SVG.Vector v n a
instantaneousEnergy = foldFreq (\_ x -> Sum (x * x)) getSum

-- | Degree of stationarity, as a function of frequency.
degreeOfStationarity
    :: forall v n a k. (VG.Vector v a, KnownNat n, Ord k, Fractional a, Eq a)
    => (a -> k)     -- ^ binning function.  takes rev/tick freq between 0 and 1.
    -> HHT v n a
    -> M.Map k a
degreeOfStationarity f h = fmap (/ n)
                         . M.unionsWith (+)
                         . concatMap go
                         . hhtLines
                         $ h
  where
    n = fromIntegral $ natVal (Proxy @n)
    meanMarg = meanMarginal f h
    go :: HHTLine v n a -> [M.Map k a]
    go HHTLine{..} = flip fmap (finites @n) $ \i ->
        let fr = f $ hlFreqs `SVG.index` i
            mm = meanMarg M.! fr
        in  M.singleton fr $
              if mm == 0
                then 0
                else (1 - (hlMags `SVG.index` weaken i / mm)) ^ (2 :: Int)

-- | Given a time series, return a time series of the /magnitude/ of the
-- hilbert transform and the /frequency/ of the hilbert transform, in units
-- of revolutions per tick.  Is only expected to taken in proper/legal
-- IMFs.
--
-- The frequency will always be between 0 and 1, since we can't determine
-- anything faster given the discretization, and we exclude negative values
-- as physically unmeaningful for an IMF.
hilbertMagFreq
    :: forall v n a. (VG.Vector v a, VG.Vector v (Complex a), KnownNat n, FFT.FFTWReal a)
    => SVG.Vector v (n + 1) a
    -> (SVG.Vector v (n + 1) a, (SVG.Vector v n a, a))
hilbertMagFreq v = (hilbertMag, (hilbertFreq, φ0))
  where
    v'           = hilbert v
    hilbertMag   = SVG.map magnitude v'
    hilbertPhase = SVG.map phase v'
    φ0           = SVG.head hilbertPhase
    hilbertFreq  = SVG.map ((`mod'` 1) . (/ (2 * pi))) $ SVG.tail hilbertPhase - SVG.init hilbertPhase

-- | The polar form of 'hilbert': returns the magnitude and phase of the
-- discrete hilbert transform of a series.
--
-- The computation of magnitude is unique, but computing phase gives us
-- some ambiguity.  The interpretation of the hilbert transform for
-- instantaneous frequency is that the original series "spirals" around the
-- complex plane as time progresses, like a helix.  So, we impose
-- a constraint on the phase to uniquely determine it: \(\phi_{t+1}\) is
-- the /minimal valid phase/ such that \(\phi_{t+1} \geq \phi_{t}\).  This
-- enforces the phase to be monotonically increasing at the slowest
-- possible detectable rate.
--
-- @since 0.1.6.0
hilbertPolar
    :: forall v n a. (VG.Vector v a, VG.Vector v (Complex a), KnownNat n, FFT.FFTWReal a)
    => SVG.Vector v (n + 1) a
    -> (SVG.Vector v (n + 1) a, SVG.Vector v (n + 1) a)
hilbertPolar v = (hilbertMag, hilbertPhase)
  where
    hilbertMag :: SVG.Vector v (n + 1) a
    hilbertFreq :: SVG.Vector v n a
    (hilbertMag, (hilbertFreq, φ0)) = hilbertMagFreq v
    hilbertPhase :: SVG.Vector v (n + 1) a
    hilbertPhase = SVG.scanl' (+) φ0 ((* (2 * pi)) `SVG.map` hilbertFreq)

-- | Real part is original series and imaginary part is hilbert transformed
-- series.  Creates a "helical" form of the original series that rotates
-- along the complex plane.
--
-- Note that since /0.1.7.0/, this uses the same algorithm as the matlab
-- implementation <https://www.mathworks.com/help/signal/ref/hilbert.html>
hilbert
    :: forall v n a.
      ( VG.Vector v a
      , VG.Vector v (Complex a)
      , KnownNat n
      , FFT.FFTWReal a
      )
    => SVG.Vector v n a
    -> SVG.Vector v n (Complex a)
hilbert v = ifft u'
  where
    v' = SVG.map (:+ 0) v
    u  = fft v'
    u' = flip SVG.imap u $ \(fromIntegral->i) x ->
      if | i == 0 || i == (n `div` 2) -> x
         | i < (n `div` 2)            -> 2 * x
         | otherwise                  -> 0
    n  = natVal (Proxy @n)

-- | Hilbert transformed series.  Essentially the same series, but
-- phase-shifted 90 degrees.  Is so-named because it is the "imaginary
-- part" of the proper hilbert transform, 'hilbert'.
--
-- Note that since /0.1.7.0/, this uses the same algorithm as the matlab
-- implementation <https://www.mathworks.com/help/signal/ref/hilbert.html>
hilbertIm
    :: forall v n a.
      ( VG.Vector v a
      , VG.Vector v (Complex a)
      , KnownNat n
      , FFT.FFTWReal a
      )
    => SVG.Vector v n a
    -> SVG.Vector v n a
hilbertIm = SVG.map imagPart . hilbert

