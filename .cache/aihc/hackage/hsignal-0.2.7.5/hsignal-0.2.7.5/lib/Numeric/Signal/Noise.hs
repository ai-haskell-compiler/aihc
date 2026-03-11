--{-# LANGUAGE UndecidableInstances,
--             FlexibleInstances,
--             FlexibleContexts,
--             TypeFamilies,
--             ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Signal.Noise
-- Copyright   :  (c) Alexander Vivian Hugh McPhail 2010, 2014, 2015
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses Concurrency
--
-- Noise generation functions
--
-----------------------------------------------------------------------------

--             IncoherentInstances,


module Numeric.Signal.Noise (
                             pinkNoise
                            , spatialNoise
                            , powerNoise
                            ) where

-----------------------------------------------------------------------------

--import qualified Numeric.Signal as S

--import Complex

--import qualified Data.Array.IArray as I
--import Data.Ix

--import Data.Word

--import System.IO.Unsafe(unsafePerformIO)

--import qualified Data.List as L

--import Data.Binary

--import Foreign.Storable

import Numeric.LinearAlgebra

import qualified Data.Vector.Generic as GV

import qualified Numeric.GSL.Fourier as F

--import qualified Numeric.GSL.Histogram as H
--import qualified Numeric.GSL.Histogram2D as H2

--import qualified Numeric.Statistics.Information as SI

import Prelude hiding(filter)

--import Control.Monad(replicateM)

-------------------------------------------------------------------

-- | The method is briefly descirbed in Lennon, J.L. "Red-shifts and red
-- herrings in geographical ecology", Ecography, Vol. 23, p101-113 (2000)
--
-- Matlab version Written by Jon Yearsley  1 May 2004
-- j.yearsley@macaulay.ac.uk
--
-- Creates 1/f scale invariant spatial noise
spatialNoise :: Double    -- ^  β: spectral distribution
                         --    0: White noise
                         --   -1: Pink noise
                         --   -2: Brownian noise
             -> Int -> Int -- ^ matrix dimensions
             -> Int       -- ^ random seed
             -> Matrix Double
spatialNoise b r' c' s = let c = fromIntegral c'
                             r = fromIntegral r'
                             pre_x = linspace c' (0::Double,c-1)
                             post_x = linspace c' (c,1)
                             freq_x = GV.map (/c) $ vjoin [pre_x,post_x]
                             u = fromRows (replicate (2*r') freq_x)
                             pre_y = linspace r' (0::Double,r-1)
                             post_y = linspace r' (r,1)
                             freq_y = GV.map (/c) $ vjoin [pre_y,post_y]
                             v = fromColumns (replicate (2*c') freq_y)
                             s_f = cmap (**(b/2)) ((u**2) + (v**2))
                             s_f' = cmap (\x -> if isInfinite x then 0 else x) s_f
                             phi = reshape (2*c') (randomVector s Uniform (4*r'*c'))
                         in subMatrix (1,1) (r',c') $ fst $ fromComplex $ fromRows $ map F.ifft $ toRows $ ((complex $ s_f'**0.5) * (toComplex (cos(2*pi*phi),sin(2*pi*phi))))

-- | 1/f scale invariant noise
pinkNoise :: 
    Double    -- ^  β: spectral distribution
              --    0: White noise
              --   -1: Pink noise
              --   -2: Brownian (red) noise
    -> Int       -- ^ samples
    -> Int       -- ^ random seed
    -> Vector Double 
pinkNoise b s r = let pre = linspace s (0::Double,fromIntegral (s-1))
                      post = linspace s (fromIntegral s,1)
                      freq = vjoin [pre/(fromIntegral s),post/(fromIntegral s)]
                      s_f = GV.map (**(b/2)) (freq**2) 
                      s_f' = GV.map (\x -> if isInfinite x then 0 else x) s_f
                      phi = randomVector r Uniform (2*s)
                  in subVector 0 s $ fst $ fromComplex $ F.ifft ((complex $ s_f'**0.5) * (toComplex (cos(2*pi*phi),sin(2*pi*phi))))

-- | generate noise from a power spectrum
powerNoise :: Vector Double   -- ^ the power spectrum
           -> Int             -- ^ random seed
           -> Vector Double
powerNoise psd r = let ln = GV.length psd
                       freq = vjoin [fromList [0],psd, (fromList . reverse . tail . toList) psd]
                       phi = randomVector r Uniform (2*ln)
                   in (fromIntegral ln) * (subVector 0 (ln-1) $ fst $ fromComplex $ F.ifft ((complex $ freq) * (toComplex (cos(2*pi*phi),sin(2*pi*phi)))))
