-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Estimation.Frequency.WLP
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains a few algorithms for weighted linear predictors
-- for estimating the frequency of a complex sinusoid in noise.
--
-----------------------------------------------------------------------------

module DSP.Estimation.Frequency.WLP (wlp, lrp, kay, lw, ckq,) where

import DSP.Basic((^!))
import Data.Array
import Data.Complex

-- | The weighted linear predictor form of the frequency estimator

wlp :: (Ix a, Integral a, RealFloat b) => Array a b -- ^ window
    -> Array a (Complex b) -- ^ z
    -> b -- ^ w

wlp w z = phase (sum [ (w!t :+ 0) * z!t * conjugate (z!(t-1)) | t <- [1..(n-1)] ])
    where n = snd (bounds z) + 1

-- | WLP using Lank, Reed, and Pollon's window

lrp :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ z
    -> b -- ^ w

lrp = processArray (\n _ _ -> recip (n-1))

-- | WLP using kay's window

kay :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ z
    -> b -- ^ w

kay = processArray (\n _ t -> kayWin n t)

kayWin :: Fractional b => b -> b -> b
kayWin n t = 6*t*(n-t) / (n*(n^!2-1))

-- | WLP using Lovell and Williamson's window

lw :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ z
    -> b -- ^ w

lw z =
   processArray
      (\ n ti t -> kayWin n t /
          (magnitude (z!ti) * magnitude (z!(ti-1)))) z

-- | WLP using Clarkson, Kootsookos, and Quinn's window

ckq :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ z
    -> b -- ^ rho
    -> b -- ^ sigma
    -> b -- ^ w

ckq z rho sig =
   processArray
      (\n ->
         let num t = sinh (n * th) - sinh (t * th) - sinh ((n-t) * th)
             den = (n-1) * sinh (n * th) -
                      2 * sinh (0.5 * n * th)
                        * sinh (0.5 * (n-1) * th) / sinh (0.5 * th)
             sigRho2 = (sig / rho) ^! 2
             th = log (1 + sig^!2 / rho^!2 + sqrt ((sigRho2+1) * sigRho2))
         in  \_ t -> num t / den) z

processArray ::
   (Integral a, Ix a, RealFloat b) =>
   (b -> a -> b -> b) -> Array a (Complex b) -> b
processArray f z =
  let n = snd (bounds z) + 1
      g = f (fromIntegral n)
      bnd = (1,n-1)
  in  wlp (listArray bnd
              (map (\t -> g t (fromIntegral t)) (range bnd))) z
