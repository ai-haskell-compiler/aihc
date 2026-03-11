-- |
-- Module      :  ELynx.MarkovProcess.GammaRateHeterogeneity
-- Description :  Discrete gamma rate heterogeneity
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Feb 28 14:09:11 2019.
--
-- At the moment, a mixture model is used to emulate gamma rate heterogeneity. This
-- does not come with huge run time increases when simulating data. For inference
-- however, it would make a lot of sense to reuse the Eigendecomposition for all
-- rate heterogeneity components though.
module ELynx.MarkovProcess.GammaRateHeterogeneity
  ( summarizeGammaRateHeterogeneity,
    expand,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V
import qualified ELynx.MarkovProcess.MixtureModel as M
import qualified ELynx.MarkovProcess.PhyloModel as P
import qualified ELynx.MarkovProcess.SubstitutionModel as S
import Numeric.Integration.TanhSinh
import Statistics.Distribution
import Statistics.Distribution.Gamma
import Prelude hiding (repeat)

-- | Short summary of gamma rate heterogeneity parameters.
summarizeGammaRateHeterogeneity :: Int -> Double -> [BL.ByteString]
summarizeGammaRateHeterogeneity n alpha =
  map
    BL.pack
    [ "Discrete gamma rate heterogeneity.",
      "Number of categories: " ++ show n,
      "Shape parameter of gamma distribution: " ++ show alpha,
      "Rates: " ++ show (getMeans n alpha)
    ]

-- | For a given number of rate categories, a gamma shape parameter alpha and a
-- substitution model, compute the scaled substitution models corresponding to
-- the gamma rates.
expand :: Int -> Double -> P.PhyloModel -> P.PhyloModel
expand n alpha (P.SubstitutionModel sm) =
  P.MixtureModel $ expandSubstitutionModel n alpha sm
expand n alpha (P.MixtureModel mm) =
  P.MixtureModel $ expandMixtureModel n alpha mm

getName :: Int -> Double -> String
getName n alpha =
  " with discrete gamma rate heterogeneity; "
    ++ show n
    ++ " categories; "
    ++ "shape parameter "
    ++ show alpha

splitSubstitutionModel :: Int -> Double -> S.SubstitutionModel -> V.Vector S.SubstitutionModel
splitSubstitutionModel n alpha sm = renamedSMs
  where
    means = getMeans n alpha
    scaledSMs = V.map (`S.scale` sm) means
    names = V.fromList $ map (("; gamma rate category " ++) . show) [1 :: Int ..]
    renamedSMs = V.zipWith S.appendName names scaledSMs

expandSubstitutionModel :: Int -> Double -> S.SubstitutionModel -> M.MixtureModel
expandSubstitutionModel n alpha sm = M.fromSubstitutionModels name S.DoNotNormalize ws sms
  where
    name = S.name sm <> getName n alpha
    ws = V.replicate n 1.0
    sms = splitSubstitutionModel n alpha sm

expandMixtureModel :: Int -> Double -> M.MixtureModel -> M.MixtureModel
expandMixtureModel n alpha mm = M.concatenate name renamedMMs
  where
    name = M.name mm <> getName n alpha
    means = getMeans n alpha
    scaledMMs = V.map (`M.scale` mm) means
    names = V.fromList $ map (("; gamma rate category " ++) . show) [1 :: Int ..]
    renamedMMs = V.zipWith M.appendNameComponents names scaledMMs

-- For a given number of rate categories 'n' and a shape parameter 'alpha' (the
-- rate or scale is set such that the mean is 1.0), return a list of rates that
-- represent the respective categories. Use the mean rate for each category.
getMeans :: Int -> Double -> V.Vector Double
getMeans n alpha
  | n <= 0 = error "getMeans: Number of rate categories is zero or negative."
  | otherwise = means <> pure lastMean
  where
    gamma = gammaDistr alpha (1.0 / alpha)
    quantiles =
      [quantile gamma (fromIntegral i / fromIntegral n) | i <- [0 .. n]]
    -- Calculate the mean rate. Multiplication with the number of rate
    -- categories 'n' is necessary because in each n-quantile the
    -- probability mass is 1/n.
    meanFunc x = fromIntegral n * x * density gamma x
    -- Only calculate the first (n-1) categories with normal integration.
    means =
      V.fromList
        [ integralAToB meanFunc (quantiles !! i) (quantiles !! (i + 1))
          | i <- [0 .. n - 2]
        ]
    -- The last category has to be calculated with an improper integration.
    lastMean = integralAToInf meanFunc (quantiles !! (n - 1))

eps :: Double
eps = 1e-8

-- The integration method to use
method :: (Double -> Double) -> Double -> Double -> [Result]
method = parSimpson

-- Helper function for a normal integral from 'a' to 'b'.
integralAToB :: (Double -> Double) -> Double -> Double -> Double
integralAToB f a b = result . absolute eps $ method f a b

-- Helper function for an improper integral from 'a' to infinity.
integralAToInf :: (Double -> Double) -> Double -> Double
integralAToInf f a =
  (result . absolute eps $ nonNegative method f) - integralAToB f 0 a
