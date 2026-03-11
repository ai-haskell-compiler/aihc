-- |
-- Module      :  ELynx.MarkovProcess.CXXModels
-- Description :  C10 to C60 models
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Feb 26 16:44:33 2019.
--
-- Quang, BL. S., Gascuel, O. & Lartillot, N. Empirical profile mixture models for
-- phylogenetic reconstruction. Bioinformatics 24, 2317–2323 (2008).
--
-- XXX: For now, I only provide Poisson exchangeabilities.
module ELynx.MarkovProcess.CXXModels
  ( cxx,
  )
where

import qualified Data.Vector as V
import ELynx.MarkovProcess.AminoAcid
import ELynx.MarkovProcess.CXXModelsData
import qualified ELynx.MarkovProcess.MixtureModel as M
import ELynx.MarkovProcess.RateMatrix
import qualified ELynx.MarkovProcess.SubstitutionModel as S

-- | Create CXX model with given number of components and probably with custom
-- weights.
cxx :: Int -> Maybe [M.Weight] -> M.MixtureModel
cxx 10 (Just ws) = c10CustomWeights ws
cxx 20 (Just ws) = c20CustomWeights ws
cxx 30 (Just ws) = c30CustomWeights ws
cxx 40 (Just ws) = c40CustomWeights ws
cxx 50 (Just ws) = c50CustomWeights ws
cxx 60 (Just ws) = c60CustomWeights ws
cxx 10 Nothing = c10
cxx 20 Nothing = c20
cxx 30 Nothing = c30
cxx 40 Nothing = c40
cxx 50 Nothing = c50
cxx 60 Nothing = c60
cxx n _ =
  error $ "cxx: cannot create CXX model with " ++ show n ++ " components."

-- | C10 model.
c10 :: M.MixtureModel
c10 = cxxFromStatDistsAndWeights c10Weights c10StatDists

-- | C20 model.
c20 :: M.MixtureModel
c20 = cxxFromStatDistsAndWeights c20Weights c20StatDists

-- | C30 model.
c30 :: M.MixtureModel
c30 = cxxFromStatDistsAndWeights c30Weights c30StatDists

-- | C40 model.
c40 :: M.MixtureModel
c40 = cxxFromStatDistsAndWeights c40Weights c40StatDists

-- | C50 model.
c50 :: M.MixtureModel
c50 = cxxFromStatDistsAndWeights c50Weights c50StatDists

-- | C60 model.
c60 :: M.MixtureModel
c60 = cxxFromStatDistsAndWeights c60Weights c60StatDists

-- | C10 model with custom weights.
c10CustomWeights :: [M.Weight] -> M.MixtureModel
c10CustomWeights ws
  | length ws == 10 = cxxFromStatDistsAndWeights ws c10StatDists
  | otherwise = error "Number of weights does not match C10 model."

-- | C20 model with custom weights.
c20CustomWeights :: [M.Weight] -> M.MixtureModel
c20CustomWeights ws
  | length ws == 20 = cxxFromStatDistsAndWeights ws c20StatDists
  | otherwise = error "Number of weights does not match C20 model."

-- | C30 model with custom weights.
c30CustomWeights :: [M.Weight] -> M.MixtureModel
c30CustomWeights ws
  | length ws == 30 = cxxFromStatDistsAndWeights ws c30StatDists
  | otherwise = error "Number of weights does not match C30 model."

-- | C40 model with custom weights.
c40CustomWeights :: [M.Weight] -> M.MixtureModel
c40CustomWeights ws
  | length ws == 40 = cxxFromStatDistsAndWeights ws c40StatDists
  | otherwise = error "Number of weights does not match C40 model."

-- | C50 model with custom weights.
c50CustomWeights :: [M.Weight] -> M.MixtureModel
c50CustomWeights ws
  | length ws == 50 = cxxFromStatDistsAndWeights ws c50StatDists
  | otherwise = error "Number of weights does not match C50 model."

-- | C60 model with custom weights.
c60CustomWeights :: [M.Weight] -> M.MixtureModel
c60CustomWeights ws
  | length ws == 60 = cxxFromStatDistsAndWeights ws c60StatDists
  | otherwise = error "Number of weights does not match C60 model."

cxxName :: Int -> String
cxxName nComps = 'C' : show nComps

componentName :: Int -> Int -> String
componentName nComps comp = cxxName nComps ++ "; component " ++ show comp

-- Keep in mind, that when using different exchangeabilities, I have to decide
-- about global or local normalization.
cxxSubstitutionModelFromStatDist ::
  Int -> Int -> StationaryDistribution -> S.SubstitutionModel
cxxSubstitutionModelFromStatDist nComps comp d =
  poissonCustom
    (Just name)
    S.DoNotNormalize
    (normalizeSD d)
  where
    name = componentName nComps comp

cxxSubstitutionModelsFromStatDists ::
  [StationaryDistribution] -> [S.SubstitutionModel]
cxxSubstitutionModelsFromStatDists ds =
  zipWith
    (cxxSubstitutionModelFromStatDist nComp)
    [1 ..]
    ds
  where
    nComp = length ds

-- XXX: The use of `Data.List.NonEmpty.fromList` is daring, but since this
-- function is not exported and only applied to predefined non-empty lists, it
-- should be OK.
cxxFromStatDistsAndWeights ::
  [M.Weight] -> [StationaryDistribution] -> M.MixtureModel
cxxFromStatDistsAndWeights ws ds =
  M.fromSubstitutionModels
    (cxxName n)
    S.DoNormalize
    (V.fromList ws)
    sms
  where
    n = length ds
    sms = V.fromList $ cxxSubstitutionModelsFromStatDists ds
