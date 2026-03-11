-- |
-- Module      :  ELynx.MarkovProcess.MixtureModel
-- Description :  Mixture models are a set of substitution models with weights
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Jan 29 19:17:40 2019.
--
-- To be imported qualified.
module ELynx.MarkovProcess.MixtureModel
  ( -- * Types
    Weight,
    Component (weight, substModel),
    MixtureModel (name, alphabet, components),

    -- * Getters
    getWeights,
    getSubstitutionModels,

    -- * Building mixture models
    fromSubstitutionModels,

    -- * Transformations
    concatenate,
    scale,
    normalize,
    appendNameComponents,
  )
where

import qualified Data.Vector as V
import ELynx.Alphabet.Alphabet hiding (all)
import qualified ELynx.MarkovProcess.SubstitutionModel as S
import Prelude

-- | Mixture model component weight.
type Weight = Double

-- | A mixture model component has a weight and a substitution model.
data Component = Component
  { weight :: Weight,
    substModel :: S.SubstitutionModel
  }
  deriving (Show, Read)

-- | A mixture model with its components.
data MixtureModel = MixtureModel
  { -- | Name
    name :: S.Name,
    alphabet :: Alphabet,
    components :: V.Vector Component
  }
  deriving (Show, Read)

-- | Get weights.
getWeights :: MixtureModel -> V.Vector Weight
getWeights = V.map weight . components

-- | Get substitution models.
getSubstitutionModels :: MixtureModel -> V.Vector S.SubstitutionModel
getSubstitutionModels = V.map substModel . components

normalizeGlobally :: V.Vector Weight -> V.Vector S.SubstitutionModel -> V.Vector S.SubstitutionModel
normalizeGlobally ws ss = V.map (S.scale $ recip c) ss
  where
    cks = V.map S.totalRate ss
    cNoWeights = V.sum $ V.zipWith (*) ws cks
    c = cNoWeights / V.sum ws

-- | Create a mixture model from a list of substitution models.
--
-- If 'S.Normalize' is 'S.DoNormalize', globally normalize the mixture model.
-- Global normalization has no effect if all components are already normalized.
fromSubstitutionModels ::
  S.Name ->
  S.Normalize ->
  V.Vector Weight ->
  V.Vector S.SubstitutionModel ->
  MixtureModel
fromSubstitutionModels n nz ws sms
  | null ws = error "fromSubstitutionModels: No weights given."
  | length ws /= length sms = error "fromSubstitutionModels: Number of weights and substitution models does not match."
  | not $ allEqual alphs = error "fromSubstitutionModels: alphabets of substitution models are not equal."
  | otherwise = MixtureModel n (V.head alphs) comps
  where
    smsNormalized = case nz of
      S.DoNormalize -> normalizeGlobally ws sms
      S.DoNotNormalize -> sms
    comps = V.zipWith Component ws smsNormalized
    alphs = V.map S.alphabet sms
    allEqual xs
      | V.null xs = True
      | otherwise = V.all (== V.head xs) xs

-- | Concatenate mixture models.
concatenate :: S.Name -> V.Vector MixtureModel -> MixtureModel
concatenate n mms = fromSubstitutionModels n S.DoNotNormalize ws sms
  where
    comps = V.concatMap components mms
    ws = V.map weight comps
    sms = V.map substModel comps

scaleComponent :: Double -> Component -> Component
scaleComponent s c = c {substModel = s'} where s' = S.scale s $ substModel c

-- | Scale all substitution models of the mixture model.
scale :: Double -> MixtureModel -> MixtureModel
scale s m = m {components = cs'}
  where
    cs = components m
    cs' = V.map (scaleComponent s) cs

-- | Globally normalize a mixture model so that on average one event happens per
-- unit time.
normalize :: MixtureModel -> MixtureModel
normalize mm = scale (1 / c) mm
  where
    c = sum $ V.zipWith (*) weights scales
    weights = getWeights mm
    scales = V.map S.totalRate $ getSubstitutionModels mm

appendNameComponent :: S.Name -> Component -> Component
appendNameComponent n c = c {substModel = s'}
  where
    s' = S.appendName n $ substModel c

-- | Append byte string to all substitution models of mixture model.
appendNameComponents :: S.Name -> MixtureModel -> MixtureModel
appendNameComponents n m = m {components = cs'}
  where
    cs = components m
    cs' = V.map (appendNameComponent n) cs
