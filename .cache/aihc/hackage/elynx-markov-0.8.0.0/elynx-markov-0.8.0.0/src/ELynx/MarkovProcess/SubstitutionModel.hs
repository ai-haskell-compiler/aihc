-- |
-- Module      :  ELynx.MarkovProcess.SubstitutionModel
-- Description :  Data type describing substitution model
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Jan 29 19:10:46 2019.
--
-- To be imported qualified.
module ELynx.MarkovProcess.SubstitutionModel
  ( -- * Types
    Name,
    Params,
    SubstitutionModel,

    -- * Accessors
    alphabet,
    name,
    params,
    stationaryDistribution,
    exchangeabilityMatrix,
    rateMatrix,
    totalRate,

    -- * Building substitution models
    Normalize (..),
    substitutionModel,

    -- * Transformations
    scale,
    normalize,
    appendName,
  )
where

import qualified Data.Vector.Storable as V
import ELynx.Alphabet.Alphabet
import qualified ELynx.MarkovProcess.RateMatrix as R
import qualified Numeric.LinearAlgebra as LinAlg

-- | Name of substitution model; abstracted and subject to change.
type Name = String

-- | Parameters of substitution model. May be the empty list.
type Params = [Double]

-- XXX: Use a proper data type. For example:
-- data SubstitutionModelAA = LG | WAG | LG-Custom dist | ...
-- data SubstitutionModelNuc = JC | HKY p1 p2 ... | GTR p1 p2 ...
--
-- I thought about this a lot, and it seems easier like it is at the moment.
-- Since the data types are abstracted anyways, not much harm can be done. Of
-- course, conflicting substitution models can be declared, or duplicate ones
-- with different names, but well...

-- | Complete definition of a substitution model. Create instances with
-- 'substitutionModel'. A substitution model has an alphabet, a name, and a list
-- of parameters (e.g., the kappa value for the HKY model). Further, the
-- transition rate matrix is defined by a stationary distribution and a set of
-- exchangeabilities.
data SubstitutionModel = SubstitutionModel
  { -- | Alphabet
    alphabet :: Alphabet,
    -- | Name
    name :: Name,
    -- | List of parameters
    params :: Params,
    -- | Stationary distribution
    stationaryDistribution :: R.StationaryDistribution,
    -- | Exchangeability matrix
    exchangeabilityMatrix :: R.ExchangeabilityMatrix
  }
  deriving (Show, Read)

-- | Calculate rate matrix from substitution model.
rateMatrix :: SubstitutionModel -> R.RateMatrix
rateMatrix sm =
  R.fromExchangeabilityMatrix
    (exchangeabilityMatrix sm)
    (stationaryDistribution sm)

-- | Get scale of substitution model.
totalRate :: SubstitutionModel -> Double
totalRate sm = R.totalRate (rateMatrix sm)

normalizeSumVec :: V.Vector Double -> V.Vector Double
normalizeSumVec v = V.map (/ s) v
  where
    s = V.sum v
{-# INLINE normalizeSumVec #-}

-- | Should the substitution model be normalized?
data Normalize = DoNormalize | DoNotNormalize
  deriving (Show, Read, Eq)

-- | Create normalized 'SubstitutionModel'. See 'normalize'.
substitutionModel ::
  Alphabet ->
  Name ->
  Params ->
  Normalize ->
  R.StationaryDistribution ->
  R.ExchangeabilityMatrix ->
  SubstitutionModel
substitutionModel c n ps nz d e =
  if R.isValid d
    then
      let sm = SubstitutionModel c n ps (normalizeSumVec d) e
       in case nz of
            DoNormalize -> normalize sm
            DoNotNormalize -> sm
    else
      error $
        "substitionModel: Stationary distribution does not sum to 1.0: "
          ++ show d

-- | Scale the rate of a substitution model by given factor.
scale :: Double -> SubstitutionModel -> SubstitutionModel
scale r sm = sm {exchangeabilityMatrix = em'}
  where
    em' = LinAlg.scale r $ exchangeabilityMatrix sm

-- | Normalize a substitution model, so that, on average, one substitution
-- happens per unit time.
normalize :: SubstitutionModel -> SubstitutionModel
normalize sm = scale (1.0 / r) sm where r = totalRate sm

-- | Abbend to name.
appendName :: Name -> SubstitutionModel -> SubstitutionModel
appendName n sm = sm {name = n'} where n' = name sm <> n
