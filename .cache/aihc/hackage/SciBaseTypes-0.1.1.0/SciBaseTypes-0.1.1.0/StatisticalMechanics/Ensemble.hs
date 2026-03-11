
-- | This module provides generic functionality to deal with ensembles in
-- statistical mechanics.

module StatisticalMechanics.Ensemble where

import Numeric.Log

import Statistics.Probability



-- | The state probability functions provide conversion from some types @a@
-- into non-normalized probabilities. For "real" applications, using the
-- @logProbability@ function is preferred. This functions allows for easy
-- abstraction when types @a@ are given as fractions of some actual value (say:
-- deka-cal), or are discretized.
--
-- The returned values are not normalized, because we do not now the total
-- evidence @Z@ until integration over all states has happened -- which is not
-- feasible in a number of problems.
--
-- TODO replace @()@ with temperature and results with non-normalized @P@ or
-- @LogP@, depending. At some point we want to have type-level physical
-- quantities, hence the need for the second type.

class StateProbability a where
  -- | Given a temperature and a state "energy", return the corresponding
  -- non-normalized probability.
  stateProbability
    ∷ Double
    -- ^ this is @k*T@
    → a
    -- ^ the energy (or discretized energy)
    → Probability NotNormalized Double
    -- ^ probability of being in state @a@, but only proportional up to @1/Z@.
  stateLogProbability
    ∷ Double
    -- ^ this is @1/(k * T)@
    → a
    -- ^ the energy (or discretized energy)
    → Log (Probability NotNormalized Double)
    -- ^ resulting probability

instance StateProbability Double where
  stateProbability kT x = Prob . exp . negate $ x/kT
  {-# Inline stateProbability #-}
  --stateLogProbability kT x = Exp . log . Prob . exp . negate $ x/kT
  stateLogProbability kT x = Exp . Prob . negate $ x/kT
  {-# Inline stateLogProbability #-}

