{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Physics.ForceLayout
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- A simple, Haskell-native simulator for doing force-directed layout,
-- /e.g./ of trees or graphs.
--
-- To use, just create an 'Ensemble' like so:
--
-- > import           Control.Lens        ((&), (.~))
-- > import           Data.Default        (def)
-- > import qualified Data.Map            as M
-- > import           Linear.Affine
-- > import           Linear.V2
-- > import           Physics.ForceLayout
-- >
-- > e :: Ensemble V2 Double
-- > e = Ensemble [ (edges,    hookeForce 0.05 4)
-- >              , (allPairs, coulombForce 1)
-- >              ]
-- >              particleMap
-- >   where edges       = [(1,2), (2,3), (2,5), (3,5), (3,4), (4,5)]
-- >         allPairs    = [(x,y) | x <- [1..4], y <- [x+1..5]]
-- >         particleMap = M.fromList . zip [1..]
-- >                     . map (initParticle . P . uncurry V2)
-- >                     $ [ (2.0, 3.1), (6.3, 7.2)
-- >                       , (0.3, 4.2), (1.6, -1.1)
-- >                       , (4.8, 2.9)
-- >                       ]
--
-- Then run a simulation using either 'simulate' (to get the list of
-- all intermediate states) or 'forceLayout' (to get only the ending
-- state):
--
-- > e' :: Ensemble V2 Double
-- > e' = forceLayout (def & damping     .~ 0.8
-- >                       & energyLimit .~ Just 0.001
-- >                       & stepLimit   .~ Nothing
-- >                  )
-- >                  e
--
-- See the diagrams-contrib package
-- (<http://github.com/diagrams/diagrams-contrib/>) for more
-- examples.
-----------------------------------------------------------------------------

module Physics.ForceLayout
       ( -- * Data structures

         Particle(..), pos, vel, force
       , initParticle

       , PID
       , Edge
       , Ensemble(..), forces, particles

         -- * Pre-defined forces

       , hookeForce
       , coulombForce
       , distForce

         -- * Running simulations

       , ForceLayoutOpts(..)
       , damping, energyLimit, stepLimit
       , simulate
       , forceLayout

         -- * Internals

       , ensembleStep
       , particleStep
       , recalcForces
       , kineticEnergy

       ) where

import           Data.Default
import qualified Data.Foldable      as F
import qualified Data.Map           as M
import           Data.Monoid

import           Control.Lens

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

------------------------------------------------------------
--  Particles
------------------------------------------------------------

-- | A particle has a current position, current velocity, and current
--   force acting on it.
data Particle v n = Particle { _pos   :: Point v n
                             , _vel   :: v n
                             , _force :: v n
                             }
  deriving Eq

makeLenses ''Particle

-- | Create an initial particle at rest at a particular location.
initParticle :: (Additive v, Num n) => Point v n -> Particle v n
initParticle p = Particle p zero zero

------------------------------------------------------------
--  Ensembles
------------------------------------------------------------

-- | Used to uniquely identify particles.
type PID = Int

-- | An edge is a pair of particle identifiers.
type Edge = (PID, PID)

-- | An @Ensemble@ is a physical configuration of particles.  It
--   consists of a mapping from particle IDs (unique integers) to
--   particles, and a list of forces that are operative.  Each force
--   has a list of edges to which it applies, and is represented by a
--   function giving the force between any two points.
data Ensemble v n = Ensemble { _forces    :: [([Edge], Point v n -> Point v n -> v n)]
                             , _particles :: M.Map PID (Particle v n)
                             }

makeLenses ''Ensemble

------------------------------------------------------------
--  Simulation internals
------------------------------------------------------------

-- | Simulate one time step for an entire ensemble, with the given
--   damping factor.
ensembleStep :: (Additive v, Num n) => n -> Ensemble v n -> Ensemble v n
ensembleStep d = (over particles . M.map) (particleStep d) . recalcForces

-- | Simulate one time step for a particle (assuming the force acting
--   on it has already been computed), with the given damping factor.
particleStep :: (Additive v, Num n) => n -> Particle v n -> Particle v n
particleStep d = stepPos . stepVel
  where stepVel p = vel .~ (d *^ (p^.vel ^+^ p^.force)) $ p
        stepPos p = pos %~ (.+^ p^.vel) $ p

-- | Recalculate all the forces acting in the next time step of an
--   ensemble.
recalcForces :: (Additive v, Num n) => Ensemble v n -> Ensemble v n
recalcForces = calcForces . zeroForces
  where zeroForces = (particles %~) . M.map $ force .~ zero
        calcForces (Ensemble fs ps)
          = Ensemble fs
            (ala Endo F.foldMap (concatMap (\(es, f) -> (map (mkForce f) es)) fs) ps)
        -- mkForce :: (Point v n -> Point v n -> v n)
        --         -> Edge
        --         -> M.Map Int (Particle v n)
        --         -> M.Map Int (Particle v n)
        mkForce f (i1, i2) m
          = case (M.lookup i1 m, M.lookup i2 m) of
              (Just p1, Just p2) ->
                ( M.adjust (force %~ (^+^ f (p1^.pos) (p2^.pos))) i1
                . M.adjust (force %~ (^-^ f (p1^.pos) (p2^.pos))) i2)
                m
              _                  -> m

-- | Compute the total kinetic energy of an ensemble.
kineticEnergy :: (Metric v, Num n) => Ensemble v n -> n
kineticEnergy = ala Sum F.foldMap . fmap (quadrance . view vel) . view particles

------------------------------------------------------------
--  Simulation
------------------------------------------------------------

-- | Options for customizing a simulation.
data ForceLayoutOpts n =
  FLOpts
  { _damping     :: n         -- ^ Damping factor to be
                              --   applied at each step.
                              --   Should be between 0 and 1.
                              --   The default is 0.8.
  , _energyLimit :: Maybe n   -- ^ Kinetic energy below which
                              --   simulation should stop.
                              --   If @Nothing@, pay no
                              --   attention to kinetic
                              --   energy.  The default is
                              --   @Just 0.001@.
  , _stepLimit   :: Maybe Int -- ^ Maximum number of
                              --   simulation steps.  If
                              --   @Nothing@, pay no
                              --   attention to the number of
                              --   steps.  The default is
                              --   @Just 1000@.
  }

makeLenses ''ForceLayoutOpts

instance Fractional n => Default (ForceLayoutOpts n) where
  def = FLOpts
        { _damping     = 0.8
        , _energyLimit = Just 0.001
        , _stepLimit   = Just 1000
        }

-- | Simulate a starting ensemble according to the given options,
--   producing a list of all the intermediate ensembles.  Useful for,
--   /e.g./, making an animation.  Note that the resulting list could
--   be infinite, if a 'stepLimit' is not specified and either the
--   kinetic energy never falls below the specified threshold, or no
--   energy threshold is specified.
simulate :: (Metric v, Num n, Ord n)
         => ForceLayoutOpts n -> Ensemble v n -> [Ensemble v n]
simulate opts e
  = (e:)
  . takeWhile (maybe (const True) (<) (opts ^. energyLimit) . kineticEnergy)
  . maybe id take (opts ^. stepLimit)
  . drop 1
  . iterate (ensembleStep (opts ^. damping))
  $ e

-- | Run a simluation from a starting ensemble, yielding either the
--   first ensemble to have kinetic energy below the 'energyLimit' (if
--   given), or the ensemble that results after a number of steps
--   equal to the 'stepLimit' (if given), whichever comes first.
--   Otherwise @forceLayout@ will not terminate.
forceLayout :: (Metric v, Num n, Ord n)
            => ForceLayoutOpts n -> Ensemble v n -> Ensemble v n
forceLayout opts = last . simulate opts

------------------------------------------------------------
--  Standard forces
------------------------------------------------------------

-- | @distForce f p1 p2@ computes the force between two points as a
--   multiple of the unit vector in the direction from @p1@ to @p2@,
--   given a function @f@ which computes the force's magnitude as a
--   function of the distance between the points.
distForce :: (Metric v, Floating n) => (n -> n) -> Point v n -> Point v n -> v n
distForce f p1 p2 = withLength (f (distance p1 p2)) (p2 .-. p1)
  where withLength s v = s *^ signorm v

-- | @hookeForce k l p1 p2@ computes the force on @p1@, assuming that
--   @p1@ and @p2@ are connected by a spring with equilibrium length @l@
--   and spring constant @k@.
hookeForce :: (Metric v, Floating n) => n -> n -> Point v n -> Point v n -> v n
hookeForce k l = distForce (\d -> k * (d - l))

-- | @coulombForce k@ computes the electrostatic repulsive force
--   between two charged particles, with constant of proportionality
--   @k@.
coulombForce :: (Metric v, Floating n) => n -> Point v n -> Point v n -> v n
coulombForce k = distForce (\d -> -k/(d*d))
