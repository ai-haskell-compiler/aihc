{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFPCore.Electricity
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 21 of the book Learn Physics with Functional Programming
-}

module LPFPCore.Electricity where

import LPFPCore.SimpleVec
    ( Vec(..), R, (*^), iHat )
import LPFPCore.Mechanics3D
    ( ParticleState(..), defaultParticleState )
import LPFPCore.MultipleObjects
    ( TwoBodyForce, MultiParticleState(..), Force(..), statesMPS
    , eulerCromerMPS, centralForce )

type Charge = R

elementaryCharge :: Charge
elementaryCharge = 1.602176634e-19  -- in Coulombs

coulombMagnitude :: Charge -> Charge -> R -> R
coulombMagnitude q1 q2 r
    = let k = 9e9  -- in N m^2 / C^2
      in k * abs (q1 * q2) / r**2

coulombForce :: TwoBodyForce
coulombForce st1 st2
    = let k = 9e9  -- N m^2 / C^2
          q1 = charge st1
          q2 = charge st2
      in centralForce (\r -> k * q1 * q2 / r**2) st1 st2

twoProtonStates :: R                     -- time step
                -> MultiParticleState    -- initial 2-particle state
                -> [MultiParticleState]  -- infinite list of states
twoProtonStates dt
    = statesMPS (eulerCromerMPS dt) [InternalForce 1 0 coulombForce]

-- protons are released from rest
initialTwoProtonState :: R  -- initial separation
                      -> MultiParticleState
initialTwoProtonState d
    = let protonMass = 1.673e-27  -- in kg
      in MPS [defaultParticleState { mass   = protonMass
                                   , charge = elementaryCharge
                                   , posVec = (-d/2) *^ iHat
                                   }
             ,defaultParticleState { mass   = protonMass
                                   , charge = elementaryCharge
                                   , posVec = ( d/2) *^ iHat
                                   }
             ]

oneProtonVelocity :: R        -- dt
                  -> R        -- starting separation
                  -> [(R,R)]  -- (time,velocity) pairs
oneProtonVelocity dt d
    = let state0 = initialTwoProtonState d
      in [(time st2, xComp $ velocity st2)
              | MPS [_,st2] <- twoProtonStates dt state0]

tvPairs :: [(R,R)]
tvPairs = takeWhile (\(t,_) -> t <= 2e-2) $
          oneProtonVelocity 1e-5 1e-2

oneProtonPosition :: R        -- dt
                  -> R        -- starting separation
                  -> [(R,R)]  -- (time,position) pairs
oneProtonPosition dt d
    = undefined dt d
