{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | 
Module      :  LPFPCore.Lorentz
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 28 of the book Learn Physics with Functional Programming
-}

module LPFPCore.Lorentz where

import LPFPCore.SimpleVec ( R, Vec, (^+^), (*^), (^*), (^/), (><), zeroV )
import LPFPCore.Mechanics1D ( RealVectorSpace(..), Diff(..), rungeKutta4 )
import LPFPCore.Mechanics3D ( HasTime(..) )
import LPFPCore.CoordinateSystems ( Position(..), VectorField, origin
                         , shiftPosition, addVectorFields )

data ParticleFieldState = ParticleFieldState { mass          :: R
                                             , charge        :: R
                                             , time          :: R
                                             , position      :: Position
                                             , velocity      :: Vec
                                             , electricField :: VectorField
                                             , magneticField :: VectorField }

data DParticleFieldState = DParticleFieldState { dmdt :: R
                                               , dqdt :: R
                                               , dtdt :: R
                                               , drdt :: Vec
                                               , dvdt :: Vec
                                               , dEdt :: VectorField
                                               , dBdt :: VectorField }

instance RealVectorSpace DParticleFieldState where
    dst1 +++ dst2
        = DParticleFieldState { dmdt = dmdt dst1  +  dmdt dst2
                              , dqdt = dqdt dst1  +  dqdt dst2
                              , dtdt = dtdt dst1  +  dtdt dst2
                              , drdt = drdt dst1 ^+^ drdt dst2
                              , dvdt = dvdt dst1 ^+^ dvdt dst2
                              , dEdt = addVectorFields [dEdt dst1, dEdt dst2]
                              , dBdt = addVectorFields [dBdt dst1, dBdt dst2]
                              }
    scale w dst
        = DParticleFieldState { dmdt = w *  dmdt dst
                              , dqdt = w *  dqdt dst
                              , dtdt = w *  dtdt dst
                              , drdt = w *^ drdt dst
                              , dvdt = w *^ dvdt dst
                              , dEdt = (w *^) . (dEdt dst)
                              , dBdt = (w *^) . (dBdt dst)
                              }

instance Diff ParticleFieldState DParticleFieldState where
    shift dt dst st
        = ParticleFieldState
          { mass          = mass     st  +  dmdt dst  * dt
          , charge        = charge   st  +  dqdt dst  * dt
          , time          = time     st  +  dtdt dst  * dt
          , position      = shiftPosition (drdt dst ^* dt) (position st)
          , velocity      = velocity st ^+^ dvdt dst ^* dt
          , electricField = \r -> electricField st r ^+^ dEdt dst r ^* dt
          , magneticField = \r -> magneticField st r ^+^ dBdt dst r ^* dt
          }

instance HasTime ParticleFieldState where
    timeOf = time

lorentzForce :: ParticleFieldState -> Vec
lorentzForce (ParticleFieldState _m q _t r v eF bF)
    = q *^ (eF r ^+^ v >< bF r)

newtonSecondPFS :: ParticleFieldState -> DParticleFieldState
newtonSecondPFS st
    = let v = velocity st
          a = lorentzForce st ^/ mass st
      in DParticleFieldState { dmdt = 0            -- dm/dt
                             , dqdt = 0            -- dq/dt
                             , dtdt = 1            -- dt/dt
                             , drdt = v            -- dr/dt
                             , dvdt = a            -- dv/dt
                             , dEdt = const zeroV  -- dE/dt
                             , dBdt = const zeroV  -- dB/dt
                             }

pfsUpdate :: R  -- time step
          -> ParticleFieldState -> ParticleFieldState
pfsUpdate dt = rungeKutta4 dt newtonSecondPFS

defaultPFS :: ParticleFieldState
defaultPFS = ParticleFieldState { mass          = 0
                                , charge        = 0
                                , time          = 0
                                , position      = origin
                                , velocity      = zeroV
                                , electricField = const zeroV
                                , magneticField = const zeroV }

scalePos :: R -> Position -> Position
scalePos metersPerVis (Cart x y z)
    = Cart (x/metersPerVis) (y/metersPerVis) (z/metersPerVis)

newtonSecondPFS' :: [ParticleFieldState -> Vec]
                 -> ParticleFieldState -> DParticleFieldState
newtonSecondPFS' fs st = undefined fs st
