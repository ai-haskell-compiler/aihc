{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | 
Module      :  LPFPCore.Mechanics3D
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapters 16, 17, and 18 of the book Learn Physics with Functional Programming
-}

module LPFPCore.Mechanics3D where

import LPFPCore.SimpleVec
    ( R, Vec, PosVec, (^+^), (^-^), (*^), (^*), (^/), (<.>), (><)
    , vec, sumV, magnitude, zeroV, xComp, yComp, zComp, iHat, jHat, kHat)
import LPFPCore.Mechanics1D
    ( RealVectorSpace(..), Diff(..), NumericalMethod
    , Time, TimeStep, rungeKutta4, solver )

-- | Data type for the state of a single particle in three-dimensional space.
data ParticleState = ParticleState { mass     :: R
                                   , charge   :: R
                                   , time     :: R
                                   , posVec   :: Vec
                                   , velocity :: Vec }
                     deriving Show

-- | A default particle state.
defaultParticleState :: ParticleState
defaultParticleState = ParticleState { mass     = 1
                                     , charge   = 0
                                     , time     = 0
                                     , posVec   = zeroV
                                     , velocity = zeroV }

rockState :: ParticleState
rockState
    = defaultParticleState { mass     = 2                        -- kg
                           , velocity = 3 *^ iHat ^+^ 4 *^ kHat  -- m/s
                           }

-- | Data type for a one-body force.
type OneBodyForce = ParticleState -> Vec

-- | Data type for the time-derivative of a particle state.
data DParticleState = DParticleState { dmdt :: R
                                     , dqdt :: R
                                     , dtdt :: R
                                     , drdt :: Vec
                                     , dvdt :: Vec }
                      deriving Show

-- | Given a list of forces, return a differential equation
--   based on Newton's second law.
newtonSecondPS :: [OneBodyForce]
               -> ParticleState -> DParticleState  -- ^ a differential equation
newtonSecondPS fs st
    = let fNet = sumV [f st | f <- fs]
          m = mass st
          v = velocity st
          acc = fNet ^/ m
      in DParticleState { dmdt = 0    -- dm/dt
                        , dqdt = 0    -- dq/dt
                        , dtdt = 1    -- dt/dt
                        , drdt = v    -- dr/dt
                        , dvdt = acc  -- dv/dt
                        }

-- | The force of gravity near Earth's surface.
--   The z direction is toward the sky.
--   Assumes SI units.
earthSurfaceGravity :: OneBodyForce
earthSurfaceGravity st
    = let g = 9.80665  -- m/s^2
      in (-mass st * g) *^ kHat

-- | The force of the Sun's gravity on an object.
--   The origin is at center of the Sun.
--   Assumes SI units.
sunGravity :: OneBodyForce
sunGravity (ParticleState m _q _t r _v)
    = let bigG = 6.67408e-11  -- N m^2/kg^2
          sunMass = 1.98848e30  -- kg
      in (-bigG * sunMass * m) *^ r ^/ magnitude r ** 3

-- | The force of air resistance on an object.
airResistance :: R  -- ^ drag coefficient
              -> R  -- ^ air density
              -> R  -- ^ cross-sectional area of object
              -> OneBodyForce
airResistance drag rho area (ParticleState _m _q _t _r v)
    = (-0.5 * drag * rho * area * magnitude v) *^ v

-- | The force of wind on an object.
windForce :: Vec  -- ^ wind velocity
          -> R    -- ^ drag coefficient
          -> R    -- ^ air density
          -> R    -- ^ cross-sectional area of object
          -> OneBodyForce
windForce vWind drag rho area (ParticleState _m _q _t _r v)
    = let vRel = v ^-^ vWind
      in (-0.5 * drag * rho * area * magnitude vRel) *^ vRel

-- | The force of uniform electric and magnetic fields on an object.
uniformLorentzForce :: Vec  -- ^ E
                    -> Vec  -- ^ B
                    -> OneBodyForce
uniformLorentzForce vE vB (ParticleState _m q _t _r v)
    = q *^ (vE ^+^ v >< vB)

-- | Euler-Cromer method for the 'ParticleState' data type.
eulerCromerPS :: TimeStep        -- dt for stepping
              -> NumericalMethod ParticleState DParticleState
eulerCromerPS dt deriv st
    = let t   = time     st
          r   = posVec   st
          v   = velocity st
          dst = deriv st
          acc = dvdt dst
          v'  = v ^+^ acc ^* dt
      in st { time     = t  +         dt
            , posVec   = r ^+^ v'  ^* dt
            , velocity = v ^+^ acc ^* dt
            }

instance RealVectorSpace DParticleState where
    dst1 +++ dst2
        = DParticleState { dmdt = dmdt dst1  +  dmdt dst2
                         , dqdt = dqdt dst1  +  dqdt dst2
                         , dtdt = dtdt dst1  +  dtdt dst2
                         , drdt = drdt dst1 ^+^ drdt dst2
                         , dvdt = dvdt dst1 ^+^ dvdt dst2
                         }
    scale w dst
        = DParticleState { dmdt = w *  dmdt dst
                         , dqdt = w *  dqdt dst
                         , dtdt = w *  dtdt dst
                         , drdt = w *^ drdt dst
                         , dvdt = w *^ dvdt dst
                         }

instance Diff ParticleState DParticleState where
    shift dt dps (ParticleState m q t r v)
        = ParticleState (m  +  dmdt dps  * dt)
                        (q  +  dqdt dps  * dt)
                        (t  +  dtdt dps  * dt)
                        (r ^+^ drdt dps ^* dt)
                        (v ^+^ dvdt dps ^* dt)

-- | Given a numerical method,
--   a list of one-body forces, and an initial state,
--   return a list of states describing how the particle
--   evolves in time.
statesPS :: NumericalMethod ParticleState DParticleState  -- ^ numerical method
         -> [OneBodyForce]  -- ^ list of force funcs
         -> ParticleState -> [ParticleState]  -- ^ evolver
statesPS method = iterate . method . newtonSecondPS

-- | Given a numerical method and a list of one-body forces,
--   return a state-update function.
updatePS :: NumericalMethod ParticleState DParticleState
         -> [OneBodyForce]
         -> ParticleState -> ParticleState
updatePS method = method . newtonSecondPS

-- | Given a numerical method,
--   a list of one-body forces, and an initial state,
--   return a position function describing how the particle
--   evolves in time.
positionPS :: NumericalMethod ParticleState DParticleState
           -> [OneBodyForce]  -- ^ list of force funcs
           -> ParticleState   -- ^ initial state
           -> Time -> PosVec  -- ^ position function
positionPS method fs st t
    = let states = statesPS method fs st
          dt = time (states !! 1) - time (states !! 0)
          numSteps = abs $ round (t / dt)
          st1 = solver method (newtonSecondPS fs) st !! numSteps
      in posVec st1

class HasTime s where
    timeOf :: s -> Time

instance HasTime ParticleState where
    timeOf = time

constantForce :: Vec -> OneBodyForce
constantForce f = undefined f

moonSurfaceGravity :: OneBodyForce
moonSurfaceGravity = undefined

earthGravity :: OneBodyForce
earthGravity = undefined

tvyPair :: ParticleState -> (R,R)
tvyPair st = undefined st

tvyPairs :: [ParticleState] -> [(R,R)]
tvyPairs sts = undefined sts

tle1yr :: ParticleState -> Bool
tle1yr st = undefined st

stateFunc :: [ParticleState]
          -> Time -> ParticleState
stateFunc sts t
    = let t0 = undefined sts
          t1 = undefined sts
          dt = undefined t0 t1
          numSteps = undefined t dt
      in undefined sts numSteps

airResAtAltitude :: R  -- ^ drag coefficient
                 -> R  -- ^ air density at sea level
                 -> R  -- ^ cross-sectional area of object
                 -> OneBodyForce
airResAtAltitude drag rho0 area (ParticleState _m _q _t r v)
    = undefined drag rho0 area r v

projectileRangeComparison :: R -> R -> (R,R,R)
projectileRangeComparison v0 thetaDeg
    = let vx0 = v0 * cos (thetaDeg / 180 * pi)
          vz0 = v0 * sin (thetaDeg / 180 * pi)
          drag = 1
          ballRadius = 0.05    -- meters
          area = pi * ballRadius**2
          airDensity  =     1.225  -- kg/m^3 @ sea level
          leadDensity = 11342      -- kg/m^3
          m = leadDensity * 4 * pi * ballRadius**3 / 3
          stateInitial = undefined m vx0 vz0
          aboveSeaLevel :: ParticleState -> Bool
          aboveSeaLevel st = zComp (posVec st) >= 0
          range :: [ParticleState] -> R
          range = xComp . posVec . last . takeWhile aboveSeaLevel
          method = rungeKutta4 0.01
          forcesNoAir
              = [earthSurfaceGravity]
          forcesConstAir
              = [earthSurfaceGravity, airResistance    drag airDensity area]
          forcesVarAir
              = [earthSurfaceGravity, airResAtAltitude drag airDensity area]
          rangeNoAir    = range $ statesPS method forcesNoAir    stateInitial
          rangeConstAir = range $ statesPS method forcesConstAir stateInitial
          rangeVarAir   = range $ statesPS method forcesVarAir   stateInitial
      in undefined rangeNoAir rangeConstAir rangeVarAir

halleyUpdate :: TimeStep
             -> ParticleState -> ParticleState
halleyUpdate dt
    = updatePS (eulerCromerPS dt) [sunGravity]

halleyInitial :: ParticleState
halleyInitial = ParticleState { mass     = 2.2e14            -- kg
                              , charge   = 0
                              , time     = 0
                              , posVec   = 8.766e10 *^ iHat  -- m
                              , velocity = 54569 *^ jHat }   -- m/s

baseballForces :: [OneBodyForce]
baseballForces
    = let area = pi * (0.074 / 2) ** 2
      in [earthSurfaceGravity
         ,airResistance 0.3 1.225 area]

baseballTrajectory :: R  -- time step
                   -> R  -- initial speed
                   -> R  -- launch angle in degrees
                   -> [(R,R)]  -- (y,z) pairs
baseballTrajectory dt v0 thetaDeg
    = let thetaRad = thetaDeg * pi / 180
          vy0 = v0 * cos thetaRad
          vz0 = v0 * sin thetaRad
          initialState
              = ParticleState { mass     = 0.145
                              , charge   = 0
                              , time     = 0
                              , posVec   = zeroV
                              , velocity = vec 0 vy0 vz0 }
      in trajectory $ zGE0 $
         statesPS (eulerCromerPS dt) baseballForces initialState

zGE0 :: [ParticleState] -> [ParticleState]
zGE0 = takeWhile (\(ParticleState _ _ _ r _) -> zComp r >= 0)

trajectory :: [ParticleState] -> [(R,R)]
trajectory sts = [(yComp r,zComp r) | (ParticleState _ _ _ r _) <- sts]

baseballRange :: R  -- time step
              -> R  -- initial speed
              -> R  -- launch angle in degrees
              -> R  -- range
baseballRange dt v0 thetaDeg
    = let (y,_) = last $ baseballTrajectory dt v0 thetaDeg
      in y

bestAngle :: (R,R)
bestAngle
    = maximum [(baseballRange 0.01 45 thetaDeg,thetaDeg) |
               thetaDeg <- [30,31..60]]

projectileUpdate :: TimeStep
                 -> ParticleState  -- old state
                 -> ParticleState  -- new state
projectileUpdate dt
    = updatePS (eulerCromerPS dt) baseballForces

projectileInitial :: [String] -> ParticleState
projectileInitial []        = error "Please supply initial speed and angle."
projectileInitial [_]       = error "Please supply initial speed and angle."
projectileInitial (_:_:_:_)
    = error "First argument is speed.  Second is angle in degrees."
projectileInitial (arg1:arg2:_)
    = let v0       = read arg1 :: R       -- initial speed, m/s
          angleDeg = read arg2 :: R       -- initial angle, degrees
          theta    = angleDeg * pi / 180  -- in radians
      in defaultParticleState
             { mass     = 0.145  -- kg
             , posVec   = zeroV
             , velocity = vec 0 (v0 * cos theta) (v0 * sin theta)
             }

protonUpdate :: TimeStep -> ParticleState -> ParticleState
protonUpdate dt
    = updatePS (rungeKutta4 dt) [uniformLorentzForce zeroV (3e-8 *^ kHat)]

protonInitial :: ParticleState
protonInitial
    = defaultParticleState { mass     = 1.672621898e-27  -- kg
                           , charge   = 1.602176621e-19  -- C
                           , posVec   = zeroV
                           , velocity = 1.5*^jHat ^+^ 0.3*^kHat  -- m/s
                           }

apR :: R
apR = 0.04  -- meters

wallForce :: OneBodyForce
wallForce ps
    = let m = mass ps
          r = posVec ps
          x = xComp r
          y = yComp r
          z = zComp r
          v = velocity ps
          timeStep = 5e-4 / 60
      in if y >= 1 && y < 1.1 && sqrt (x**2 + z**2) > apR
         then (-m) *^ (v ^/ timeStep)
         else zeroV

energy :: ParticleState -> R
energy ps = undefined ps

firstOrbit :: ParticleState -> Bool
firstOrbit st
    = let year = 365.25 * 24 * 60 * 60
      in time st < 50 * year || yComp (posVec st) <= 0

-- | Given a list of forces, return a differential equation
--   based on the theory of special relativity.
relativityPS :: [OneBodyForce]
             -> ParticleState -> DParticleState  -- a differential equation
relativityPS fs st
    = let fNet = sumV [f st | f <- fs]
          c = 299792458  -- m / s
          m = mass st
          v = velocity st
          u = v ^/ c
          acc = sqrt (1 - u <.> u) *^ (fNet ^-^ (fNet <.> u) *^ u) ^/ m
      in DParticleState { dmdt = 0    -- dm/dt
                        , dqdt = 0    -- dq/dt
                        , dtdt = 1    -- dt/dt
                        , drdt = v    -- dr/dt
                        , dvdt = acc  -- dv/vt
                        }

twoProtUpdate :: TimeStep
              -> (ParticleState,ParticleState)
              -> (ParticleState,ParticleState)
twoProtUpdate dt (stN,stR)
    = let forces = [uniformLorentzForce zeroV kHat]
      in (rungeKutta4 dt (newtonSecondPS forces) stN
         ,rungeKutta4 dt (relativityPS   forces) stR)

twoProtInitial :: (ParticleState,ParticleState)
twoProtInitial
    = let c = 299792458  -- m/s
          pInit = protonInitial { velocity = 0.8 *^ c *^ jHat }
      in (pInit,pInit)

relativityPS' :: R  -- c
              -> [OneBodyForce]
              -> ParticleState -> DParticleState
relativityPS' c fs st = undefined c fs st
