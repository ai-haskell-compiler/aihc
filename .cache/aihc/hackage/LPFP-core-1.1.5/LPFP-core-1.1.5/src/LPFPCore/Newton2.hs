{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFPCore.Newton2
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 14 of the book Learn Physics with Functional Programming
-}

module LPFPCore.Newton2 where

velocityCF :: Mass
           -> Velocity          -- initial velocity
           -> [Force]           -- list of forces
           -> Time -> Velocity  -- velocity function

type R = Double

type Mass     = R
type Time     = R
type Position = R
type Velocity = R
type Force    = R

velocityCF m v0 fs
    = let fNet = sum fs       -- net force
          a0   = fNet / m     -- Newton's second law
          v t  = v0 + a0 * t  -- constant acceleration eqn
      in v

positionCF :: Mass
           -> Position          -- initial position
           -> Velocity          -- initial velocity
           -> [Force]           -- list of forces
           -> Time -> Position  -- position function
positionCF m x0 v0 fs
    = let fNet = sum fs
          a0   = fNet / m
          x t  = x0 + v0 * t + a0*t**2 / 2
      in x

velocityFt :: R                 -- dt for integral
           -> Mass
           -> Velocity          -- initial velocity
           -> [Time -> Force]   -- list of force functions
           -> Time -> Velocity  -- velocity function
velocityFt dt m v0 fs
    = let fNet t = sum [f t | f <- fs]
          a t = fNet t / m
      in antiDerivative dt v0 a

-- | Given a step size, a y-intercept, and a function, return a function
--   with the given y-intercept whose
--   derivative is the given function.
antiDerivative :: R -> R -> (R -> R) -> (R -> R)
antiDerivative dt v0 a t = v0 + integral dt a 0 t

-- | Given a step size, a function, a lower limit, and an upper limit, return
--   the definite integral of the function.
integral :: R -> (R -> R) -> R -> R -> R
integral dt f a b
    = sum [f t * dt | t <- [a+dt/2, a+3*dt/2 .. b - dt/2]]

positionFt :: R                 -- dt for integral
           -> Mass
           -> Position          -- initial position
           -> Velocity          -- initial velocity
           -> [Time -> Force]   -- list of force functions
           -> Time -> Position  -- position function
positionFt dt m x0 v0 fs
    = antiDerivative dt x0 (velocityFt dt m v0 fs)

pedalCoast :: Time -> Force
pedalCoast t
    = let tCycle = 20
          nComplete :: Int
          nComplete = truncate (t / tCycle)
          remainder = t - fromIntegral nComplete * tCycle
      in if remainder < 10
         then 10
         else 0

fAir :: R  -- drag coefficient
     -> R  -- air density
     -> R  -- cross-sectional area of object
     -> Velocity
     -> Force
fAir drag rho area v = -drag * rho * area * abs v * v / 2

newtonSecondV :: Mass
              -> [Velocity -> Force]  -- list of force functions
              -> Velocity             -- current velocity
              -> R                    -- derivative of velocity
newtonSecondV m fs v0 = sum [f v0 | f <- fs] / m

updateVelocity :: R                    -- time interval dt
               -> Mass
               -> [Velocity -> Force]  -- list of force functions
               -> Velocity             -- current velocity
               -> Velocity             -- new velocity
updateVelocity dt m fs v0
    = v0 + (newtonSecondV m fs v0) * dt

velocityFv :: R                    -- time step
           -> Mass
           -> Velocity             -- initial velocity v(0)
           -> [Velocity -> Force]  -- list of force functions
           -> Time -> Velocity     -- velocity function
velocityFv dt m v0 fs t
    = let numSteps = abs $ round (t / dt)
      in iterate (updateVelocity dt m fs) v0 !! numSteps

bikeVelocity :: Time -> Velocity
bikeVelocity = velocityFv 1 70 0 [const 100,fAir 2 1.225 0.6]

newtonSecondTV :: Mass
               -> [(Time,Velocity) -> Force]  -- force funcs
               -> (Time,Velocity)             -- current state
               -> (R,R)                       -- deriv of state
newtonSecondTV m fs (t,v0)
    = let fNet = sum [f (t,v0) | f <- fs]
          acc = fNet / m
      in (1,acc)

updateTV :: R                           -- time interval dt
         -> Mass
         -> [(Time,Velocity) -> Force]  -- list of force funcs
         -> (Time,Velocity)             -- current state
         -> (Time,Velocity)             -- new state
updateTV dt m fs (t,v0)
    = let (dtdt, dvdt) = newtonSecondTV m fs (t,v0)
      in (t  + dtdt * dt
         ,v0 + dvdt * dt)

statesTV :: R                           -- time step
         -> Mass
         -> (Time,Velocity)             -- initial state
         -> [(Time,Velocity) -> Force]  -- list of force funcs
         -> [(Time,Velocity)]           -- infinite list of states
statesTV dt m tv0 fs
    = iterate (updateTV dt m fs) tv0

velocityFtv :: R                           -- time step
            -> Mass
            -> (Time,Velocity)             -- initial state
            -> [(Time,Velocity) -> Force]  -- list of force funcs
            -> Time -> Velocity            -- velocity function
velocityFtv dt m tv0 fs t
    = let numSteps = abs $ round (t / dt)
      in snd $ statesTV dt m tv0 fs !! numSteps

pedalCoastAir :: [(Time,Velocity)]
pedalCoastAir = statesTV 0.1 20 (0,0)
                [\(t,_) -> pedalCoast t
                ,\(_,v) -> fAir 2 1.225 0.5 v]

pedalCoastAir2 :: Time -> Velocity
pedalCoastAir2 = velocityFtv 0.1 20 (0,0)
                 [\( t,_v) -> pedalCoast t
                 ,\(_t, v) -> fAir 2 1.225 0.5 v]

velocityCF' :: Mass
            -> Velocity          -- initial velocity
            -> [Force]           -- list of forces
            -> Time -> Velocity  -- velocity function
velocityCF' m v0 fs t = undefined m v0 fs t

sumF :: [R -> R] -> R -> R
sumF = undefined

positionFv :: R                    -- time step
           -> Mass
           -> Position             -- initial position x(0)
           -> Velocity             -- initial velocity v(0)
           -> [Velocity -> Force]  -- list of force functions
           -> Time -> Position     -- position function
positionFv = undefined

positionFtv :: R                    -- time step
            -> Mass
            -> Position             -- initial position x(0)
            -> Velocity             -- initial velocity v(0)
            -> [(Time,Velocity) -> Force]  -- force functions
            -> Time -> Position     -- position function
positionFtv = undefined

updateExample :: (Time,Velocity)  -- starting state
              -> (Time,Velocity)  -- ending state
updateExample = undefined
