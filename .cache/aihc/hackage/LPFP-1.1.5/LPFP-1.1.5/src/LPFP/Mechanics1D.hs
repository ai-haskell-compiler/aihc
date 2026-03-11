{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{- | 
Module      :  LPFP.Mechanics1D
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 15 of the book Learn Physics with Functional Programming
-}

module LPFP.Mechanics1D where

import Graphics.Gnuplot.Simple

import LPFP.Newton2 ( fAir )

import LPFP.SimpleVec ( R )

type Time     = R
type TimeStep = R
type Mass     = R
type Position = R
type Velocity = R
type Force    = R

type State1D = (Time,Position,Velocity)

newtonSecond1D :: Mass
               -> [State1D -> Force]  -- force funcs
               -> State1D             -- current state
               -> (R,R,R)             -- deriv of state
newtonSecond1D m fs (t,x0,v0)
    = let fNet = sum [f (t,x0,v0) | f <- fs]
          acc = fNet / m
      in (1,v0,acc)

euler1D :: R                     -- time step dt
        -> (State1D -> (R,R,R))  -- differential equation
        -> State1D -> State1D    -- state-update function
euler1D dt deriv (t0,x0,v0)
    = let (_, _, dvdt) = deriv (t0,x0,v0)
          t1 = t0 + dt
          x1 = x0 + v0 * dt
          v1 = v0 + dvdt * dt
      in (t1,x1,v1)

updateTXV :: R                   -- time interval dt
          -> Mass
          -> [State1D -> Force]  -- list of force funcs
          -> State1D -> State1D  -- state-update function
updateTXV dt m fs = euler1D dt (newtonSecond1D m fs)

statesTXV :: R                   -- time step
          -> Mass
          -> State1D             -- initial state
          -> [State1D -> Force]  -- list of force funcs
          -> [State1D]           -- infinite list of states
statesTXV dt m txv0 fs = iterate (updateTXV dt m fs) txv0

-- assume that dt is the same between adjacent pairs
velocity1D :: [State1D]           -- infinite list
           -> Time -> Velocity    -- velocity function
velocity1D sts t
    = let (t0,_,_) = sts !! 0
          (t1,_,_) = sts !! 1
          dt = t1 - t0
          numSteps = abs $ round (t / dt)
          (_,_,v0) = sts !! numSteps
      in v0

velocityFtxv :: R                   -- time step
             -> Mass
             -> State1D             -- initial state
             -> [State1D -> Force]  -- list of force funcs
             -> Time -> Velocity    -- velocity function
velocityFtxv dt m txv0 fs = velocity1D (statesTXV dt m txv0 fs)

-- assume that dt is the same between adjacent pairs
position1D :: [State1D]           -- infinite list
           -> Time -> Position    -- position function
position1D sts t
    = let (t0,_,_) = sts !! 0
          (t1,_,_) = sts !! 1
          dt = t1 - t0
          numSteps = abs $ round (t / dt)
          (_,x0,_) = sts !! numSteps
      in x0

positionFtxv :: R                   -- time step
             -> Mass
             -> State1D             -- initial state
             -> [State1D -> Force]  -- list of force funcs
             -> Time -> Position    -- position function
positionFtxv dt m txv0 fs = position1D (statesTXV dt m txv0 fs)

springForce :: R -> State1D -> Force
springForce k (_,x0,_) = -k * x0

dampedHOForces :: [State1D -> Force]
dampedHOForces = [springForce 0.8
                 ,\(_,_,v0) -> fAir 2 1.225 (pi * 0.02**2) v0
                 ,\_ -> -0.0027 * 9.80665
                 ]

dampedHOStates :: [State1D]
dampedHOStates = statesTXV 0.001 0.0027 (0.0,0.1,0.0) dampedHOForces

dampedHOGraph :: IO ()
dampedHOGraph
    = plotPath [Title "Ping Pong Ball on a Slinky"
               ,XLabel "Time (s)"
               ,YLabel "Position (m)"
               ,PNG "dho.png"
               ,Key Nothing
               ] [(t,x) | (t,x,_) <- take 3000 dampedHOStates]

pingpongPosition :: Time -> Position
pingpongPosition = positionFtxv 0.001 0.0027 (0,0.1,0) dampedHOForces

dampedHOGraph2 :: IO ()
dampedHOGraph2
    = plotFunc [Title "Ping Pong Ball on a Slinky"
               ,XLabel "Time (s)"
               ,YLabel "Position (m)"
               ,Key Nothing
               ] [0,0.01..3] pingpongPosition

pingpongVelocity :: Time -> Velocity
pingpongVelocity = velocityFtxv 0.001 0.0027 (0,0.1,0) dampedHOForces

dampedHOGraph3 :: IO ()
dampedHOGraph3
    = plotFunc [Title "Ping Pong Ball on a Slinky"
               ,XLabel "Time (s)"
               ,YLabel "Velocity (m/s)"
               ,PNG "dho2.png"
               ,Key Nothing
               ] [0,0.01..3] pingpongVelocity

eulerCromer1D :: R                     -- time step dt
              -> (State1D -> (R,R,R))  -- differential equation
              -> State1D -> State1D    -- state-update function
eulerCromer1D dt deriv (t0,x0,v0)
    = let (_, _, dvdt) = deriv (t0,x0,v0)
          t1 = t0 + dt
          x1 = x0 + v1 * dt
          v1 = v0 + dvdt * dt
      in (t1,x1,v1)

updateTXVEC :: R                   -- time interval dt
            -> Mass
            -> [State1D -> Force]  -- list of force funcs
            -> State1D -> State1D  -- state-update function
updateTXVEC dt m fs = eulerCromer1D dt (newtonSecond1D m fs)

-- | An update function takes a state as input and returns an updated state as output.
type UpdateFunction s = s -> s

-- | A differential equation takes a state as input and returns as output the rate at which
--   the state is changing.
type DifferentialEquation s ds = s -> ds

-- | A numerical method turns a differential equation into a state-update function.
type NumericalMethod s ds = DifferentialEquation s ds -> UpdateFunction s

-- | Given a numerical method, a differential equation, and an initial state,
--   return a list of states.
solver :: NumericalMethod s ds -> DifferentialEquation s ds -> s -> [s]
solver method = iterate . method

-- | A real vector space allows vector addition and scalar multiplication by reals.
class RealVectorSpace ds where
      (+++) :: ds -> ds -> ds
      scale :: R -> ds -> ds

-- | A triple of real numbers is a real vector space.
instance RealVectorSpace (R,R,R) where
    (dtdt0, dxdt0, dvdt0) +++ (dtdt1, dxdt1, dvdt1)
        = (dtdt0 + dtdt1, dxdt0 + dxdt1, dvdt0 + dvdt1)
    scale w (dtdt0, dxdt0, dvdt0) = (w * dtdt0, w * dxdt0, w * dvdt0)

-- | A type class that expresses a relationship between a state space
--   and a time-derivative-state space.
class RealVectorSpace ds => Diff s ds where
    shift :: R -> ds -> s -> s

-- | A triple of real numbers can serve as the time derivative of a 'State1D'.
instance Diff State1D (R,R,R) where
    shift dt (dtdt,dxdt,dvdt) (t,x,v)
        = (t + dtdt * dt, x + dxdt * dt, v + dvdt * dt)

-- | Given a step size, return the numerical method that uses the Euler
--   method with that step size.
euler :: Diff s ds => R -> (s -> ds) -> s -> s
euler dt deriv st0 = shift dt (deriv st0) st0

-- | Given a step size, return the numerical method that uses the 4th order Runge Kutta
--   method with that step size.
rungeKutta4 :: Diff s ds => R -> (s -> ds) -> s -> s
rungeKutta4 dt deriv st0
    = let m0 = deriv                  st0
          m1 = deriv (shift (dt/2) m0 st0)
          m2 = deriv (shift (dt/2) m1 st0)
          m3 = deriv (shift  dt    m2 st0)
      in shift (dt/6) (m0 +++ m1 +++ m1 +++ m2 +++ m2 +++ m3) st0

exponential :: DifferentialEquation (R,R,R) (R,R,R)
exponential (_,x0,v0) = (1,v0,x0)

update2 :: (R,R,R)  -- starting state
        -> (R,R,R)  -- ending state
update2 = undefined

earthGravity :: Mass -> State1D -> Force
earthGravity m _ = let g = 9.80665
                   in -m * g

type MState = (Time,Mass,Position,Velocity)

earthGravity2 :: MState -> Force
earthGravity2 (_,m,_,_) = let g = 9.80665
                          in -m * g

positionFtxv2 :: R                  -- time step
              -> MState             -- initial state
              -> [MState -> Force]  -- list of force funcs
              -> Time -> Position   -- position function
positionFtxv2 = undefined

statesTXV2 :: R                 -- time step
          -> MState             -- initial state
          -> [MState -> Force]  -- list of force funcs
          -> [MState]           -- infinite list of states
statesTXV2 = undefined

updateTXV2 :: R                  -- dt for stepping
           -> [MState -> Force]  -- list of force funcs
           -> MState             -- current state
           -> MState             -- new state
updateTXV2 = undefined

instance RealVectorSpace (R,R) where
    (dtdt0, dvdt0) +++ (dtdt1, dvdt1) = (dtdt0 + dtdt1, dvdt0 + dvdt1)
    scale w (dtdt0, dvdt0) = (w * dtdt0, w * dvdt0)

instance Diff (Time,Velocity) (R,R) where
    shift dt (dtdt,dvdt) (t,v)
        = (t + dtdt * dt, v + dvdt * dt)

updateTV' :: R                           -- dt for stepping
          -> Mass
          -> [(Time,Velocity) -> Force]  -- list of force funcs
          -> (Time,Velocity)             -- current state
          -> (Time,Velocity)             -- new state
updateTV' = undefined

forces :: R -> [State1D -> R]
forces mu = [\(_t,x,_v) -> undefined x
            ,\(_t,x, v) -> undefined mu x v]

vdp :: R -> [(R,R)]
vdp mu = map (\(_,x,v) -> (x,v)) $ take 10000 $
         solver (rungeKutta4 0.01) (newtonSecond1D 1 $ forces mu) (0,2,0)

vdpPhasePlanePlot :: IO ()
vdpPhasePlanePlot = plotPaths [Title "Van der Pol oscillator"
                              ,XLabel "x"
                              ,YLabel "v"
                              ,PNG "VanderPol.png"
                              ,Key Nothing] (undefined :: [[(R,R)]])
