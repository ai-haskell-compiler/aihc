{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFP.MOExamples
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 20 of the book Learn Physics with Functional Programming
-}

module LPFP.MOExamples where

import LPFP.SimpleVec
    ( R, Vec, (^+^), (^-^), (*^), vec, zeroV, magnitude
    , sumV, iHat, jHat, kHat, xComp, yComp, zComp )
import LPFP.Mechanics1D ( TimeStep, NumericalMethod, euler, rungeKutta4 )
import LPFP.Mechanics3D
    ( ParticleState(..), HasTime(..), defaultParticleState
    , earthSurfaceGravity, customLabel, orient, disk )
import LPFP.MultipleObjects
    ( MultiParticleState(..), DMultiParticleState, Force(..), TwoBodyForce
    , newtonSecondMPS, updateMPS, statesMPS, eulerCromerMPS
    , linearSpring, fixedLinearSpring, billiardForce )
import Graphics.Gnuplot.Simple
import qualified Graphics.Gloss as G
import qualified Vis as V

twoSpringsForces :: [Force]
twoSpringsForces
    = [ExternalForce 0 (fixedLinearSpring 100 0.5 zeroV)
      ,InternalForce 0 1 (linearSpring 100 0.5)
      ,ExternalForce 0 earthSurfaceGravity
      ,ExternalForce 1 earthSurfaceGravity
      ]

twoSpringsInitial :: MultiParticleState
twoSpringsInitial
    = MPS [defaultParticleState
           { mass   = 2
           , posVec = 0.4 *^ jHat ^-^ 0.3 *^ kHat }
          ,defaultParticleState
           { mass   = 3
           , posVec = 0.4 *^ jHat ^-^ 0.8 *^ kHat }
          ]

twoSpringsUpdate :: TimeStep
                 -> MultiParticleState  -- old state
                 -> MultiParticleState  -- new state
twoSpringsUpdate dt = updateMPS (eulerCromerMPS dt) twoSpringsForces

kineticEnergy :: ParticleState -> R
kineticEnergy st = let m = mass st
                       v = magnitude (velocity st)
                   in (1/2) * m * v**2

systemKE :: MultiParticleState -> R
systemKE (MPS sts) = sum [kineticEnergy st | st <- sts]

linearSpringPE :: R              -- spring constant
               -> R              -- equilibrium length
               -> ParticleState  -- state of particle at one end of spring
               -> ParticleState  -- state of particle at other end of spring
               -> R              -- potential energy of the spring
linearSpringPE k re st1 st2
    = let r1 = posVec st1
          r2 = posVec st2
          r21 = r2 ^-^ r1
          r21mag = magnitude r21
      in k * (r21mag - re)**2 / 2

-- z direction is toward the sky
-- assumes SI units
earthSurfaceGravityPE :: ParticleState -> R
earthSurfaceGravityPE st
    = let g = 9.80665  -- m/s^2
          m = mass st
          z = zComp (posVec st)
      in m * g * z

twoSpringsPE :: MultiParticleState -> R
twoSpringsPE (MPS sts)
    = linearSpringPE 100 0.5 defaultParticleState (sts !! 0)
      + linearSpringPE 100 0.5 (sts !! 0) (sts !! 1)
      + earthSurfaceGravityPE (sts !! 0)
      + earthSurfaceGravityPE (sts !! 1)

twoSpringsME :: MultiParticleState -> R
twoSpringsME mpst = systemKE mpst + twoSpringsPE mpst

billiardForces :: R -> [Force]
billiardForces k = [InternalForce 0 1 (billiardForce k (2*ballRadius))]

ballRadius :: R
ballRadius = 0.03  -- 6cm diameter = 0.03m radius

billiardDiffEq :: R -> MultiParticleState -> DMultiParticleState
billiardDiffEq k = newtonSecondMPS $ billiardForces k

billiardUpdate
    :: (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState)
    -> R         -- k
    -> TimeStep  -- dt
    -> MultiParticleState -> MultiParticleState
billiardUpdate nMethod k dt = updateMPS (nMethod dt) (billiardForces k)

billiardEvolver
    :: (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState)
    -> R         -- k
    -> TimeStep  -- dt
    -> MultiParticleState -> [MultiParticleState]
billiardEvolver nMethod k dt = statesMPS (nMethod dt) (billiardForces k)

billiardInitial :: MultiParticleState
billiardInitial
    = let ballMass = 0.160  -- 160g
      in MPS [defaultParticleState { mass     = ballMass
                                   , posVec   = zeroV
                                   , velocity = 0.2 *^ iHat }
             ,defaultParticleState { mass     = ballMass
                                   , posVec   = iHat ^+^ 0.02 *^ jHat
                                   , velocity = zeroV }
             ]

billiardStates
    :: (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState)
    -> R         -- k
    -> TimeStep  -- dt
    -> [MultiParticleState]
billiardStates nMethod k dt
    = statesMPS (nMethod dt) (billiardForces k) billiardInitial

billiardStatesFinite
    :: (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState)
    -> R         -- k
    -> TimeStep  -- dt
    -> [MultiParticleState]
billiardStatesFinite nMethod k dt
    = takeWhile (\st -> timeOf st <= 10) (billiardStates nMethod k dt)

momentum :: ParticleState -> Vec
momentum st = let m = mass st
                  v = velocity st
              in m *^ v

systemP :: MultiParticleState -> Vec
systemP (MPS sts) = sumV [momentum st | st <- sts]

percentChangePMag :: [MultiParticleState] -> R
percentChangePMag mpsts
    = let p0 = systemP (mpsts !! 0)
          p1 = systemP (last mpsts)
      in 100 * magnitude (p1 ^-^ p0) / magnitude p0

sigFigs :: Int -> R -> Float
sigFigs n x = let expon :: Int
                  expon = floor (logBase 10 x) - n + 1
                  toInt :: R -> Int
                  toInt = round
              in (10^^expon *) $ fromIntegral $ toInt (10^^(-expon) * x)

data Justification = LJ | RJ deriving Show

data Table a = Table Justification [[a]]

instance Show a => Show (Table a) where
    show (Table j xss)
        = let pairWithLength x = let str = show x in (str, length str)
              pairss = map (map pairWithLength) xss
              maxLength = maximum (map maximum (map (map snd) pairss))
              showPair (str,len)
                  = case j of
                      LJ -> str ++ replicate (maxLength + 1 - len) ' '
                      RJ -> replicate (maxLength + 1 - len) ' ' ++ str
              showLine pairs = concatMap showPair pairs ++ "\n"
          in init $ concatMap showLine pairss

pTable :: (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState)
       -> [R]         -- ks
       -> [TimeStep]  -- dts
       -> Table Float
pTable nMethod ks dts
    = Table LJ [[sigFigs 2 $
                 percentChangePMag (billiardStatesFinite nMethod k dt)
                     | dt <- dts] | k <- ks]

pTableEu :: [R]         -- ks
         -> [TimeStep]  -- dts
         -> Table Float
pTableEu = pTable euler

systemKEWithTime :: IO ()
systemKEWithTime
    = let timeKEPairsEC
              = [(timeOf mpst, systemKE mpst)
                     | mpst <- billiardStatesFinite eulerCromerMPS 30 0.03]
          timeKEPairsRK4
              = [(timeOf mpst, systemKE mpst)
                     | mpst <- billiardStatesFinite rungeKutta4    30 0.03]
      in plotPaths [Key Nothing
                   ,Title "System Kinetic Energy versus Time"
                   ,XLabel "Time (s)"
                   ,YLabel "System Kinetic Energy (J)"
                   ,XRange (4,6)
                   ,PNG "SystemKE.png"
                   ,customLabel (4.1,0.0026) "dt = 0.03 s"
                   ,customLabel (4.1,0.0025) "k = 30 N/m"
                   ,customLabel (5.4,0.00329) "Euler-Cromer"
                   ,customLabel (5.4,0.00309) "Runge-Kutta 4"
                   ] [timeKEPairsEC,timeKEPairsRK4]

percentChangeKE :: [MultiParticleState] -> R
percentChangeKE mpsts
    = let ke0 = systemKE (mpsts !! 0)
          ke1 = systemKE (last mpsts)
      in 100 * (ke1 - ke0) / ke0

tenths :: R -> Float
tenths = let toInt :: R -> Int
             toInt = round
         in (/ 10) . fromIntegral . toInt . (* 10)

keTable
    :: (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState)
    -> [R]         -- ks
    -> [TimeStep]  -- dts
    -> Table Float
keTable nMethod ks dts
    = Table RJ [[tenths $
                 percentChangeKE (billiardStatesFinite nMethod k dt)
                     | dt <- dts] | k <- ks]

contactSteps :: [MultiParticleState] -> Int
contactSteps = length . takeWhile inContact . dropWhile (not . inContact)

inContact :: MultiParticleState -> Bool
inContact (MPS sts)
    = let r = magnitude $ posVec (sts !! 0) ^-^ posVec (sts !! 1)
      in r < 2 * ballRadius

contactTable
    :: (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState)
    -> [R]         -- ks
    -> [TimeStep]  -- dts
    -> Table Int
contactTable nMethod ks dts
    = Table RJ [[contactSteps (billiardStatesFinite nMethod k dt)
                     | dt <- dts] | k <- ks]

closest :: [MultiParticleState] -> R
closest = minimum . map separation

separation :: MultiParticleState -> R
separation (MPS sts)
    = magnitude $ posVec (sts !! 0) ^-^ posVec (sts !! 1)

closestTable
    :: (TimeStep -> NumericalMethod MultiParticleState DMultiParticleState)
    -> [R]         -- ks
    -> [TimeStep]  -- dts
    -> Table Float
closestTable nMethod ks dts
    = Table RJ [[tenths $ (100*) $
                 closest (billiardStatesFinite nMethod k dt)
                     | dt <- dts] | k <- ks]

billiardPicture :: MultiParticleState -> G.Picture
billiardPicture (MPS sts)
    = G.scale ppm ppm $ G.pictures [place st | st <- sts]
      where
        ppm = 300  -- pixels per meter
        place st = G.translate (xSt st) (ySt st) blueBall
        xSt = realToFrac . xComp . posVec
        ySt = realToFrac . yComp . posVec
        blueBall = G.Color G.blue (disk $ realToFrac ballRadius)

-- 64 masses (0 to 63)
-- There are 63 internal springs, 2 external springs
forcesString :: [Force]
forcesString
    = [ExternalForce  0 (fixedLinearSpring 5384 0 (vec    0 0 0))
      ,ExternalForce 63 (fixedLinearSpring 5384 0 (vec 0.65 0 0))] ++
      [InternalForce n (n+1) (linearSpring 5384 0) | n <- [0..62]]

stringUpdate :: TimeStep
             -> MultiParticleState  -- old state
             -> MultiParticleState  -- new state
stringUpdate dt = updateMPS (rungeKutta4 dt) forcesString

stringInitialOvertone :: Int -> MultiParticleState
stringInitialOvertone n
    = MPS [defaultParticleState
           { mass     = 0.8293e-3 * 0.65 / 64
           , posVec   = x *^ iHat ^+^ y *^ jHat
           , velocity = zeroV
           } | x <- [0.01, 0.02 .. 0.64],
           let y = 0.005 * sin (fromIntegral n * pi * x / 0.65)]

stringInitialPluck :: MultiParticleState
stringInitialPluck = MPS [defaultParticleState
             { mass     = 0.8293e-3 * 0.65 / 64
             , posVec   = x *^ iHat ^+^ y *^ jHat
             , velocity = zeroV
             } | x <- [0.01, 0.02 .. 0.64], let y = pluckEq x]
    where
      pluckEq :: R -> R
      pluckEq x
          | x <= 0.51  = 0.005 / (0.51 - 0.00) * (x - 0.00)
          | otherwise  = 0.005 / (0.51 - 0.65) * (x - 0.65)

mpsPos :: MultiParticleState -> IO ()
mpsPos = undefined

mpsVel :: MultiParticleState -> IO ()
mpsVel = undefined

dissipation :: R  -- damping constant
            -> R  -- threshold center separation
            -> TwoBodyForce
dissipation b re st1 st2
    = let r1 = posVec st1
          r2 = posVec st2
          v1 = velocity st1
          v2 = velocity st2
          r21 = r2 ^-^ r1
          v21 = v2 ^-^ v1
      in if magnitude r21 >= re
         then zeroV
         else (-b) *^ v21

animateGloss :: HasTime s => R  -- time-scale factor
             -> (s -> G.Picture)
             -> [s]
             -> IO ()
animateGloss tsFactor displayFunc mpsts
    = let dtp = timeOf (mpsts !! 1) - timeOf (mpsts !! 0)
          n tp = round (tp / dtp)
          picFromAnimTime :: Float -> G.Picture
          picFromAnimTime ta = displayFunc (mpsts !! n (tsFactor * realToFrac ta))
          displayMode = G.InWindow "My Window" (1000, 700) (10, 10)
      in G.animate displayMode G.black picFromAnimTime

animateVis :: HasTime s => R  -- time-scale factor
           -> (s -> V.VisObject R)
           -> [s]
           -> IO ()
animateVis tsFactor displayFunc mpsts
    = let dtp = timeOf (mpsts !! 1) - timeOf (mpsts !! 0)
          n tp = round (tp / dtp)
          picFromAnimTime :: Float -> V.VisObject R
          picFromAnimTime ta = displayFunc (mpsts !! n (tsFactor * realToFrac ta))
      in V.animate V.defaultOpts (orient . picFromAnimTime)
