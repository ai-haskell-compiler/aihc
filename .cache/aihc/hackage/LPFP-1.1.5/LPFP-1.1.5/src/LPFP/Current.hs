{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFP.Current
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 26 of the book Learn Physics with Functional Programming
-}

module LPFP.Current where

import LPFP.SimpleVec
    ( R, Vec, sumV, (><), (*^) )
import LPFP.CoordinateSystems
    ( VectorField, rVF, cyl, phiHat )
import LPFP.Geometry
    ( Curve(..), Surface(..), Volume(..) )
import LPFP.ElectricField
    ( CurveApprox, curveSample, surfaceSample, volumeSample
    , vectorSurfaceIntegral, vectorVolumeIntegral )

type Current = R

data CurrentDistribution 
  = LineCurrent    Current     Curve
  | SurfaceCurrent VectorField Surface
  | VolumeCurrent  VectorField Volume
  | MultipleCurrents [CurrentDistribution]

circularCurrentLoop :: R  -- radius
                    -> R  -- current
                    -> CurrentDistribution
circularCurrentLoop radius i
    = LineCurrent i (Curve (\phi -> cyl radius phi 0) 0 (2*pi))

wireSolenoid :: R  -- radius
             -> R  -- length
             -> R  -- turns/length
             -> R  -- current
             -> CurrentDistribution
wireSolenoid radius len n i
    = LineCurrent i (Curve (\phi -> cyl radius phi (phi/(2*pi*n)))
                               (-pi*n*len) (pi*n*len))

sheetSolenoid :: R  -- radius
              -> R  -- length
              -> R  -- turns/length
              -> R  -- current
              -> CurrentDistribution
sheetSolenoid radius len n i
    = SurfaceCurrent (\r -> (n*i) *^ phiHat r)
      (Surface (\(phi,z) -> cyl radius phi z)
       0 (2*pi) (const $ -len/2) (const $ len/2))

wireToroid :: R  -- small radius
           -> R  -- big radius
           -> R  -- number of turns
           -> R  -- current
           -> CurrentDistribution
wireToroid smallR bigR n i
    = let alpha phi = n * phi
          curve phi = cyl (bigR + smallR * cos (alpha phi)) phi
                      (smallR * sin (alpha phi))
      in LineCurrent i (Curve curve 0 (2*pi))

crossedLineIntegral :: CurveApprox -> VectorField -> Curve -> Vec
crossedLineIntegral approx vF c
    = sumV [vF r' >< dl' | (r',dl') <- approx c]

magneticDipoleMoment :: CurrentDistribution -> Vec
magneticDipoleMoment (LineCurrent    i c)
    = crossedLineIntegral   (curveSample  1000) (\r -> 0.5 *^ i *^ rVF r) c
magneticDipoleMoment (SurfaceCurrent k s)
    = vectorSurfaceIntegral (surfaceSample 200) (\r -> 0.5 *^ (rVF r >< k r)) s
magneticDipoleMoment (VolumeCurrent  j v)
    = vectorVolumeIntegral  (volumeSample   50) (\r -> 0.5 *^ (rVF r >< j r)) v
magneticDipoleMoment (MultipleCurrents ds    )
    = sumV [magneticDipoleMoment d | d <- ds]

helmholtzCoil :: R  -- radius
              -> R  -- current
              -> CurrentDistribution
helmholtzCoil radius i = undefined radius i

longStraightWire :: R  -- wire length
                 -> R  -- current
                 -> CurrentDistribution
longStraightWire len i = undefined len i

torus :: R -> R -> Surface
torus smallR bigR
    = Surface (\(phi,alpha) -> cyl (bigR + smallR * cos alpha) phi
                               (smallR * sin alpha))
      0 (2*pi) (const 0) (const $ 2*pi)

totalCurrent :: VectorField  -- volume current density
             -> Surface
             -> Current      -- total current through surface
totalCurrent j s = undefined j s
