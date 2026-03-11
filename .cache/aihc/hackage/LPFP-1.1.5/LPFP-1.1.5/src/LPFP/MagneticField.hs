{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFP.MagneticField
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 27 of the book Learn Physics with Functional Programming
-}

module LPFP.MagneticField where

import LPFP.SimpleVec ( Vec(..), R
                 , (^-^), (*^), (^/), (<.>), (><)
                 , magnitude, kHat, zComp )
import LPFP.CoordinateSystems
    ( VectorField
    , rVF, displacement, addVectorFields, cart, vfGrad )
import LPFP.Geometry ( Curve(..), Surface(..), Volume(..) )
import LPFP.ElectricField
    ( curveSample, surfaceSample, volumeSample
    , vectorSurfaceIntegral, vectorVolumeIntegral, mu0 )
import LPFP.Current
    ( Current, CurrentDistribution(..)
    , wireSolenoid, wireToroid, crossedLineIntegral, circularCurrentLoop )

bFieldFromLineCurrent
    :: Current      -- current (in Amps)
    -> Curve
    -> VectorField  -- magnetic field (in Tesla)
bFieldFromLineCurrent i c r
    = let coeff = -mu0 * i / (4 * pi)  -- SI units
          integrand r' = d ^/ magnitude d ** 3
              where d = displacement r' r
      in coeff *^ crossedLineIntegral (curveSample 1000) integrand c

bField :: CurrentDistribution -> VectorField
bField (LineCurrent    i  c) = bFieldFromLineCurrent    i  c
bField (SurfaceCurrent kC s) = bFieldFromSurfaceCurrent kC s
bField (VolumeCurrent  j  v) = bFieldFromVolumeCurrent  j  v
bField (MultipleCurrents cds) = addVectorFields $ map bField cds

circleB :: VectorField  -- magnetic field
circleB = bField $ circularCurrentLoop 0.25 10

bFieldPicLoop :: IO ()
bFieldPicLoop
    = vfGrad (**0.2) (\(y,z) -> cart 0 y z) (\v -> (yComp v, zComp v))
      "bFieldPicLoop.png" 20 circleB

bFieldIdealDipole :: Vec          -- magnetic dipole moment
                  -> VectorField  -- magnetic field
bFieldIdealDipole m r
    = let coeff = mu0 / (4 * pi)    -- SI units
          rMag = magnitude (rVF r)
          rUnit = rVF r ^/ rMag
      in coeff *^ (1 / rMag**3) *^ (3 *^ (m <.> rUnit) *^ rUnit ^-^ m)

bFieldPicIdealDipole :: IO ()
bFieldPicIdealDipole
    = vfGrad (**0.2) (\(y,z) -> cart 0 y z) (\v -> (yComp v, zComp v))
      "bFieldPicIdealDipole.png" 20 (bFieldIdealDipole kHat)

bFieldPicSolenoid10 :: IO ()
bFieldPicSolenoid10 = vfGrad (**0.2) (\(y,z) -> cart 0 (0.02*y) (0.02*z))
                     (\v -> (yComp v, zComp v)) "bFieldPicSolenoid10.png" 20
                     (bField $ wireSolenoid 0.01 0.1 100 10)

bFieldPicSolenoid100 :: IO ()
bFieldPicSolenoid100 = vfGrad (**0.2) (\(y,z) -> cart 0 (0.02*y) (0.02*z))
                     (\v -> (yComp v, zComp v)) "bFieldPicSolenoid100.png" 20
                     (bField $ wireSolenoid 0.01 0.1 1000 10)

bFieldWireToroid :: VectorField
bFieldWireToroid = bField (wireToroid 0.3 1 50 10)

bFieldPicWireToroid :: IO ()
bFieldPicWireToroid
    = vfGrad (**0.2) (\(x,y) -> cart (1.5*x) (1.5*y) 0)
      (\v -> (xComp v, yComp v)) "bFieldPicWireToroid.png" 20 bFieldWireToroid

bFieldFromSurfaceCurrent
    :: VectorField  -- surface current density
    -> Surface      -- surface across which current flows
    -> VectorField  -- magnetic field (in T)
bFieldFromSurfaceCurrent kCurrent s r
    = let coeff = mu0 / (4 * pi)  -- SI units
          integrand r' = (kCurrent r' >< d) ^/ magnitude d ** 3
              where d = displacement r' r
      in coeff *^ vectorSurfaceIntegral (surfaceSample 200) integrand s

bFieldFromVolumeCurrent
    :: VectorField  -- volume current density
    -> Volume       -- volume throughout which current flows
    -> VectorField  -- magnetic field (in T)
bFieldFromVolumeCurrent j vol r
    = let coeff = mu0 / (4 * pi)  -- SI units
          integrand r' = (j r' >< d) ^/ magnitude d ** 3
              where d = displacement r' r
      in coeff *^ vectorVolumeIntegral (volumeSample 50) integrand vol

magneticFluxFromField :: VectorField -> Surface -> R
magneticFluxFromField = undefined

magneticFluxFromCurrent :: CurrentDistribution -> Surface -> R
magneticFluxFromCurrent = undefined

visLoop :: IO ()
visLoop = undefined

bFieldPicSolenoid1000 :: IO ()
bFieldPicSolenoid1000
    = vfGrad (**0.2) (\(y,z) -> cart 0 (0.02*y) (0.02*z))
             (\v -> (yComp v, zComp v)) "bFieldPicSolenoid1000.png" 20
             (bField $ wireSolenoid 0.01 0.1 10000 10)
