{-# OPTIONS -Wall #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  LPFPCore
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from the book Learn Physics with Functional Programming
-}

module LPFPCore
    (
    -- * (Approximations to) Real numbers
      R
    , Time
    -- * Vectors
    , Vec
    , PosVec
    , Velocity
    , Acceleration
    , vec
    , (^+^)
    , (^-^)
    , (*^)
    , (^*)
    , (^/)
    , (<.>)
    , (><)
    , magnitude
    , zeroV
    , negateV
    , sumV
    , xComp
    , yComp
    , zComp
    , iHat
    , jHat
    , kHat
    , positionCV
    , velocityCA
    , positionCA
    , aParallel
    , aPerp
    , speedRateChange
    -- * Calculus
    , Derivative
    , VecDerivative
    , derivative
    , vecDerivative
    , integral
    , antiDerivative
    , velFromPos
    , accFromVel
    -- ** Differential equations
    , UpdateFunction
    , DifferentialEquation
    , NumericalMethod
    , RealVectorSpace(..)
    , Diff(..)
    , solver
    , euler
    , rungeKutta4
    -- * 3D Mechanics
    -- ** Single particle state
    , ParticleState(..)
    , DParticleState(..)
    , HasTime(..)
    , defaultParticleState
    , newtonSecondPS
    , relativityPS
    , eulerCromerPS
    , statesPS
    , updatePS
    -- ** One-body forces
    , OneBodyForce
    , earthSurfaceGravity
    , sunGravity
    , airResistance
    , windForce
    , uniformLorentzForce
    , fixedLinearSpring
    -- * Interacting particles
    , Force(..)
    , MultiParticleState(..)
    , DMultiParticleState(..)
    -- ** Two-body forces
    , TwoBodyForce
    , universalGravity
    , linearSpring
    , centralForce
    , billiardForce
    , newtonSecondMPS
    , eulerCromerMPS
    , updateMPS
    , statesMPS
    , Justification(..)
    , Table(..)
    , kineticEnergy
    , systemKE
    , momentum
    , systemP
    , linearSpringPE
    , earthSurfaceGravityPE
    , tenths
    , sigFigs
    -- * Electricity
    , elementaryCharge
    , coulombForce
    -- * Coordinate Systems
    , Position
    , Displacement
    , ScalarField
    , VectorField
    , CoordinateSystem
    , cartesian
    , cylindrical
    , spherical
    , cart
    , cyl
    , sph
    , cartesianCoordinates
    , cylindricalCoordinates
    , sphericalCoordinates
    , displacement
    , shiftPosition
    , rHat
    , thetaHat
    , phiHat
    , sHat
    , xHat
    , yHat
    , zHat
    , origin
    , xSF
    , ySF
    , rSF
    , rVF
    , fst3
    , snd3
    , thd3
    , addScalarFields
    , addVectorFields
    , sfTable
    -- * Geometry
    , Curve(..)
    , unitCircle
    , straightLine
    , Surface(..)
    , unitSphere
    , centeredSphere
    , sphere
    , northernHemisphere
    , disk
    , shiftSurface
    , Volume(..)
    , unitBall
    , centeredBall
    , northernHalfBall
    , centeredCylinder
    -- * Electromagnetic Theory
    -- ** Charge
    , Charge
    , ChargeDistribution(..)
    , totalCharge
    , electricDipoleMoment
    -- ** Electric Field
    , epsilon0
    , cSI
    , mu0
    , eField
    , ScalarLineIntegral
    , ScalarSurfaceIntegral
    , ScalarVolumeIntegral
    , VectorLineIntegral
    , VectorSurfaceIntegral
    , VectorVolumeIntegral
    , CurveApprox
    , SurfaceApprox
    , VolumeApprox
    , scalarLineIntegral
    , scalarSurfaceIntegral
    , scalarVolumeIntegral
    , vectorLineIntegral
    , vectorSurfaceIntegral
    , vectorVolumeIntegral
    , dottedLineIntegral
    , dottedSurfaceIntegral
    , curveSample
    , surfaceSample
    , volumeSample
    , Field
    -- ** Current
    , Current
    , CurrentDistribution(..)
    , crossedLineIntegral
    , totalCurrent
    , magneticDipoleMoment
    -- ** Magnetic Field
    , bField
    -- ** Lorentz Force Law
    , lorentzForce
    , newtonSecondPFS
    , defaultPFS
    -- ** Maxwell Equations
    , directionalDerivative
    , curl
    , FieldState
    )
    where

import LPFPCore.SimpleVec
    ( R
    , Vec
    , Time
    , PosVec
    , Velocity
    , Acceleration
    , Derivative
    , VecDerivative
    , vec
    , (^+^)
    , (^-^)
    , (*^)
    , (^*)
    , (^/)
    , (<.>)
    , (><)
    , magnitude
    , zeroV
    , negateV
    , sumV
    , xComp
    , yComp
    , zComp
    , iHat
    , jHat
    , kHat
    , positionCV
    , velocityCA
    , positionCA
    , derivative
    , vecDerivative
    , velFromPos
    , accFromVel
    , aParallel
    , aPerp
    , speedRateChange
    )
import LPFPCore.Newton2
    ( integral
    , antiDerivative
    )
import LPFPCore.Mechanics1D
    ( UpdateFunction
    , DifferentialEquation
    , NumericalMethod
    , RealVectorSpace(..)
    , Diff(..)
    , solver
    , euler
    , rungeKutta4
    )
import LPFPCore.Mechanics3D
    ( ParticleState(..)
    , DParticleState(..)
    , HasTime(..)
    , OneBodyForce
    , defaultParticleState
    , newtonSecondPS
    , relativityPS
    , earthSurfaceGravity
    , sunGravity
    , airResistance
    , windForce
    , uniformLorentzForce
    , eulerCromerPS
    , statesPS
    , updatePS
    )
import LPFPCore.MultipleObjects
    ( TwoBodyForce
    , Force(..)
    , MultiParticleState(..)
    , DMultiParticleState(..)
    , universalGravity
    , linearSpring
    , fixedLinearSpring
    , centralForce
    , billiardForce
    , newtonSecondMPS
    , eulerCromerMPS
    , updateMPS
    , statesMPS
    )
import LPFPCore.MOExamples
    ( Justification(..)
    , Table(..)
    , kineticEnergy
    , systemKE
    , momentum
    , systemP
    , linearSpringPE
    , earthSurfaceGravityPE
    , tenths
    , sigFigs
    )
import LPFPCore.Electricity
    ( Charge
    , elementaryCharge
    , coulombForce
    )
import LPFPCore.CoordinateSystems
    ( Position
    , Displacement
    , ScalarField
    , VectorField
    , CoordinateSystem
    , cartesian
    , cylindrical
    , spherical
    , cart
    , cyl
    , sph
    , cartesianCoordinates
    , cylindricalCoordinates
    , sphericalCoordinates
    , displacement
    , shiftPosition
    , rHat
    , thetaHat
    , phiHat
    , sHat
    , xHat
    , yHat
    , zHat
    , origin
    , xSF
    , ySF
    , rSF
    , rVF
    , fst3
    , snd3
    , thd3
    , addScalarFields
    , addVectorFields
    , sfTable
    )
import LPFPCore.Geometry
    ( Curve(..)
    , unitCircle
    , straightLine
    , Surface(..)
    , unitSphere
    , centeredSphere
    , sphere
    , northernHemisphere
    , disk
    , shiftSurface
    , Volume(..)
    , unitBall
    , centeredBall
    , northernHalfBall
    , centeredCylinder
    )
import LPFPCore.Charge
    ( ChargeDistribution(..)
    , totalCharge
    , electricDipoleMoment
    )
import LPFPCore.ElectricField
    ( epsilon0
    , cSI
    , mu0
    , eField
    , ScalarLineIntegral
    , ScalarSurfaceIntegral
    , ScalarVolumeIntegral
    , VectorLineIntegral
    , VectorSurfaceIntegral
    , VectorVolumeIntegral
    , CurveApprox
    , SurfaceApprox
    , VolumeApprox
    , scalarLineIntegral
    , scalarSurfaceIntegral
    , scalarVolumeIntegral
    , vectorLineIntegral
    , vectorSurfaceIntegral
    , vectorVolumeIntegral
    , dottedLineIntegral
    , dottedSurfaceIntegral
    , curveSample
    , surfaceSample
    , volumeSample
    , Field
    )
import LPFPCore.Current
    ( Current
    , CurrentDistribution(..)
    , crossedLineIntegral
    , totalCurrent
    , magneticDipoleMoment
    )
import LPFPCore.MagneticField
    ( bField
    )
import LPFPCore.Lorentz
    ( lorentzForce
    , newtonSecondPFS
    , defaultPFS
    )
import LPFPCore.Maxwell
    ( directionalDerivative
    , curl
    , FieldState
    )
