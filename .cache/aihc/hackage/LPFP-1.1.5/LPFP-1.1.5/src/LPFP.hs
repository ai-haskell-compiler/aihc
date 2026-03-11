{-# OPTIONS -Wall #-}
{-# LANGUAGE Trustworthy #-}

{- | 
Module      :  LPFP
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from the book Learn Physics with Functional Programming
-}

module LPFP
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
    -- ** Graphics utilities
    , simulateGloss
    , simulateVis
    , v3FromVec
    , orient
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
    , animateGloss
    , animateVis
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
    , sf3D
    , vf3D
    , v3FromPos
    , sfTable
    , vfPNG
    , vfPNGxy
    , vfGrad
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
    , pfsVisObject
    , animatePFS
    -- ** Maxwell Equations
    , directionalDerivative
    , curl
    , FieldState
    -- * Plotting
    , plotFunc
    , plotFuncs
    , plotPath
    , plotPaths
    )
    where

import LPFP.SimpleVec
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
import LPFP.Newton2
    ( integral
    , antiDerivative
    )
import LPFP.Mechanics1D
    ( UpdateFunction
    , DifferentialEquation
    , NumericalMethod
    , RealVectorSpace(..)
    , Diff(..)
    , solver
    , euler
    , rungeKutta4
    )
import LPFP.Mechanics3D
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
    , simulateGloss
    , simulateVis
    , v3FromVec
    , orient
    )
import LPFP.MultipleObjects
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
import LPFP.MOExamples
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
    , animateGloss
    , animateVis
    )
import LPFP.Electricity
    ( Charge
    , elementaryCharge
    , coulombForce
    )
import LPFP.CoordinateSystems
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
    , sf3D
    , vf3D
    , v3FromPos
    , sfTable
    , vfPNG
    , vfPNGxy
    , vfGrad
    )
import LPFP.Geometry
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
import LPFP.Charge
    ( ChargeDistribution(..)
    , totalCharge
    , electricDipoleMoment
    )
import LPFP.ElectricField
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
import LPFP.Current
    ( Current
    , CurrentDistribution(..)
    , crossedLineIntegral
    , totalCurrent
    , magneticDipoleMoment
    )
import LPFP.MagneticField
    ( bField
    )
import LPFP.Lorentz
    ( lorentzForce
    , newtonSecondPFS
    , defaultPFS
    , pfsVisObject
    , animatePFS
    )
import LPFP.Maxwell
    ( directionalDerivative
    , curl
    , FieldState
    )
import Graphics.Gnuplot.Simple
    ( plotFunc
    , plotFuncs
    , plotPath
    , plotPaths
    )
