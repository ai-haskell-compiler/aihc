{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFP.ElectricField
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 25 of the book Learn Physics with Functional Programming
-}

module LPFP.ElectricField where

import LPFP.SimpleVec
    ( R, Vec, (^+^), (^-^), (*^), (^*), (^/), (<.>), (><)
    , sumV, magnitude, vec, xComp, yComp, zComp, kHat )
import LPFP.CoordinateSystems
    ( Position, ScalarField, VectorField
    , displacement, shiftPosition, addVectorFields
    , cart, sph, vf3D, vfPNGxy, vfGrad, origin, rVF )
import LPFP.Geometry ( Curve(..), Surface(..), Volume(..) )
import LPFP.Charge
    ( Charge, ChargeDistribution(..)
    , diskCap, protonOrigin, simpleDipole, lineDipole )

epsilon0 :: R
epsilon0 = 1/(mu0 * cSI**2)

cSI :: R
cSI = 299792458  -- m/s

mu0 :: R
mu0 = 4e-7 * pi  -- N/A^2

eFieldFromPointCharge
    :: Charge       -- in Coulombs
    -> Position     -- of point charge (in m)
    -> VectorField  -- electric field (in V/m)
eFieldFromPointCharge q1 r1 r
    = let k = 1 / (4 * pi * epsilon0)
          d = displacement r1 r
      in (k * q1) *^ d ^/ magnitude d ** 3

eField :: ChargeDistribution -> VectorField
eField (PointCharge   q   r) = eFieldFromPointCharge   q   r
eField (LineCharge    lam c) = eFieldFromLineCharge    lam c
eField (SurfaceCharge sig s) = eFieldFromSurfaceCharge sig s
eField (VolumeCharge  rho v) = eFieldFromVolumeCharge  rho v
eField (MultipleCharges cds) = addVectorFields $ map eField cds

eFieldPicProton2D :: IO ()
eFieldPicProton2D
    = vfPNGxy "eFieldPicProton2D.png" 3e-9 pts (eField protonOrigin)
      where
        pts = [(r * cos th, r * sin th) | r <- [1,1.5,2]
              , th <- [0,pi/4 .. 2*pi]]

eFieldPicProtonGrad :: IO ()
eFieldPicProtonGrad
    = vfGrad (**0.2) (\(x,y) -> cart x y 0) (\v -> (xComp v, yComp v))
      "eFieldPicProtonGrad.png" 20 (eField protonOrigin)

eFieldPicProton3D :: IO ()
eFieldPicProton3D = vf3D 4e-9
                 [sph r th ph | r  <- [1,1.5,2]
                              , th <- [0,pi/4..pi]
                              , ph <- [0,pi/4..2*pi]] (eField protonOrigin)

simpleDipoleSodiumChloride :: ChargeDistribution
simpleDipoleSodiumChloride = simpleDipole (vec 0 0 2.99e-29) 2.36e-10

eFieldSodiumChloride :: VectorField
eFieldSodiumChloride = eField simpleDipoleSodiumChloride

eFieldPicSimpleDipole :: IO ()
eFieldPicSimpleDipole
    = vfGrad (**0.2) (\(y,z) -> cart 0 (3e-10*y) (3e-10*z))
      (\v -> (yComp v, zComp v)) "eFieldPicSimpleDipole.png" 20
      eFieldSodiumChloride

eFieldIdealDipole :: Vec          -- electric dipole moment
                  -> VectorField  -- electric field
eFieldIdealDipole p r
    = let k = 1 / (4 * pi * epsilon0)  -- SI units
          rMag = magnitude (rVF r)
          rUnit = rVF r ^/ rMag
      in k *^ (1 / rMag**3) *^ (3 *^ (p <.> rUnit) *^ rUnit ^-^ p)

eFieldPicIdealDipole :: IO ()
eFieldPicIdealDipole
    = vfGrad (**0.2) (\(y,z) -> cart 0 (3e-10*y) (3e-10*z))
      (\v -> (yComp v, zComp v)) "eFieldPicIdealDipole.png" 20
                                     (eFieldIdealDipole kHat)

type VectorLineIntegral = VectorField -> Curve -> Vec

type CurveApprox = Curve -> [(Position,Vec)]

vectorLineIntegral :: CurveApprox -> VectorField -> Curve -> Vec
vectorLineIntegral approx vF c
    = sumV [vF r' ^* magnitude dl' | (r',dl') <- approx c]

eFieldFromLineCharge
    :: ScalarField  -- linear charge density lambda
    -> Curve        -- geometry of the line charge
    -> VectorField  -- electric field (in V/m)
eFieldFromLineCharge lambda c r
    = let k = 1 / (4 * pi * epsilon0)
          integrand r' = lambda r' *^ d ^/ magnitude d ** 3
              where d = displacement r' r
      in k *^ vectorLineIntegral (curveSample 1000) integrand c

lineDipoleSodiumChloride :: ChargeDistribution
lineDipoleSodiumChloride = lineDipole (vec 0 0 2.99e-29) 2.36e-10

eFieldLineDipole :: VectorField
eFieldLineDipole = eField lineDipoleSodiumChloride

type VectorSurfaceIntegral = VectorField -> Surface -> Vec

type SurfaceApprox = Surface -> [(Position,Vec)]

vectorSurfaceIntegral :: SurfaceApprox -> VectorField -> Surface -> Vec
vectorSurfaceIntegral approx vF s
    = sumV [vF r' ^* magnitude da' | (r',da') <- approx s]

eFieldFromSurfaceCharge
    :: ScalarField  -- surface charge density sigma
    -> Surface      -- geometry of the surface charge
    -> VectorField  -- electric field (in V/m)
eFieldFromSurfaceCharge sigma s r
    = let k = 1 / (4 * pi * epsilon0)
          integrand r' = sigma r' *^ d ^/ magnitude d ** 3
              where d = displacement r' r
      in k *^ vectorSurfaceIntegral (surfaceSample 200) integrand s

eFieldDiskCap :: VectorField
eFieldDiskCap = eField $ diskCap 0.05 0.04 2e-8

eFieldPicDiskCap :: IO ()
eFieldPicDiskCap = vfGrad (**0.2) (\(x,z) -> cart (0.1*x) 0 (0.1*z))
                (\v -> (xComp v, zComp v)) "eFieldPicDiskCap.png" 20
                eFieldDiskCap

type VectorVolumeIntegral = VectorField -> Volume -> Vec

type VolumeApprox = Volume -> [(Position,R)]

vectorVolumeIntegral :: VolumeApprox -> VectorField -> Volume -> Vec
vectorVolumeIntegral approx vF vol
    = sumV [vF r' ^* dv' | (r',dv') <- approx vol]

eFieldFromVolumeCharge
    :: ScalarField  -- volume charge density rho
    -> Volume       -- geometry of the volume charge
    -> VectorField  -- electric field (in V/m)
eFieldFromVolumeCharge rho v r
    = let k = 1 / (4 * pi * epsilon0)
          integrand r' = rho r' *^ d ^/ magnitude d ** 3
              where d = displacement r' r
      in k *^ vectorVolumeIntegral (volumeSample 50) integrand v

type ScalarLineIntegral = ScalarField -> Curve -> R

scalarLineIntegral :: CurveApprox -> ScalarField -> Curve -> R
scalarLineIntegral approx f c
    = sum [f r' * magnitude dl' | (r',dl') <- approx c]

type ScalarSurfaceIntegral = ScalarField -> Surface -> R

scalarSurfaceIntegral :: SurfaceApprox -> ScalarField -> Surface -> R
scalarSurfaceIntegral approx f s
    = sum [f r' * magnitude da' | (r',da') <- approx s]

type ScalarVolumeIntegral = ScalarField -> Volume -> R

scalarVolumeIntegral :: VolumeApprox -> ScalarField -> Volume -> R
scalarVolumeIntegral approx f vol
    = sum [f r' * dv' | (r',dv') <- approx vol]

curveSample :: Int -> Curve -> [(Position,Vec)]
curveSample n c
    = let segCent :: Segment -> Position
          segCent (p1,p2) = shiftPosition ((rVF p1 ^+^ rVF p2) ^/ 2) origin
          segDisp :: Segment -> Vec
          segDisp = uncurry displacement
      in [(segCent seg, segDisp seg) | seg <- segments n c]

type Segment = (Position,Position)

segments :: Int -> Curve -> [Segment]
segments n (Curve g a b)
    = let ps = map g $ linSpaced n a b
      in zip ps (drop 1 ps)

linSpaced :: Int -> R -> R -> [R]
linSpaced n x0 x1 = take (n+1) [x0, x0+dx .. x1]
    where dx = (x1 - x0) / fromIntegral n

surfaceSample :: Int -> Surface -> [(Position,Vec)]
surfaceSample n s = [(triCenter tri, triArea tri) | tri <- triangles n s]

data Triangle = Tri Position Position Position

triCenter :: Triangle -> Position
triCenter (Tri p1 p2 p3)
    = shiftPosition ((rVF p1 ^+^ rVF p2 ^+^ rVF p3) ^/ 3) origin

triArea :: Triangle -> Vec  -- vector area
triArea (Tri p1 p2 p3) = 0.5 *^ (displacement p1 p2 >< displacement p2 p3)

triangles :: Int -> Surface -> [Triangle]
triangles n (Surface g sl su tl tu)
    = let sts = [[(s,t) | t <- linSpaced n (tl s) (tu s)]
                     | s <- linSpaced n sl su]
          stSquares = [( sts !! j     !! k
                       , sts !! (j+1) !! k
                       , sts !! (j+1) !! (k+1)
                       , sts !! j     !! (k+1))
                      | j <- [0..n-1], k <- [0..n-1]]
          twoTriangles (pp1,pp2,pp3,pp4)
              = [Tri (g pp1) (g pp2) (g pp3),Tri (g pp1) (g pp3) (g pp4)]
      in concatMap twoTriangles stSquares

volumeSample :: Int -> Volume -> [(Position,R)]
volumeSample n v = [(tetCenter tet, tetVolume tet) | tet <- tetrahedrons n v]

data Tet = Tet Position Position Position Position

tetCenter :: Tet -> Position
tetCenter (Tet p1 p2 p3 p4)
    = shiftPosition ((rVF p1 ^+^ rVF p2 ^+^ rVF p3 ^+^ rVF p4) ^/ 4) origin

tetVolume :: Tet -> R
tetVolume (Tet p1 p2 p3 p4)
    = abs $ (d1 <.> (d2 >< d3)) / 6
      where
        d1 = displacement p1 p4
        d2 = displacement p2 p4
        d3 = displacement p3 p4

data ParamCube
    = PC { v000 :: (R,R,R)
         , v001 :: (R,R,R)
         , v010 :: (R,R,R)
         , v011 :: (R,R,R)
         , v100 :: (R,R,R)
         , v101 :: (R,R,R)
         , v110 :: (R,R,R)
         , v111 :: (R,R,R)
         }

tetrahedrons :: Int -> Volume -> [Tet]
tetrahedrons n (Volume g sl su tl tu ul uu)
    = let stus = [[[(s,t,u) | u <- linSpaced n (ul s t) (uu s t)]
                            | t <- linSpaced n (tl s) (tu s)]
                            | s <- linSpaced n sl su]
          stCubes = [PC (stus !!  j    !!  k    !!  l   )
                        (stus !!  j    !!  k    !! (l+1))
                        (stus !!  j    !! (k+1) !!  l   )
                        (stus !!  j    !! (k+1) !! (l+1))
                        (stus !! (j+1) !!  k    !!  l   )
                        (stus !! (j+1) !!  k    !! (l+1))
                        (stus !! (j+1) !! (k+1) !!  l   )
                        (stus !! (j+1) !! (k+1) !! (l+1))
                    | j <- [0..n-1], k <- [0..n-1], l <- [0..n-1]]
          tets (PC c000 c001 c010 c011 c100 c101 c110 c111)
              = [Tet (g c000) (g c100) (g c010) (g c001)
                ,Tet (g c011) (g c111) (g c001) (g c010)
                ,Tet (g c110) (g c010) (g c100) (g c111)
                ,Tet (g c101) (g c001) (g c111) (g c100)
                ,Tet (g c111) (g c100) (g c010) (g c001)
                ]
      in concatMap tets stCubes

type Field a = Position -> a

class AbstractVector a where
    zeroVector :: a
    add   :: a -> a -> a
    scale :: R -> a -> a

sumG :: AbstractVector a => [a] -> a
sumG = foldr add zeroVector

generalLineIntegral
    :: AbstractVector a => CurveApprox -> Field a -> Curve -> a
generalLineIntegral approx f c
    = sumG [scale (magnitude dl') (f r') | (r',dl') <- approx c]

dottedSurfaceIntegral :: SurfaceApprox -> VectorField -> Surface -> R
dottedSurfaceIntegral approx vF s
    = sum [vF r' <.> da' | (r',da') <- approx s]

electricFluxFromField :: VectorField -> Surface -> R
electricFluxFromField = undefined

electricFluxFromCharge :: ChargeDistribution -> Surface -> R
electricFluxFromCharge dist = undefined dist

eFieldFromSurfaceChargeP :: SurfaceApprox -> ScalarField -> Surface
                         -> VectorField
eFieldFromSurfaceChargeP approx sigma s r
    = sumV [eFieldFromPointCharge (sigma r' * magnitude da') r' r
                | (r',da') <- approx s]

surfaceArea :: Surface -> R
surfaceArea = undefined

dottedLineIntegral :: CurveApprox -> VectorField -> Curve -> R
dottedLineIntegral approx f c = sum [f r' <.> dl' | (r',dl') <- approx c]

electricPotentialFromField :: VectorField  -- electric field
                           -> ScalarField  -- electric potential
electricPotentialFromField ef r = undefined ef r
