{-# OPTIONS -Wall #-}

{- | 
Module      :  LPFP.Maxwell
Copyright   :  (c) Scott N. Walck 2023
License     :  BSD3 (see LICENSE)
Maintainer  :  Scott N. Walck <walck@lvc.edu>
Stability   :  stable

Code from chapter 29 of the book Learn Physics with Functional Programming
-}

module LPFP.Maxwell where

import LPFP.SimpleVec
    ( R, Vec(..), (^/), (^+^), (^-^), (*^)
    , vec, negateV, magnitude, xComp, yComp, zComp, iHat, jHat, kHat )
import LPFP.CoordinateSystems
    ( ScalarField, VectorField
    , cart, shiftPosition, rVF, magRad )
import LPFP.ElectricField ( cSI, mu0 )
import qualified Data.Map.Strict as M
import qualified Diagrams.Prelude as D
import Diagrams.Prelude
    ( Diagram, Colour
    , PolyType(..), PolyOrientation(..), PolygonOpts(..), V2(..)
    , (#), rotate, deg, rad, polygon, sinA, dims, p2
    , fc, none, lw, blend )
import Diagrams.Backend.Cairo ( B, renderCairo )

directionalDerivative :: Vec -> ScalarField -> ScalarField
directionalDerivative d f r
    = (f (shiftPosition (d ^/ 2) r) - f (shiftPosition (negateV d ^/ 2) r))
      / magnitude d

curl :: R -> VectorField -> VectorField
curl a vf r
    = let vx = xComp . vf
          vy = yComp . vf
          vz = zComp . vf
          derivX = directionalDerivative (a *^ iHat)
          derivY = directionalDerivative (a *^ jHat)
          derivZ = directionalDerivative (a *^ kHat)
      in      (derivY vz r - derivZ vy r) *^ iHat
          ^+^ (derivZ vx r - derivX vz r) *^ jHat
          ^+^ (derivX vy r - derivY vx r) *^ kHat

type FieldState = (R            -- time t
                  ,VectorField  -- electric field E
                  ,VectorField  -- magnetic field B
                  )

maxwellUpdate :: R                   -- dx
              -> R                   -- dt
              -> (R -> VectorField)  -- J
              -> FieldState -> FieldState
maxwellUpdate dx dt j (t,eF,bF)
    = let t'    = t + dt
          eF' r = eF r ^+^ cSI**2 *^ dt *^ (curl dx bF r ^-^ mu0 *^ j t r)
          bF' r = bF r ^-^           dt *^  curl dx eF r
      in (t',eF',bF')

maxwellEvolve :: R                   -- dx
              -> R                   -- dt
              -> (R -> VectorField)  -- J
              -> FieldState -> [FieldState]
maxwellEvolve dx dt j st0 = iterate (maxwellUpdate dx dt j) st0

exLocs, eyLocs, ezLocs, bxLocs, byLocs, bzLocs :: [(Int,Int,Int)]
exLocs = [(nx,ny,nz) | nx <- odds , ny <- evens, nz <- evens]
eyLocs = [(nx,ny,nz) | nx <- evens, ny <- odds , nz <- evens]
ezLocs = [(nx,ny,nz) | nx <- evens, ny <- evens, nz <- odds ]
bxLocs = [(nx,ny,nz) | nx <- evens, ny <- odds , nz <- odds ]
byLocs = [(nx,ny,nz) | nx <- odds , ny <- evens, nz <- odds ]
bzLocs = [(nx,ny,nz) | nx <- odds , ny <- odds , nz <- evens]

spaceStepsCE :: Int
spaceStepsCE = 40

hiEven :: Int
hiEven =  2 * spaceStepsCE

evens :: [Int]
evens = [-hiEven, -hiEven + 2 .. hiEven]

odds :: [Int]
odds = [-hiEven + 1, -hiEven + 3 .. hiEven - 1]

data StateFDTD = StateFDTD {timeFDTD :: R
                           ,stepX    :: R
                           ,stepY    :: R
                           ,stepZ    :: R
                           ,eField   :: M.Map (Int,Int,Int) R
                           ,bField   :: M.Map (Int,Int,Int) R
                           } deriving Show

initialStateFDTD :: R -> StateFDTD
initialStateFDTD spatialStep
    = StateFDTD {timeFDTD  = 0
                ,stepX = spatialStep
                ,stepY = spatialStep
                ,stepZ = spatialStep
                ,eField = M.fromList [(loc,0) | loc <- exLocs++eyLocs++ezLocs]
                ,bField = M.fromList [(loc,0) | loc <- bxLocs++byLocs++bzLocs]
                }

lookupAZ :: Ord k => k -> M.Map k R -> R
lookupAZ key m = case M.lookup key m of
                     Nothing -> 0
                     Just x  -> x

partialX,partialY,partialZ :: R -> M.Map (Int,Int,Int) R -> (Int,Int,Int) -> R
partialX dx m (i,j,k) = (lookupAZ (i+1,j,k) m - lookupAZ (i-1,j,k) m) / dx
partialY dy m (i,j,k) = (lookupAZ (i,j+1,k) m - lookupAZ (i,j-1,k) m) / dy
partialZ dz m (i,j,k) = (lookupAZ (i,j,k+1) m - lookupAZ (i,j,k-1) m) / dz

curlEx,curlEy,curlEz,curlBx,curlBy,curlBz :: StateFDTD -> (Int,Int,Int) -> R
curlBx (StateFDTD _ _ dy dz _ b) loc = partialY dy b loc - partialZ dz b loc
curlBy (StateFDTD _ dx _ dz _ b) loc = partialZ dz b loc - partialX dx b loc
curlBz (StateFDTD _ dx dy _ _ b) loc = partialX dx b loc - partialY dy b loc
curlEx (StateFDTD _ _ dy dz e _) loc = partialY dy e loc - partialZ dz e loc
curlEy (StateFDTD _ dx _ dz e _) loc = partialZ dz e loc - partialX dx e loc
curlEz (StateFDTD _ dx dy _ e _) loc = partialX dx e loc - partialY dy e loc

stateUpdate :: R                   -- dt
            -> (R -> VectorField)  -- current density J
            -> StateFDTD -> StateFDTD
stateUpdate dt j st0@(StateFDTD t _dx _dy _dz _e _b)
    = let st1 = updateE dt (j t) st0
          st2 = updateB dt st1
      in st2

updateE :: R            -- time step dt
        -> VectorField  -- current density J
        -> StateFDTD -> StateFDTD
updateE dt jVF st
    = st { timeFDTD = timeFDTD st + dt / 2
         , eField   = M.mapWithKey (updateEOneLoc dt jVF st) (eField st) }

updateB :: R -> StateFDTD -> StateFDTD
updateB dt st
    = st { timeFDTD = timeFDTD st + dt / 2
         , bField   = M.mapWithKey (updateBOneLoc dt st) (bField st) }

updateEOneLoc :: R -> VectorField -> StateFDTD -> (Int,Int,Int) -> R -> R
updateEOneLoc dt jVF st (nx,ny,nz) ec
    = let r = cart (fromIntegral nx * stepX st / 2)
                   (fromIntegral ny * stepY st / 2)
                   (fromIntegral nz * stepZ st / 2)
          Vec jx jy jz = jVF r
      in case (odd nx, odd ny, odd nz) of
           (True , False, False)
               -> ec + cSI**2 * (curlBx st (nx,ny,nz) - mu0 * jx) * dt  -- Ex
           (False, True , False)
               -> ec + cSI**2 * (curlBy st (nx,ny,nz) - mu0 * jy) * dt  -- Ey
           (False, False, True )
               -> ec + cSI**2 * (curlBz st (nx,ny,nz) - mu0 * jz) * dt  -- Ez
           _ -> error "updateEOneLoc passed bad indices"

updateBOneLoc :: R -> StateFDTD -> (Int,Int,Int) -> R -> R
updateBOneLoc dt st (nx,ny,nz) bc
    = case (odd nx, odd ny, odd nz) of
        (False, True , True ) -> bc - curlEx st (nx,ny,nz) * dt  -- Bx
        (True , False, True ) -> bc - curlEy st (nx,ny,nz) * dt  -- By
        (True , True , False) -> bc - curlEz st (nx,ny,nz) * dt  -- Bz
        _ -> error "updateBOneLoc passed bad indices"

jGaussian :: R -> VectorField
jGaussian t r
    = let wavelength = 1.08             -- meters
          frequency = cSI / wavelength  -- Hz
          j0 = 77.5                     -- A/m^2
          l = 0.108                     -- meters
          rMag = magnitude (rVF r)      -- meters
      in j0 *^ exp (-rMag**2 / l**2) *^ cos (2*pi*frequency*t) *^ kHat

makeEpng :: (Colour R, Colour R) -> (Int,StateFDTD) -> IO ()
makeEpng (scol,zcol) (n,StateFDTD _ _ _ _ em _)
    = let threeDigitString = reverse $ take 3 $ reverse ("00" ++ show n)
          pngFilePath = "MaxVF" ++ threeDigitString ++ ".png"
          strongE = 176  -- V/m
          vs = [((fromIntegral nx, fromIntegral nz),(xComp ev, zComp ev))
               | nx <- evens, nz <- evens, abs nx <= 50, abs nz <= 50
               , let ev = getAverage (nx,0,nz) em ^/ strongE]
      in gradientVectorPNG pngFilePath (scol,zcol) vs

getAverage :: (Int,Int,Int)  -- (even,even,even) or (odd,odd,odd)
           -> M.Map (Int,Int,Int) R
           -> Vec
getAverage (i,j,k) m
    = let vXl = lookupAZ (i-1,j  ,k  ) m
          vYl = lookupAZ (i  ,j-1,k  ) m
          vZl = lookupAZ (i  ,j  ,k-1) m
          vXr = lookupAZ (i+1,j  ,k  ) m
          vYr = lookupAZ (i  ,j+1,k  ) m
          vZr = lookupAZ (i  ,j  ,k+1) m
      in vec ((vXl+vXr)/2) ((vYl+vYr)/2) ((vZl+vZr)/2)

gradientVectorPNG :: FilePath
                  -> (Colour R, Colour R)
                  -> [((R,R),(R,R))]
                  -> IO ()
gradientVectorPNG fileName (scol,zcol) vs
    = let maxX = maximum $ map fst $ map fst $ vs
          normalize (x,y) = (x/maxX,y/maxX)
          array = [(normalize (x,y), magRad v) | ((x,y),v) <- vs]
          arrowMagRadColors :: R  -- magnitude
                            -> R  -- angle in radians, ccw from x axis
                            -> Diagram B
          arrowMagRadColors mag th
              = let r      = sinA (15 D.@@ deg) / sinA (60 D.@@ deg)
                    myType = PolyPolar [120 D.@@ deg,  0 D.@@ deg, 45 D.@@ deg
                                       , 30 D.@@ deg, 45 D.@@ deg,  0 D.@@ deg
                                       ,120 D.@@ deg]
                             [1,1,r,1,1,r,1,1]
                    myOpts = PolygonOpts myType NoOrient (p2 (0,0))
                in D.scale 0.5 $ polygon myOpts # lw none #
                   fc (blend mag scol zcol) # rotate (th D.@@ rad)
          step = 2 / (sqrt $ fromIntegral $ length vs)
          scaledArrow m th = D.scale step $ arrowMagRadColors m th
          pic = D.position [(p2 pt, scaledArrow m th) | (pt,(m,th)) <- array]
      in renderCairo fileName (dims (V2 1024 1024)) pic
