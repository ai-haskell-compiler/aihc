{-# OPTIONS -Wall #-}

import LPFP.SimpleVec
    ( xComp, yComp )
import LPFP.Mechanics3D
    ( ParticleState(..), simulateGloss, disk, halleyInitial, halleyUpdate )
import Graphics.Gloss
    ( Picture(..), pictures, translate, red, yellow )

diskComet :: Picture
diskComet = Color red (disk 10)

diskSun :: Picture
diskSun = Color yellow (disk 20)

halleyPicture :: ParticleState -> Picture
halleyPicture (ParticleState _m _q _t r _v)
    = pictures [diskSun, translate xPixels yPixels diskComet]
          where
            pixelsPerMeter = 1e-10
            xPixels = pixelsPerMeter * realToFrac (xComp r)
            yPixels = pixelsPerMeter * realToFrac (yComp r)

main :: IO ()
main = simulateGloss (365.25 * 24 * 60 * 60) 400
       halleyInitial halleyPicture halleyUpdate
