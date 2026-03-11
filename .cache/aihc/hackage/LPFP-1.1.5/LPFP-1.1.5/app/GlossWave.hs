{-# OPTIONS -Wall #-}

import LPFP.SimpleVec ( zeroV, iHat, (*^), xComp, yComp )
import LPFP.Mechanics3D ( ParticleState(..), simulateGloss )
import LPFP.MultipleObjects ( MultiParticleState(..) )
import LPFP.MOExamples
import Graphics.Gloss ( Picture(..), scale, blue )

stringPicture :: MultiParticleState -> Picture
stringPicture (MPS sts)
    = let rs = [zeroV] ++ [posVec st | st <- sts] ++ [0.65 *^ iHat]
          xy r = (realToFrac $ xComp r, realToFrac $ yComp r)
          xys = map xy rs
          ppm = 400  -- pixels per meter
      in scale ppm (20*ppm) $ Color blue $ Line xys

main :: IO ()
main = let initialState = stringInitialOvertone 3
       in simulateGloss 0.001 40 initialState stringPicture stringUpdate
