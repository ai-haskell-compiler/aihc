{-# OPTIONS -Wall #-}

import LPFP.SimpleVec ( R, zeroV, iHat, (*^), xComp, yComp )
import LPFP.Mechanics3D ( ParticleState(..) )
import LPFP.MultipleObjects ( MultiParticleState(..) )
import LPFP.MOExamples
import Graphics.Gnuplot.Simple

makePNG :: (Int,MultiParticleState) -> IO ()
makePNG (n,MPS sts)
    = let rs = [zeroV] ++ [posVec st | st <- sts] ++ [0.65 *^ iHat]
          xy r = (xComp r, yComp r)
          xys :: [(R,R)]
          xys = map xy rs
          threeDigitString = reverse $ take 3 $ reverse ("00" ++ show n)
          pngFilePath = "GnuplotWave" ++ threeDigitString ++ ".png"
      in plotPath [Title "Wave"
                  ,XLabel "Position (m)"
                  ,YLabel "Displacement (m)"
                  ,XRange (0,0.65)
                  ,YRange (-0.01,0.01)
                  ,PNG pngFilePath
                  ,Key Nothing
                  ] xys

main :: IO ()
main = sequence_ $ map makePNG $ zip [0..999] $
       iterate (stringUpdate 25e-6) (stringInitialOvertone 3)
