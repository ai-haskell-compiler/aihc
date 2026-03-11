import qualified Data.ByteString as BS
import           Criterion.Main

import Graphics.Netpbm


main :: IO ()
main = do
  sipi <- BS.readFile "test/ppms/SIPI.ppm"
  image <- BS.readFile "test/ppms/image.ppm"

  defaultMain [
       bgroup "fib" [ bench "SIPI.ppm" $ whnf parsePPM sipi
                    , bench "image.ppm" $ whnf parsePPM image
                    ] ]
