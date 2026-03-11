module Main where

import Foreign.C.Types (CFloat)
import qualified Sound.JACK.Audio as Audio

main :: IO ()
main = Audio.mainStereo foo

foo :: (CFloat, CFloat) -> IO (CFloat, CFloat)
foo (a, b) = return (a * fac, b * fac)

fac :: CFloat
fac = 2
