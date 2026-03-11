{-# OPTIONS_GHC -funbox-strict-fields -ddump-simpl -ddump-simpl-stats -O #-}
{-  -dverbose-core2core -ddump-simpl-iterations -}
{-
This module allows to compare speed of custom Storable instance of Stereo
versus the ones implemented using Storable.Record and Storable.Traversable.
-}
module Main where

-- import qualified Sound.Frame.Stereo as Stereo
import qualified Sound.Frame.Stereo.Traversable as Stereo
-- import qualified Sound.Frame.Stereo.Record as Stereo
import qualified Data.StorableVector as SV

import Control.Applicative (pure, liftA2, )

import Foreign.Storable.Tuple ()

import Data.Int (Int16)

import GHC.Float (float2Int, )


{-
GHC-6.10.4:

Custom Storable instance:
real    0m0.680s
user    0m0.560s
sys     0m0.112s

Storable.Traversable instance:
real    0m25.187s
user    0m25.014s
sys     0m0.152s

Storable.Record instance:
real    0m3.017s
user    0m2.844s
sys     0m0.168s


GHC-7.2.1:

Storable.Traversable instance:
real    0m3.794s
user    0m3.564s
sys     0m0.196s
-}
stereo :: SV.Vector (Stereo.T Int16)
stereo =
   fst $ SV.unfoldrN 10000000
      (\x0 ->
          let frac x = x - fromIntegral (float2Int x)
              x1 = frac (x0+440/44100)
              y0 = 2*x0-1
              toInt16 y =
                 fromIntegral $
                 float2Int (y*fromIntegral(maxBound::Int16))
          in  Just (Stereo.cons (toInt16 y0) (toInt16 y0), x1))
      (0::Float)

{-
GHC-6.10.4:

Custom Storable instance:
real    0m0.698s
user    0m0.668s
sys     0m0.032s

Storable.Traversable instance:
real    0m47.012s
user    0m46.839s
sys     0m0.164s

Storable.Record instance:
real    0m4.382s
user    0m4.336s
sys     0m0.044s


GHC-6.12.1:

Storable.Traversable instance:
real    0m47.021s
user    0m46.827s
sys     0m0.108s

GHC-7.2.1:

Storable.Traversable instance:
real    0m3.933s
user    0m3.864s
sys     0m0.068s
-}
stereoSum :: SV.Vector (Stereo.T Int16) -> Stereo.T Int16
stereoSum =
   SV.foldl' (liftA2 (+)) (pure 0)

stereoPair :: SV.Vector (Int16,Int16)
stereoPair =
   fst $ SV.unfoldrN 10000000
      (\x0 ->
          let frac x = x - fromIntegral (float2Int x)
              x1 = frac (x0+440/44100)
              y0 = 2*x0-1
              toInt16 y =
                 fromIntegral $
                 float2Int (y*fromIntegral(maxBound::Int16))
          in  Just ((toInt16 y0, toInt16 y0), x1))
      (0::Float)


main :: IO ()
main =
   print $ stereoSum stereo
--   SV.writeFile "speed.s16" stereo
