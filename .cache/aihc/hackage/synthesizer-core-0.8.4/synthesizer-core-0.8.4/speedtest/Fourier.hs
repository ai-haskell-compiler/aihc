module Main where

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.Generic.Fourier as Fourier
import qualified Synthesizer.Generic.Noise as NoiseG
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.State.Noise as NoiseS
import qualified Synthesizer.State.Signal as SigS

import qualified Data.StorableVector as SV

import qualified Number.Complex as NPComplex

import System.TimeIt (timeIt, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()



test0 :: IO ()
test0 =
   SigSt.writeFile "fouriertest.f64" $
   SigG.take 65536 $
   (NoiseG.white SigG.defaultLazySize :: SigSt.T Double)

test1 :: IO ()
test1 =
   SigSt.writeFile "fouriertest.f64" $
   SigG.fromState SigG.defaultLazySize $
   SigS.take 65536 $
   SigS.map (NPComplex.+: 0) $
   (NoiseS.white :: SigS.T Double)

test2 :: Int -> IO ()
test2 n =
   writeFile "fouriertest.cache" $
   show $ Fourier.cacheBackward $
   (\sig ->
      SigG.fromState SigG.defaultLazySize sig ::
         SigSt.T (NPComplex.T Double)) $
   SigS.take n $
   SigS.map (NPComplex.+: 0) $
   NoiseS.white

test3 :: Int -> IO ()
test3 n =
   let sig :: SigSt.T (NPComplex.T Double)
       sig =
          SigG.fromState SigG.defaultLazySize $
          SigS.take n $
          SigS.map (NPComplex.+: 0) $
          NoiseS.white
       cache =
          Fourier.cacheBackward sig
   in  do timeIt $ writeFile "fouriertest.cache" $ show cache
          timeIt $ SigSt.writeFile "fouriertest.f64" $
             Fourier.transformWithCache cache sig

test4 :: Int -> IO ()
test4 n =
   let sig :: SV.Vector (NPComplex.T Double)
       sig =
          SigS.toStrictStorableSignal n $
          SigS.take n $
          SigS.map (NPComplex.+: 0) $
          NoiseS.white
       cache =
          Fourier.cacheBackward sig
   in  do -- timeIt $ writeFile "fouriertest.cache" $ show cache
          timeIt $ SV.writeFile "fouriertest.f64" $
             Fourier.transformWithCache cache sig


main :: IO ()
main =
--   timeIt $ test2 (4096*3+1)
--   test4 (4096*3+1)
   sequence_ $
   timeIt test0 : timeIt test1 :
   map test4
      (16384 : (4096*3) : (4096*3+1) : 11025 :
       (3^9) : (5^6) : (7^5) :
       (6^6) : (3*5*7*11*13) :
       [])
{-
      (65536 : 65537 : 44100 :
       (3^10) : (5^7) : (7^5) :
       (6^6) : (2*3*5*7*11*13) :
       [])
-}
