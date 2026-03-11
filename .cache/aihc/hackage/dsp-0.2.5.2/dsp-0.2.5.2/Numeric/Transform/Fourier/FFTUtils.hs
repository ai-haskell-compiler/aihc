-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.FFTUtils
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions based on the FFT
--
-----------------------------------------------------------------------------

module Numeric.Transform.Fourier.FFTUtils (
   fft_mag, fft_db, fft_phase, fft_grd, fft_info,
   rfft_mag, rfft_db, rfft_phase, rfft_grd, rfft_info,
   write_fft_info, write_rfft_info,
   ) where

import System.IO
import Data.Array
import Data.Complex

import Numeric.Transform.Fourier.FFT
import DSP.Unwrap

magsq :: RealFloat a => Complex a -> a
magsq (x:+y) = x*x + y*y

log10 :: (Floating a, Eq a) => a -> a
log10 0 = -1.0e9
log10 x = logBase 10 x

dot :: RealFloat a => Complex a -> Complex a -> a
dot a b = realPart a * realPart b + imagPart a * imagPart b

eps :: Double
eps = 1.0e-1 :: Double

-- General functions

fft_mag :: (RealFloat b, Integral a, Ix a) =>
           Array a (Complex b) -> Array a b
fft_mag x = fmap magnitude $ fft $ x

fft_db :: (RealFloat b, Integral a, Ix a) =>
          Array a (Complex b) -> Array a b
fft_db x = fmap (10 *) $ fmap log10 $ fmap magsq $ fft $ x

fft_phase :: (Integral a, Ix a) =>
             Array a (Complex Double) -> Array a Double
fft_phase x = unwrap eps $ fmap phase $ fft $ x

fft_grd :: (Integral i, RealFloat a, Ix i) =>
           Array i (Complex a) -> Array i a
fft_grd x = listArray (bounds x') [ dot (x'!i) (dx'!i) / magsq (x'!i) | i <- indices x' ]
    where x'  = fft x
          dx' = fft $ listArray (bounds x) [ fromIntegral i * x!i | i <- indices x ]

fft_info :: (Integral i, Ix i) =>
            Array i (Complex Double)
            -> (Array i Double, Array i Double, Array i Double, Array i Double)
fft_info x = (mag,db,arg,grd)
    where x'  = fft x
          dx' = fft $ listArray (bounds x) [ fromIntegral i * x!i | i <- indices x ]
          mag = fmap magnitude $ x'
          db  = fmap (10 *) $ fmap log10 $ fmap magsq $ x'
          arg = unwrap eps $ fmap phase $ x'
          grd = listArray (bounds x') [ dot (x'!i) (dx'!i) / magsq (x'!i) | i <- indices x' ]

rfft_mag :: (RealFloat b, Integral a, Ix a) =>
            Array a b -> Array a b
rfft_mag x = fmap magnitude $ rfft $ x

rfft_db :: (RealFloat b, Integral a, Ix a) =>
           Array a b -> Array a b
rfft_db x = fmap (10 *) $ fmap log10 $ fmap magsq $ rfft $ x

rfft_phase :: (Integral a, Ix a) =>
              Array a Double -> Array a Double
rfft_phase x = unwrap eps $ fmap phase $ rfft $ x

rfft_grd :: (Integral i, Ix i, RealFloat a) =>
            Array i a -> Array i a
rfft_grd x = listArray (bounds x') [ dot (x'!i) (dx'!i) / magsq (x'!i) | i <- indices x' ]
    where x'  = rfft x
          dx' = rfft $ listArray (bounds x) [ fromIntegral i * x!i | i <- indices x ]

-- I/O

rfft_info :: (Integral i, Ix i) =>
             Array i Double
             -> (Array i Double, Array i Double, Array i Double, Array i Double)
rfft_info x = (mag,db,arg,grd)
    where x'  = rfft x
          dx' = rfft $ listArray (bounds x) [ fromIntegral i * x!i | i <- indices x ]
          mag = fmap magnitude $ x'
          db  = fmap (10 *) $ fmap log10 $ fmap magsq $ x'
          arg = unwrap eps $ fmap phase $ x'
          grd = listArray (bounds x') [ dot (x'!i) (dx'!i) / magsq (x'!i) | i <- indices x' ]

hPrintIndex :: (Integral a, Integral i, Show b) =>
               Handle -> i -> (a, b) -> IO ()
hPrintIndex h n (i,x) = do
   hPutStr   h $ show (fromIntegral i / fromIntegral n :: Double)
   hPutStr   h $ " "
   hPutStrLn h $ show x

write_cvector :: (Show e, Integral i, Ix i) =>
                 FilePath -> Array i e -> IO ()
write_cvector f x = do
   let n = (snd $ bounds x) + 1
   withFile f WriteMode $ \h ->
      mapM_ (hPrintIndex h n) $ assocs x

write_fft_info :: (Ix i, Integral i) =>
                  String -> Array i (Complex Double) -> IO ()
write_fft_info b x = do
   let (mag,db,arg,grd) = fft_info x
   write_cvector (b ++ "_mag.out") mag
   write_cvector (b ++ "_db.out")  db
   write_cvector (b ++ "_arg.out") arg
   write_cvector (b ++ "_grd.out") grd

write_rvector :: Show e => FilePath -> Array Int e -> IO ()
write_rvector f x = do
   let n = (snd $ bounds x) + 1
   withFile f WriteMode $ \h ->
      mapM_ (hPrintIndex h n) $ take (n `div` 2) $ assocs x

write_rfft_info :: String -> Array Int Double -> IO ()
write_rfft_info b x = do
   let (mag,db,arg,grd) = rfft_info x
   write_rvector (b ++ "_mag.out") mag
   write_rvector (b ++ "_db.out")  db
   write_rvector (b ++ "_arg.out") arg
   write_rvector (b ++ "_grd.out") grd
