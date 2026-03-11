module Main (main) where

import Numeric.Transform.Fourier.FFT (fft)
import Numeric.Transform.Fourier.FFTHard (fft'2, fft'4)
import Numeric.Transform.Fourier.R2DIF (fft_r2dif)
import Numeric.Transform.Fourier.R2DIT (fft_r2dit)
import Numeric.Transform.Fourier.R4DIF (fft_r4dif)
import Numeric.Transform.Fourier.SRDIF (fft_srdif)
import Numeric.Transform.Fourier.CT (fft_ct1, fft_ct2)
import Numeric.Transform.Fourier.Rader (fft_rader1, fft_rader2)

import Numeric.Random.Generator.MT19937 (genrand)
import Numeric.Random.Distribution.Uniform (uniform53cc)

import Control.Monad (when)
import Data.Complex (Complex((:+)), magnitude)
import Data.Array (Array, listArray, elems, bounds)


len, iter :: Int
len = 2048
iter = 100

m1 :: Double -> Double
m1 x = x - 1

real, imag :: [Double]
real = map m1 $ map (2*) $ uniform53cc $ genrand 42
imag = map m1 $ map (2*) $ uniform53cc $ genrand 43

xl :: [Complex Double]
xl = zipWith (:+) real imag

gendata :: [Complex Double] -> Int -> [Array Int (Complex Double)]
gendata xs n = map (listArray (0,n-1)) $ slice xs n

slice :: [a] -> Int -> [[a]]
slice xs n = take n xs : slice (drop n xs) n

calc :: (array -> Array Int (Complex Double)) -> [array] -> Int -> Double
calc f xs n = magnitude $ sum $ map (sum . elems . f) $ take n xs

f1, f2, f3, f4, f5, f6, f7, f8 ::
   Array Int (Complex Double) -> Array Int (Complex Double)
f1 xs | n == 2    = fft'2 xs
      | n == 4    = fft'4 xs
      | otherwise = fft_r2dit xs n f1
    where n = (snd $ bounds xs) + 1

f2 xs | n == 2    = fft'2 xs
      | n == 4    = fft'4 xs
      | otherwise = fft_r2dif xs n f2
    where n = (snd $ bounds xs) + 1

f3 xs | n == 2    = fft'2 xs
      | n == 4    = fft'4 xs
      | otherwise = fft_r4dif xs n f3
    where n = (snd $ bounds xs) + 1

f4 xs | n == 2    = fft'2 xs
      | n == 4    = fft'4 xs
      | otherwise = fft_srdif xs n f4
    where n = (snd $ bounds xs) + 1

choose1 :: Int -> Int
choose1 n = loop1 1 1
    where loop1 i f | i * i > n = f
	            | (n `mod` i) == 0 && gcd i (n `div` i) == 1 = loop1 (i+1) i
	            | otherwise = loop1 (i+1) f

choose2 :: Int -> Int
choose2 n = loop2 1 1
    where loop2 i f | i * i > n = f
                    | n `mod` i == 0 = loop2 (i+1) i
	            | otherwise = loop2 (i+1) f

choose_factor :: Int -> Int
choose_factor n | i > 1 = i
	        | otherwise = choose2 n
    where i = choose1 n

f5 xs | n == 2    = fft'2 xs
      | n == 4    = fft'4 xs
      | otherwise = fft_ct1 xs l m f5
    where n = (snd $ bounds xs) + 1
	  l = choose_factor n
          m = n `div` l

f6 xs | n == 2    = fft'2 xs
      | n == 4    = fft'4 xs
      | otherwise = fft_ct2 xs l m f6
    where n = (snd $ bounds xs) + 1
	  l = choose_factor n
          m = n `div` l

f7 xs = fft_rader1 xs n
    where n = (snd $ bounds xs) + 1

f8 xs = fft_rader2 xs n fft
    where n = (snd $ bounds xs) + 1

main :: IO ()
main = do
       let xs = gendata xl len
       print $ calc f1 xs iter
       print $ calc f2 xs iter
       print $ calc f3 xs iter
       print $ calc f4 xs iter
       print $ calc f5 xs iter
       print $ calc f6 xs iter
       when False $ print $ calc f7 xs iter
       when False $ print $ calc f8 xs iter
