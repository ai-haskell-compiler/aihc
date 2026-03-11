-- This program was used to generate the data for
--
-- Matthew Donadio, "Lost Knowledge Refound: Sharpened FIR Filters,"
-- IEEE Signal Processing Magazine, to appear

module Main (main) where

import DSP.Filter.FIR.Sharpen (sharpen)
import DSP.Filter.FIR.FIR (fir)
import DSP.Source.Basic (impulse)

import Numeric.Transform.Fourier.FFTUtils (write_rfft_info)

import Data.Array (Array, listArray)


n :: Int
n = 1000

h :: Array Int Double
h = listArray (0,16) [ -0.016674, -0.022174,  0.015799, 0.047422, -0.013137,
		       -0.090271,  0.021409,  0.31668,  0.48352,   0.31668,
		        0.021409, -0.090271, -0.013137, 0.047422,  0.015799,
		       -0.022174, -0.016674 ]

y1, y2, y3 :: [Double]
y1 = fir h         $ impulse
y2 = fir h $ fir h $ impulse
y3 = sharpen h     $ impulse

example :: String -> [Double] -> IO ()
example name y = write_rfft_info name $ listArray (0,n-1) $ y

main :: IO ()
main = do
   example "y1" y1
   example "y2" y2
   example "y3" y3
