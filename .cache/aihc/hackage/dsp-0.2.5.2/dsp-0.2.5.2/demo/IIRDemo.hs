-- Copyright (c) 2003 Matthew P. Donadio (m.p.donadio@ieee.org)
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

module Main (main) where

import qualified DSP.Filter.IIR.Design as IIR
import DSP.Filter.IIR.IIR (iir_df1)
import DSP.Source.Basic (impulse)

import Numeric.Transform.Fourier.FFTUtils (write_rfft_info)

import Data.Array (Array, listArray)


-- Examples from Oppenheim and Schafer

ex7'3lp, ex7'3hp, ex7'3bp, ex7'8, ex7'5, ex7'6a, ex7'6b ::
   (Array Int Double, Array Int Double)

ex7'3lp = IIR.butterworthLowpass  (0.2 * pi, 1 - 0.89125) (0.3 * pi, 0.17783)
ex7'3hp = IIR.butterworthHighpass (0.2 * pi, 1 - 0.89125) (0.3 * pi, 0.17783)
ex7'3bp = IIR.butterworthBandpass (0.2 * pi, 1 - 0.89125) (0.3 * pi, 0.17783)

ex7'8 = IIR.chebyshev1Lowpass (0.2 * pi, 1 - 0.89125) (0.3 * pi, 0.17783)

ex7'5  = IIR.butterworthLowpass (0.4 * pi, 0.01) (0.6 * pi, 0.001)
ex7'6a = IIR.chebyshev1Lowpass  (0.4 * pi, 0.01) (0.6 * pi, 0.001)
ex7'6b = IIR.chebyshev2Lowpass  (0.4 * pi, 0.01) (0.6 * pi, 0.001)

example :: String -> (Array Int Double, Array Int Double) -> IO ()
example name coeffs =
   write_rfft_info name $ listArray (0,999) $ iir_df1 coeffs impulse

main :: IO ()
main = do
   example "ex-7.3lp" ex7'3lp
   example "ex-7.3hp" ex7'3hp
   example "ex-7.3bp" ex7'3bp
   example "ex-7.8"   ex7'8
   example "ex-7.5"   ex7'5
   example "ex-7.6a"  ex7'6a
   example "ex-7.6b"  ex7'6b
