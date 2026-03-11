{-# LANGUAGE FlexibleContexts #-}

module Numeric.HHT.Internal.FFT (
    fft
  , ifft
  ) where

import           Data.Complex
import qualified Data.Array.CArray         as CA
import qualified Data.Array.IArray         as IA
import qualified Data.Ix                   as Ix
import qualified Data.Vector.Generic       as VG
import qualified Data.Vector.Generic.Sized as SVG
import qualified Foreign.Storable          as FS
import qualified Math.FFT                  as FFT
import qualified Math.FFT.Base             as FFT

fft :: (FFT.FFTWReal a, VG.Vector v (Complex a))
    => SVG.Vector v n (Complex a)
    -> SVG.Vector v n (Complex a)
fft = SVG.withVectorUnsafe $
        fromCA
      . FFT.dft
      . toCA

ifft
    :: (FFT.FFTWReal a, VG.Vector v (Complex a))
    => SVG.Vector v n (Complex a)
    -> SVG.Vector v n (Complex a)
ifft = SVG.withVectorUnsafe $
        fromCA
      . FFT.idft
      . toCA

fromCA
    :: (FS.Storable a, VG.Vector v (Complex a))
    => CA.CArray Int (Complex a)
    -> v (Complex a)
fromCA v = VG.generate (Ix.rangeSize (IA.bounds v)) (v IA.!)

toCA
    :: (FS.Storable a, VG.Vector v (Complex a))
    => v (Complex a)
    -> CA.CArray Int (Complex a)
toCA v = IA.listArray (0, VG.length v - 1) (VG.toList v)
