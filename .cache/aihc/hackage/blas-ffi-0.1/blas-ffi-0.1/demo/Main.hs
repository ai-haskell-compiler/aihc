module Main where

import qualified Numeric.BLAS.FFI.ComplexFloat as BlasComplexFloat
import qualified Foreign
import Foreign.Storable.Complex ()
import Data.Complex (Complex((:+)))


main :: IO ()
main =
   Foreign.with 1 $ \nPtr ->
   Foreign.with (2:+3) $ \cxPtr ->
   Foreign.with 1 $ \incxPtr ->
   print =<< BlasComplexFloat.casum nPtr cxPtr incxPtr
