module Numeric.LAPACK.ComfortArray.Demo where

import qualified Numeric.LAPACK.ComfortArray.Float as Lapack
import Numeric.Netlib.ComfortArray.Utility (ZeroInt)

import qualified Data.Array.Comfort.Storable.Mutable as MutArray
import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable (Array)

import Control.Monad (liftM2)



gels ::
   Array (ZeroInt,ZeroInt) Float -> Array (ZeroInt,ZeroInt) Float ->
   IO (Array (ZeroInt,ZeroInt) Float, Array (ZeroInt,ZeroInt) Float)
gels a b = do
   ioa <- MutArray.thaw a
   iob <- MutArray.thaw b
   print =<< Lapack.gels 'N' 3 ioa iob 100
   liftM2 (,)
      (MutArray.freeze ioa)
      (MutArray.freeze iob)

main :: IO ()
main =
   let z3 = Shape.ZeroBased 3
   in print =<<
      gels
         (Array.fromList (z3,z3) [1,2,3,0,1,4,0,0,1])
         (Array.fromList (z3,z3) [1,0,0,0,1,0,0,0,1])
