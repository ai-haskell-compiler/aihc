module Numeric.LAPACK.CArray.Demo where

import qualified Numeric.LAPACK.CArray.Float as Lapack

import qualified Data.Array.IOCArray as IOCArray
import qualified Data.Array.CArray as CArray
import Data.Array.CArray (CArray)

import Control.Monad (liftM2)



gels ::
   CArray (Int,Int) Float -> CArray (Int,Int) Float ->
   IO (CArray (Int,Int) Float, CArray (Int,Int) Float)
gels a b = do
   ioa <- IOCArray.thaw a
   iob <- IOCArray.thaw b
   print =<< Lapack.gels 'N' 3 ioa iob 100
   liftM2 (,)
      (IOCArray.freeze ioa)
      (IOCArray.freeze iob)

main :: IO ()
main =
   print =<<
   gels
      (CArray.listArray ((0,0),(2,2)) [1,2,3,0,1,4,0,0,1])
      (CArray.listArray ((0,0),(2,2)) [1,0,0,0,1,0,0,0,1])
