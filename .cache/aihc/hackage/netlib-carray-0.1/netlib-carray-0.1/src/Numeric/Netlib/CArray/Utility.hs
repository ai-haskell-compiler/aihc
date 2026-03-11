module Numeric.Netlib.CArray.Utility (
   FortranIO,
   Util.run,
   Util.runChecked,
   Util.check,
   Util.assert,
   Util.ignore,
   Util.cint,
   Util.alloca,
   Util.allocaArray,
   Util.bool,
   Util.char,
   Util.string,
   Util.float,
   Util.double,
   Util.complexFloat,
   Util.complexDouble,
   Util.real,
   Util.complex,
   Util.number,

   newArray,
   newArray1,
   newArray2,
   newArray3,
   freezeArray,
   sizes1,
   sizes2,
   sizes3,
   range,
   array,
   arrayBounds,
   ioarray,
   unzipBounds,
   (^!),
   ) where

import qualified Numeric.Netlib.Utility as Util
import Numeric.Netlib.Utility (FortranIO)

import qualified Data.Array.IOCArray as IOCArray
import qualified Data.Array.CArray as CArray
import qualified Data.Array.Unsafe as UnsafeArray
import Data.Array.IOCArray (IOCArray, withIOCArray)
import Data.Array.CArray (CArray, withCArray, Ix)

import qualified Foreign.C.Types as C
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)

import Control.Monad.Trans.Cont (ContT(ContT))
import Control.Applicative ((<$>))



newArray :: (Ix i, Storable e) => (i, i) -> IO (IOCArray i e)
newArray bnds = IOCArray.newArray_ bnds

newArray1 :: (Storable e) => Int -> IO (IOCArray Int e)
newArray1 m = newArray (0, m-1)

newArray2 :: (Storable e) => Int -> Int -> IO (IOCArray (Int,Int) e)
newArray2 m n = newArray ((0,0), (m-1,n-1))

newArray3 :: (Storable e) => Int -> Int -> Int -> IO (IOCArray (Int,Int,Int) e)
newArray3 m n k = newArray ((0,0,0), (m-1,n-1,k-1))


freezeArray :: (Ix i, Storable e) => IOCArray i e -> IO (CArray i e)
freezeArray = UnsafeArray.unsafeFreeze


sizes1 :: (Ix i) => (i,i) -> Int
sizes1 = CArray.rangeSize

sizes2 :: (Ix i, Ix j) => ((i,j),(i,j)) -> (Int,Int)
sizes2 ((i0,j0), (i1,j1)) =
   (CArray.rangeSize (i0,i1), CArray.rangeSize (j0,j1))

sizes3 :: (Ix i, Ix j, Ix k) => ((i,j,k),(i,j,k)) -> (Int,Int,Int)
sizes3 ((i0,j0,k0), (i1,j1,k1)) =
   (CArray.rangeSize (i0,i1),
    CArray.rangeSize (j0,j1),
    CArray.rangeSize (k0,k1))


range :: (Int,Int) -> FortranIO r (Ptr C.CInt)
range = Util.cint . CArray.rangeSize



array :: (Storable a) => CArray i a -> FortranIO r (Ptr a)
array = ContT . withCArray

arrayBounds :: (Storable a, Ix i) => CArray i a -> FortranIO r (Ptr a, (i,i))
arrayBounds v = flip (,) (CArray.bounds v) <$> array v

ioarray :: (Storable a) => IOCArray i a -> FortranIO r (Ptr a)
ioarray = ContT . withIOCArray


unzipBounds :: ((i,j),(i,j)) -> ((i,i), (j,j))
unzipBounds ((i0,j0), (i1,j1)) = ((i0,i1), (j0,j1))

(^!) :: (Num a) => a -> Int -> a
x^!n = x^n
