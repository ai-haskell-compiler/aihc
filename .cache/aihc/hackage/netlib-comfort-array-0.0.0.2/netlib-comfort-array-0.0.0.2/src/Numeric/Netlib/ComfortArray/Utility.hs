module Numeric.Netlib.ComfortArray.Utility (
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

   ZeroInt,
   newArray,
   newArray1,
   newArray2,
   newArray3,
   freezeArray,
   sizes1,
   sizes2,
   sizes3,

   shapeSize,
   array,
   ioarray,
   (^!),
   ) where

import qualified Numeric.Netlib.Utility as Util
import Numeric.Netlib.Utility (FortranIO)

import qualified Data.Array.Comfort.Storable.Mutable.Unchecked as MutArray
import qualified Data.Array.Comfort.Storable.Unchecked as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Mutable (IOArray)
import Data.Array.Comfort.Storable (Array)

import qualified Foreign.C.Types as C
import Foreign.Storable (Storable)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)

import Control.Monad.Trans.Cont (ContT(ContT))



type ZeroInt = Shape.ZeroBased Int

newArray :: (Shape.C sh, Storable e) => sh -> IO (IOArray sh e)
newArray sh = MutArray.unsafeCreate sh (\_ -> return ())

newArray1 :: (Storable e) => Int -> IO (IOArray ZeroInt e)
newArray1 m = newArray (Shape.ZeroBased m)

newArray2 :: (Storable e) => Int -> Int -> IO (IOArray (ZeroInt,ZeroInt) e)
newArray2 m n = newArray (Shape.ZeroBased m, Shape.ZeroBased n)

newArray3 ::
   (Storable e) => Int -> Int -> Int -> IO (IOArray (ZeroInt,ZeroInt,ZeroInt) e)
newArray3 m n k =
   newArray (Shape.ZeroBased m, Shape.ZeroBased n, Shape.ZeroBased k)


freezeArray :: (Shape.C sh, Storable e) => IOArray sh e -> IO (Array sh e)
freezeArray = MutArray.unsafeFreeze


sizes1 :: (Shape.C sh0) => sh0 -> Int
sizes1 = Shape.size

sizes2 :: (Shape.C sh0, Shape.C sh1) => (sh0,sh1) -> (Int,Int)
sizes2 (sh0,sh1) = (Shape.size sh0, Shape.size sh1)

sizes3 ::
   (Shape.C sh0, Shape.C sh1, Shape.C sh2) => (sh0,sh1,sh2) -> (Int,Int,Int)
sizes3 (sh0,sh1,sh2) = (Shape.size sh0, Shape.size sh1, Shape.size sh2)


shapeSize :: (Shape.C sh) => sh -> FortranIO r (Ptr C.CInt)
shapeSize = Util.cint . Shape.size

array :: (Storable a) => Array i a -> FortranIO r (Ptr a)
array (Array.Array _sh fptr) = ContT $ withForeignPtr fptr

ioarray :: (Storable a) => IOArray i a -> FortranIO r (Ptr a)
ioarray = ContT . MutArray.withPtr


(^!) :: (Num a) => a -> Int -> a
x^!n = x^n
