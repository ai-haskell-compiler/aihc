module Numeric.FFTW.Private where

import qualified Numeric.FFTW.FFI as FFI
import qualified Numeric.Netlib.Class as Class

import qualified Foreign.C.Types as C
import Foreign.Marshal.Array (copyArray, withArrayLen)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Unchecked (Array(Array))

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket)

import qualified Test.QuickCheck as QC


{- |
This lock must be taken during /planning/ of any transform.
The FFTW library is not thread-safe in the planning phase.
Thankfully, the lock is not needed during the execution phase.
-}
{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

withLock :: IO a -> IO a
withLock = withMVar lock . const


run :: Class.Real a => IO (FFI.Plan a) -> IO ()
run planner = bracket (withLock planner) FFI.destroyPlan FFI.execute

runCopiedArray ::
   (Shape.C sh, Class.Floating b, Class.Real a) =>
   Array sh b -> (Ptr b -> IO (FFI.Plan a)) -> IO ()
runCopiedArray (Array sh x) planner =
   withForeignPtr x $ \ptr ->
   let n = Shape.size sh in
   allocaArray n $ \tmpPtr -> run $ do
      plan <- planner tmpPtr
      copyArray tmpPtr ptr n
      return plan



{- |
Order is chosen such that the numeric sign is @(-1) ^ fromEnum sign@.
-}
data Sign = Backward | Forward
   deriving (Eq, Ord, Enum, Show)

instance QC.Arbitrary Sign where
   arbitrary = QC.elements [Backward, Forward]

flipSign :: Sign -> Sign
flipSign Backward = Forward
flipSign Forward = Backward

ffiSign :: Sign -> FFI.Sign
ffiSign Backward = FFI.backward
ffiSign Forward = FFI.forward


allocaArray :: (Class.Floating a) => Int -> (Ptr a -> IO b) -> IO b
allocaArray n =
   case mallocFree of
      MallocFree alloc free -> bracket (alloc (fromIntegral n)) (free . castPtr)

data MallocFree a = MallocFree (C.CSize -> IO (Ptr a)) (Ptr a -> IO ())

mallocFree :: (Class.Floating a) => MallocFree a
mallocFree =
   Class.switchFloating
      (MallocFree FFI.allocReal FFI.free)
      (MallocFree FFI.allocReal FFI.free)
      (MallocFree FFI.allocComplex FFI.freeComplex)
      (MallocFree FFI.allocComplex FFI.freeComplex)


withDims :: [C.CInt] -> (C.CInt -> Ptr C.CInt -> IO a) -> IO a
withDims dims f =
   withArrayLen dims $ \len dimPtr -> f (fromIntegral len) dimPtr
