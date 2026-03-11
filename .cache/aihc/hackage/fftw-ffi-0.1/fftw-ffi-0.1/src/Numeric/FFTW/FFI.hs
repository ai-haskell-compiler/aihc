module Numeric.FFTW.FFI (
   Type.Plan,
   module Numeric.FFTW.FFI.Generic,
   ptrDestroyPlan,
   ptrFree,
   Type.IODim(..),
   Type.Flags, Type.Flag,
   Type.Kind,
   Type.Sign,

   Type.measure,
   Type.destroyInput,
   Type.unaligned,
   Type.conserveMemory,
   Type.exhaustive,
   Type.preserveInput,
   Type.patient,
   Type.estimate,

   Type.forward,
   Type.backward,

   Type.r2hc,
   Type.hc2r,
   Type.dht,
   Type.redft00,
   Type.redft10,
   Type.redft01,
   Type.redft11,
   Type.rodft00,
   Type.rodft10,
   Type.rodft01,
   Type.rodft11,
   ) where

import qualified Numeric.FFTW.FFI.Type as Type
import qualified Numeric.FFTW.FFI.Double as FFTD
import qualified Numeric.FFTW.FFI.Float  as FFTF
import Numeric.FFTW.FFI.Generic

import qualified Numeric.Netlib.Class as Class

import Foreign.ForeignPtr (FinalizerPtr)
import Foreign.Ptr (Ptr)


newtype DestroyPlan a =
   DestroyPlan {runDestroyPlan :: FinalizerPtr (Type.Plan a)}

ptrDestroyPlan :: (Class.Real a) => FinalizerPtr (Type.Plan a)
ptrDestroyPlan =
   runDestroyPlan $
   Class.switchReal
      (DestroyPlan FFTF.ptrDestroyPlan)
      (DestroyPlan FFTD.ptrDestroyPlan)

newtype Free a = Free {runFree :: FinalizerPtr (Ptr a)}

ptrFree :: (Class.Real a) => FinalizerPtr (Ptr a)
ptrFree =
   runFree $
   Class.switchReal
      (Free FFTF.ptrFree)
      (Free FFTD.ptrFree)
