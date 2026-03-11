module Numeric.FFTW.FFI.Type where

import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr)
import Foreign.Storable
          (Storable, sizeOf, alignment, peek, poke, peekByteOff, pokeByteOff)

import qualified Data.EnumBitSet as EnumSet


#include <Numeric/FFTW/FFI/Common.h>
#include <fftw3.h>


type Flags = EnumSet.T C.CUInt Flag
{-
Warning: Enum methods do not match flag positions.
Thus we keep the Flag type private.
-}
data Flag =
     Measure
   | DestroyInput
   | Unaligned
   | ConserveMemory
   | Exhaustive
   | PreserveInput
   | Patient
   | Estimate
   deriving (Eq, Ord, Enum)

#{enum Flags, EnumSet.Cons
   , measure        = FFTW_MEASURE
   , destroyInput   = FFTW_DESTROY_INPUT
   , unaligned      = FFTW_UNALIGNED
   , conserveMemory = FFTW_CONSERVE_MEMORY
   , exhaustive     = FFTW_EXHAUSTIVE
   , preserveInput  = FFTW_PRESERVE_INPUT
   , patient        = FFTW_PATIENT
   , estimate       = FFTW_ESTIMATE
   }


newtype Sign = Sign C.CInt
   deriving (Eq)

#{enum Sign, Sign
   , forward  = FFTW_FORWARD
   , backward = FFTW_BACKWARD
   }


newtype Kind = Kind C.CInt
   deriving (Eq)

#{enum Kind, Kind
   , r2hc    = FFTW_R2HC
   , hc2r    = FFTW_HC2R
   , dht     = FFTW_DHT
   , redft00 = FFTW_REDFT00
   , redft10 = FFTW_REDFT10
   , redft01 = FFTW_REDFT01
   , redft11 = FFTW_REDFT11
   , rodft00 = FFTW_RODFT00
   , rodft10 = FFTW_RODFT10
   , rodft01 = FFTW_RODFT01
   , rodft11 = FFTW_RODFT11
   }


{- |
Corresponds to the @fftw_iodim@ structure.
It completely describes the layout of each dimension,
before and after the transform.
-}
data IODim = IODim {
     ioDimN  :: Int -- ^ Logical size of dimension
   , ioDimIS :: Int -- ^ Stride along dimension in input array
   , ioDimOS :: Int -- ^ Stride along dimension in output array
   }
   deriving (Eq, Show)

instance Storable IODim where
   sizeOf _ = #{size fftw_iodim}
   alignment _ = #{alignment fftw_iodim}
   peek p = do
      n'  <- #{peek fftw_iodim,  n} p
      is' <- #{peek fftw_iodim, is} p
      os' <- #{peek fftw_iodim, os} p
      return (IODim n' is' os')
   poke p (IODim n' is' os') = do
      #{poke fftw_iodim,  n} p  n'
      #{poke fftw_iodim, is} p is'
      #{poke fftw_iodim, os} p os'


type Plan a = Ptr (PlanObj a)
data PlanObj a = PlanObj
