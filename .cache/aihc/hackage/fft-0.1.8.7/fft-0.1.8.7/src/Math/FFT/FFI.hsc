{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Math.FFT.FFI where

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import Foreign.Storable
          (Storable, sizeOf, alignment, peek, poke, peekByteOff, pokeByteOff)
import Foreign.Storable.Complex ()

import Data.Complex (Complex)
import Data.Generics (Data, Typeable)
import Data.Typeable ()


#include <fftw3.h>


type FFTWFlag = C.CUInt

#{enum FFTWFlag,
 , c_measure         = FFTW_MEASURE
 , c_destroy_input   = FFTW_DESTROY_INPUT
 , c_unaligned       = FFTW_UNALIGNED
 , c_conserve_memory = FFTW_CONSERVE_MEMORY
 , c_exhaustive      = FFTW_EXHAUSTIVE
 , c_preserve_input  = FFTW_PRESERVE_INPUT
 , c_patient         = FFTW_PATIENT
 , c_estimate        = FFTW_ESTIMATE
 }


type FFTWSign = C.CInt

#{enum FFTWSign,
 , c_forward = FFTW_FORWARD
 , c_backward = FFTW_BACKWARD
 }


type FFTWKind = C.CInt

#{enum FFTWKind,
 , c_r2hc    = FFTW_R2HC
 , c_hc2r    = FFTW_HC2R
 , c_dht     = FFTW_DHT
 , c_redft00 = FFTW_REDFT00
 , c_redft10 = FFTW_REDFT10
 , c_redft01 = FFTW_REDFT01
 , c_redft11 = FFTW_REDFT11
 , c_rodft00 = FFTW_RODFT00
 , c_rodft10 = FFTW_RODFT10
 , c_rodft01 = FFTW_RODFT01
 , c_rodft11 = FFTW_RODFT11
 }


-- | Corresponds to the @fftw_iodim@ structure.  It completely describes the
-- layout of each dimension, before and after the transform.
data IODim = IODim { nIODim :: Int  -- ^ Logical size of dimension
                   , isIODim :: Int -- ^ Stride along dimension in input array
                   , osIODim :: Int -- ^ Stride along dimension in output array
                   }
    deriving (Eq, Show, Data, Typeable)

instance Storable IODim where
    sizeOf _ = #{size fftw_iodim}
    alignment _ = alignment (undefined :: C.CInt)
    peek p = do
        n' <- #{peek fftw_iodim, n} p
        is' <- #{peek fftw_iodim, is} p
        os' <- #{peek fftw_iodim, os} p
        return (IODim n' is' os')
    poke p (IODim n' is' os') = do
        #{poke fftw_iodim, n} p n'
        #{poke fftw_iodim, is} p is'
        #{poke fftw_iodim, os} p os'


-- | A plan is an opaque foreign object.
type Plan = Ptr FFTWPlan

type FFTWPlan = ()

-- We use "safe" calls for anything which could take a while so that it won't block
-- other Haskell threads.

-- | Plan a complex to complex transform using the guru interface.
foreign import ccall safe "fftw3.h fftwf_plan_guru_dft" cf_plan_guru_dft
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr (Complex Float)
    -> Ptr (Complex Float) -> FFTWSign -> FFTWFlag -> IO Plan

-- | Plan a real to complex transform using the guru interface.
foreign import ccall safe "fftw3.h fftwf_plan_guru_dft_r2c" cf_plan_guru_dft_r2c
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr Float
    -> Ptr (Complex Float) -> FFTWFlag -> IO Plan

-- | Plan a complex to real transform using the guru interface.
foreign import ccall safe "fftw3.h fftwf_plan_guru_dft_c2r" cf_plan_guru_dft_c2r
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr (Complex Float)
    -> Ptr Float -> FFTWFlag -> IO Plan

-- | Plan a real to real transform using the guru interface.
foreign import ccall safe "fftw3.h fftwf_plan_guru_r2r" cf_plan_guru_r2r
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr Float
    -> Ptr Float -> Ptr FFTWKind -> FFTWFlag -> IO Plan


-- | Plan a complex to complex transform using the guru interface.
foreign import ccall safe "fftw3.h fftw_plan_guru_dft" c_plan_guru_dft
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr (Complex Double)
    -> Ptr (Complex Double) -> FFTWSign -> FFTWFlag -> IO Plan

-- | Plan a real to complex transform using the guru interface.
foreign import ccall safe "fftw3.h fftw_plan_guru_dft_r2c" c_plan_guru_dft_r2c
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr Double
    -> Ptr (Complex Double) -> FFTWFlag -> IO Plan

-- | Plan a complex to real transform using the guru interface.
foreign import ccall safe "fftw3.h fftw_plan_guru_dft_c2r" c_plan_guru_dft_c2r
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr (Complex Double)
    -> Ptr Double -> FFTWFlag -> IO Plan

-- | Plan a real to real transform using the guru interface.
foreign import ccall safe "fftw3.h fftw_plan_guru_r2r" c_plan_guru_r2r
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr Double
    -> Ptr Double -> Ptr FFTWKind -> FFTWFlag -> IO Plan

-- | Simple plan execution
foreign import ccall safe "fftw3.h fftw_execute" c_execute
    :: Plan -> IO ()

-- Execute a plan on different memory than the plan was created for.
-- Alignment /must/ be the same.  If we parallelize a transform of
-- multi-dimensional data by making separate calls within an un-transformed
-- dimension, it is possible that the alignment constraint would not be
-- fulfilled.  However, this only poses a problem for real transforms with odd
-- transform dimension.
foreign import ccall safe "fftw3.h fftw_execute_dft" c_execute_dft
    :: Plan -> Ptr (Complex Double) -> Ptr (Complex Double) -> IO ()
foreign import ccall safe "fftw3.h fftw_execute_dft_r2c" c_execute_dft_r2c
    :: Plan -> Ptr Double -> Ptr (Complex Double) -> IO ()
foreign import ccall safe "fftw3.h fftw_execute_dft_c2r" c_execute_dft_c2r
    :: Plan -> Ptr (Complex Double) -> Ptr Double -> IO ()
foreign import ccall safe "fftw3.h fftw_execute_r2r" c_execute_r2r
    :: Plan -> Ptr Double -> Ptr Double -> IO ()

foreign import ccall unsafe "fftw3.h fftw_export_wisdom_to_string"
        c_export_wisdom_string :: IO CString

foreign import ccall unsafe "fftw3.h fftw_import_wisdom_from_string"
        c_import_wisdom_string :: CString -> IO C.CInt

foreign import ccall unsafe "fftw3.h fftw_import_system_wisdom"
        c_import_wisdom_system :: IO C.CInt

-- | Frees memory allocated by 'fftw_malloc'.  Currently, we only need this to
-- free the wisdom string.
foreign import ccall unsafe "fftw3.h fftw_free" c_free :: Ptr a -> IO ()
