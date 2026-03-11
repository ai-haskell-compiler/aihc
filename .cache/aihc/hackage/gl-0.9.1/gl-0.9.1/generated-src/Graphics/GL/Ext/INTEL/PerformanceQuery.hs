-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.INTEL.PerformanceQuery (
  -- * Extension Support
    gl_INTEL_performance_query

  -- * GL_INTEL_performance_query
  , glBeginPerfQueryINTEL
  , glCreatePerfQueryINTEL
  , glDeletePerfQueryINTEL
  , glEndPerfQueryINTEL
  , glGetFirstPerfQueryIdINTEL
  , glGetNextPerfQueryIdINTEL
  , glGetPerfCounterInfoINTEL
  , glGetPerfQueryDataINTEL
  , glGetPerfQueryIdByNameINTEL
  , glGetPerfQueryInfoINTEL
  , pattern GL_PERFQUERY_COUNTER_DATA_BOOL32_INTEL
  , pattern GL_PERFQUERY_COUNTER_DATA_DOUBLE_INTEL
  , pattern GL_PERFQUERY_COUNTER_DATA_FLOAT_INTEL
  , pattern GL_PERFQUERY_COUNTER_DATA_UINT32_INTEL
  , pattern GL_PERFQUERY_COUNTER_DATA_UINT64_INTEL
  , pattern GL_PERFQUERY_COUNTER_DESC_LENGTH_MAX_INTEL
  , pattern GL_PERFQUERY_COUNTER_DURATION_NORM_INTEL
  , pattern GL_PERFQUERY_COUNTER_DURATION_RAW_INTEL
  , pattern GL_PERFQUERY_COUNTER_EVENT_INTEL
  , pattern GL_PERFQUERY_COUNTER_NAME_LENGTH_MAX_INTEL
  , pattern GL_PERFQUERY_COUNTER_RAW_INTEL
  , pattern GL_PERFQUERY_COUNTER_THROUGHPUT_INTEL
  , pattern GL_PERFQUERY_COUNTER_TIMESTAMP_INTEL
  , pattern GL_PERFQUERY_DONOT_FLUSH_INTEL
  , pattern GL_PERFQUERY_FLUSH_INTEL
  , pattern GL_PERFQUERY_GLOBAL_CONTEXT_INTEL
  , pattern GL_PERFQUERY_GPA_EXTENDED_COUNTERS_INTEL
  , pattern GL_PERFQUERY_QUERY_NAME_LENGTH_MAX_INTEL
  , pattern GL_PERFQUERY_SINGLE_CONTEXT_INTEL
  , pattern GL_PERFQUERY_WAIT_INTEL
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/INTEL/INTEL_performance_query.txt GL_INTEL_performance_query> extension is available.

gl_INTEL_performance_query :: Bool
gl_INTEL_performance_query = member "GL_INTEL_performance_query" extensions
{-# NOINLINE gl_INTEL_performance_query #-}

-- | Usage: @'glBeginPerfQueryINTEL' queryHandle@


glBeginPerfQueryINTEL :: MonadIO m => GLuint -> m ()
glBeginPerfQueryINTEL = ffiuintIOV glBeginPerfQueryINTELFunPtr

glBeginPerfQueryINTELFunPtr :: FunPtr (GLuint -> IO ())
glBeginPerfQueryINTELFunPtr = unsafePerformIO (getProcAddress "glBeginPerfQueryINTEL")

{-# NOINLINE glBeginPerfQueryINTELFunPtr #-}

-- | Usage: @'glCreatePerfQueryINTEL' queryId queryHandle@


glCreatePerfQueryINTEL :: MonadIO m => GLuint -> Ptr GLuint -> m ()
glCreatePerfQueryINTEL = ffiuintPtruintIOV glCreatePerfQueryINTELFunPtr

glCreatePerfQueryINTELFunPtr :: FunPtr (GLuint -> Ptr GLuint -> IO ())
glCreatePerfQueryINTELFunPtr = unsafePerformIO (getProcAddress "glCreatePerfQueryINTEL")

{-# NOINLINE glCreatePerfQueryINTELFunPtr #-}

-- | Usage: @'glDeletePerfQueryINTEL' queryHandle@


glDeletePerfQueryINTEL :: MonadIO m => GLuint -> m ()
glDeletePerfQueryINTEL = ffiuintIOV glDeletePerfQueryINTELFunPtr

glDeletePerfQueryINTELFunPtr :: FunPtr (GLuint -> IO ())
glDeletePerfQueryINTELFunPtr = unsafePerformIO (getProcAddress "glDeletePerfQueryINTEL")

{-# NOINLINE glDeletePerfQueryINTELFunPtr #-}

-- | Usage: @'glEndPerfQueryINTEL' queryHandle@


glEndPerfQueryINTEL :: MonadIO m => GLuint -> m ()
glEndPerfQueryINTEL = ffiuintIOV glEndPerfQueryINTELFunPtr

glEndPerfQueryINTELFunPtr :: FunPtr (GLuint -> IO ())
glEndPerfQueryINTELFunPtr = unsafePerformIO (getProcAddress "glEndPerfQueryINTEL")

{-# NOINLINE glEndPerfQueryINTELFunPtr #-}

-- | Usage: @'glGetFirstPerfQueryIdINTEL' queryId@


glGetFirstPerfQueryIdINTEL :: MonadIO m => Ptr GLuint -> m ()
glGetFirstPerfQueryIdINTEL = ffiPtruintIOV glGetFirstPerfQueryIdINTELFunPtr

glGetFirstPerfQueryIdINTELFunPtr :: FunPtr (Ptr GLuint -> IO ())
glGetFirstPerfQueryIdINTELFunPtr = unsafePerformIO (getProcAddress "glGetFirstPerfQueryIdINTEL")

{-# NOINLINE glGetFirstPerfQueryIdINTELFunPtr #-}

-- | Usage: @'glGetNextPerfQueryIdINTEL' queryId nextQueryId@


glGetNextPerfQueryIdINTEL :: MonadIO m => GLuint -> Ptr GLuint -> m ()
glGetNextPerfQueryIdINTEL = ffiuintPtruintIOV glGetNextPerfQueryIdINTELFunPtr

glGetNextPerfQueryIdINTELFunPtr :: FunPtr (GLuint -> Ptr GLuint -> IO ())
glGetNextPerfQueryIdINTELFunPtr = unsafePerformIO (getProcAddress "glGetNextPerfQueryIdINTEL")

{-# NOINLINE glGetNextPerfQueryIdINTELFunPtr #-}

-- | Usage: @'glGetPerfCounterInfoINTEL' queryId counterId counterNameLength counterName counterDescLength counterDesc counterOffset counterDataSize counterTypeEnum counterDataTypeEnum rawCounterMaxValue@


glGetPerfCounterInfoINTEL :: MonadIO m => GLuint -> GLuint -> GLuint -> Ptr GLchar -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint64 -> m ()
glGetPerfCounterInfoINTEL = ffiuintuintuintPtrcharuintPtrcharPtruintPtruintPtruintPtruintPtruint64IOV glGetPerfCounterInfoINTELFunPtr

glGetPerfCounterInfoINTELFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint64 -> IO ())
glGetPerfCounterInfoINTELFunPtr = unsafePerformIO (getProcAddress "glGetPerfCounterInfoINTEL")

{-# NOINLINE glGetPerfCounterInfoINTELFunPtr #-}

-- | Usage: @'glGetPerfQueryDataINTEL' queryHandle flags dataSize data bytesWritten@


glGetPerfQueryDataINTEL :: MonadIO m => GLuint -> GLuint -> GLsizei -> Ptr () -> Ptr GLuint -> m ()
glGetPerfQueryDataINTEL = ffiuintuintsizeiPtrVPtruintIOV glGetPerfQueryDataINTELFunPtr

glGetPerfQueryDataINTELFunPtr :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr () -> Ptr GLuint -> IO ())
glGetPerfQueryDataINTELFunPtr = unsafePerformIO (getProcAddress "glGetPerfQueryDataINTEL")

{-# NOINLINE glGetPerfQueryDataINTELFunPtr #-}

-- | Usage: @'glGetPerfQueryIdByNameINTEL' queryName queryId@


glGetPerfQueryIdByNameINTEL :: MonadIO m => Ptr GLchar -> Ptr GLuint -> m ()
glGetPerfQueryIdByNameINTEL = ffiPtrcharPtruintIOV glGetPerfQueryIdByNameINTELFunPtr

glGetPerfQueryIdByNameINTELFunPtr :: FunPtr (Ptr GLchar -> Ptr GLuint -> IO ())
glGetPerfQueryIdByNameINTELFunPtr = unsafePerformIO (getProcAddress "glGetPerfQueryIdByNameINTEL")

{-# NOINLINE glGetPerfQueryIdByNameINTELFunPtr #-}

-- | Usage: @'glGetPerfQueryInfoINTEL' queryId queryNameLength queryName dataSize noCounters noInstances capsMask@


glGetPerfQueryInfoINTEL :: MonadIO m => GLuint -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> m ()
glGetPerfQueryInfoINTEL = ffiuintuintPtrcharPtruintPtruintPtruintPtruintIOV glGetPerfQueryInfoINTELFunPtr

glGetPerfQueryInfoINTELFunPtr :: FunPtr (GLuint -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> IO ())
glGetPerfQueryInfoINTELFunPtr = unsafePerformIO (getProcAddress "glGetPerfQueryInfoINTEL")

{-# NOINLINE glGetPerfQueryInfoINTELFunPtr #-}

pattern GL_PERFQUERY_COUNTER_DATA_BOOL32_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_DATA_BOOL32_INTEL = 0x94FC

pattern GL_PERFQUERY_COUNTER_DATA_DOUBLE_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_DATA_DOUBLE_INTEL = 0x94FB

pattern GL_PERFQUERY_COUNTER_DATA_FLOAT_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_DATA_FLOAT_INTEL = 0x94FA

pattern GL_PERFQUERY_COUNTER_DATA_UINT32_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_DATA_UINT32_INTEL = 0x94F8

pattern GL_PERFQUERY_COUNTER_DATA_UINT64_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_DATA_UINT64_INTEL = 0x94F9

pattern GL_PERFQUERY_COUNTER_DESC_LENGTH_MAX_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_DESC_LENGTH_MAX_INTEL = 0x94FF

pattern GL_PERFQUERY_COUNTER_DURATION_NORM_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_DURATION_NORM_INTEL = 0x94F1

pattern GL_PERFQUERY_COUNTER_DURATION_RAW_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_DURATION_RAW_INTEL = 0x94F2

pattern GL_PERFQUERY_COUNTER_EVENT_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_EVENT_INTEL = 0x94F0

pattern GL_PERFQUERY_COUNTER_NAME_LENGTH_MAX_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_NAME_LENGTH_MAX_INTEL = 0x94FE

pattern GL_PERFQUERY_COUNTER_RAW_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_RAW_INTEL = 0x94F4

pattern GL_PERFQUERY_COUNTER_THROUGHPUT_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_THROUGHPUT_INTEL = 0x94F3

pattern GL_PERFQUERY_COUNTER_TIMESTAMP_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_COUNTER_TIMESTAMP_INTEL = 0x94F5

pattern GL_PERFQUERY_DONOT_FLUSH_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_DONOT_FLUSH_INTEL = 0x83F9

pattern GL_PERFQUERY_FLUSH_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_FLUSH_INTEL = 0x83FA

pattern GL_PERFQUERY_GLOBAL_CONTEXT_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_GLOBAL_CONTEXT_INTEL = 0x00000001

pattern GL_PERFQUERY_GPA_EXTENDED_COUNTERS_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_GPA_EXTENDED_COUNTERS_INTEL = 0x9500

pattern GL_PERFQUERY_QUERY_NAME_LENGTH_MAX_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_QUERY_NAME_LENGTH_MAX_INTEL = 0x94FD

pattern GL_PERFQUERY_SINGLE_CONTEXT_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_SINGLE_CONTEXT_INTEL = 0x00000000

pattern GL_PERFQUERY_WAIT_INTEL  :: (Eq a, Num a) => a

pattern GL_PERFQUERY_WAIT_INTEL = 0x83FB