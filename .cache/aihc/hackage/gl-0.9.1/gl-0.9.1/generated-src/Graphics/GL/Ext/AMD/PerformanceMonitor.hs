-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.PerformanceMonitor (
  -- * Extension Support
    gl_AMD_performance_monitor

  -- * GL_AMD_performance_monitor
  , glBeginPerfMonitorAMD
  , glDeletePerfMonitorsAMD
  , glEndPerfMonitorAMD
  , glGenPerfMonitorsAMD
  , glGetPerfMonitorCounterDataAMD
  , glGetPerfMonitorCounterInfoAMD
  , glGetPerfMonitorCounterStringAMD
  , glGetPerfMonitorCountersAMD
  , glGetPerfMonitorGroupStringAMD
  , glGetPerfMonitorGroupsAMD
  , glSelectPerfMonitorCountersAMD
  , pattern GL_COUNTER_RANGE_AMD
  , pattern GL_COUNTER_TYPE_AMD
  , pattern GL_PERCENTAGE_AMD
  , pattern GL_PERFMON_RESULT_AMD
  , pattern GL_PERFMON_RESULT_AVAILABLE_AMD
  , pattern GL_PERFMON_RESULT_SIZE_AMD
  , pattern GL_UNSIGNED_INT64_AMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/AMD/AMD_performance_monitor.txt GL_AMD_performance_monitor> extension is available.

gl_AMD_performance_monitor :: Bool
gl_AMD_performance_monitor = member "GL_AMD_performance_monitor" extensions
{-# NOINLINE gl_AMD_performance_monitor #-}

-- | Usage: @'glBeginPerfMonitorAMD' monitor@


glBeginPerfMonitorAMD :: MonadIO m => GLuint -> m ()
glBeginPerfMonitorAMD = ffiuintIOV glBeginPerfMonitorAMDFunPtr

glBeginPerfMonitorAMDFunPtr :: FunPtr (GLuint -> IO ())
glBeginPerfMonitorAMDFunPtr = unsafePerformIO (getProcAddress "glBeginPerfMonitorAMD")

{-# NOINLINE glBeginPerfMonitorAMDFunPtr #-}

-- | Usage: @'glDeletePerfMonitorsAMD' n monitors@
--
-- The length of @monitors@ should be @n@.


glDeletePerfMonitorsAMD :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeletePerfMonitorsAMD = ffisizeiPtruintIOV glDeletePerfMonitorsAMDFunPtr

glDeletePerfMonitorsAMDFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeletePerfMonitorsAMDFunPtr = unsafePerformIO (getProcAddress "glDeletePerfMonitorsAMD")

{-# NOINLINE glDeletePerfMonitorsAMDFunPtr #-}

-- | Usage: @'glEndPerfMonitorAMD' monitor@


glEndPerfMonitorAMD :: MonadIO m => GLuint -> m ()
glEndPerfMonitorAMD = ffiuintIOV glEndPerfMonitorAMDFunPtr

glEndPerfMonitorAMDFunPtr :: FunPtr (GLuint -> IO ())
glEndPerfMonitorAMDFunPtr = unsafePerformIO (getProcAddress "glEndPerfMonitorAMD")

{-# NOINLINE glEndPerfMonitorAMDFunPtr #-}

-- | Usage: @'glGenPerfMonitorsAMD' n monitors@
--
-- The length of @monitors@ should be @n@.


glGenPerfMonitorsAMD :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenPerfMonitorsAMD = ffisizeiPtruintIOV glGenPerfMonitorsAMDFunPtr

glGenPerfMonitorsAMDFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenPerfMonitorsAMDFunPtr = unsafePerformIO (getProcAddress "glGenPerfMonitorsAMD")

{-# NOINLINE glGenPerfMonitorsAMDFunPtr #-}

-- | Usage: @'glGetPerfMonitorCounterDataAMD' monitor pname dataSize data bytesWritten@
--
-- The length of @data@ should be @dataSize@.
--
-- The length of @bytesWritten@ should be @1@.


glGetPerfMonitorCounterDataAMD :: MonadIO m => GLuint -> GLenum -> GLsizei -> Ptr GLuint -> Ptr GLint -> m ()
glGetPerfMonitorCounterDataAMD = ffiuintenumsizeiPtruintPtrintIOV glGetPerfMonitorCounterDataAMDFunPtr

glGetPerfMonitorCounterDataAMDFunPtr :: FunPtr (GLuint -> GLenum -> GLsizei -> Ptr GLuint -> Ptr GLint -> IO ())
glGetPerfMonitorCounterDataAMDFunPtr = unsafePerformIO (getProcAddress "glGetPerfMonitorCounterDataAMD")

{-# NOINLINE glGetPerfMonitorCounterDataAMDFunPtr #-}

-- | Usage: @'glGetPerfMonitorCounterInfoAMD' group counter pname data@
--
-- The length of @data@ should be @COMPSIZE(pname)@.


glGetPerfMonitorCounterInfoAMD :: MonadIO m => GLuint -> GLuint -> GLenum -> Ptr () -> m ()
glGetPerfMonitorCounterInfoAMD = ffiuintuintenumPtrVIOV glGetPerfMonitorCounterInfoAMDFunPtr

glGetPerfMonitorCounterInfoAMDFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr () -> IO ())
glGetPerfMonitorCounterInfoAMDFunPtr = unsafePerformIO (getProcAddress "glGetPerfMonitorCounterInfoAMD")

{-# NOINLINE glGetPerfMonitorCounterInfoAMDFunPtr #-}

-- | Usage: @'glGetPerfMonitorCounterStringAMD' group counter bufSize length counterString@
--
-- The length of @length@ should be @1@.
--
-- The length of @counterString@ should be @bufSize@.


glGetPerfMonitorCounterStringAMD :: MonadIO m => GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
glGetPerfMonitorCounterStringAMD = ffiuintuintsizeiPtrsizeiPtrcharIOV glGetPerfMonitorCounterStringAMDFunPtr

glGetPerfMonitorCounterStringAMDFunPtr :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
glGetPerfMonitorCounterStringAMDFunPtr = unsafePerformIO (getProcAddress "glGetPerfMonitorCounterStringAMD")

{-# NOINLINE glGetPerfMonitorCounterStringAMDFunPtr #-}

-- | Usage: @'glGetPerfMonitorCountersAMD' group numCounters maxActiveCounters counterSize counters@
--
-- The length of @numCounters@ should be @1@.
--
-- The length of @maxActiveCounters@ should be @1@.
--
-- The length of @counters@ should be @counterSize@.


glGetPerfMonitorCountersAMD :: MonadIO m => GLuint -> Ptr GLint -> Ptr GLint -> GLsizei -> Ptr GLuint -> m ()
glGetPerfMonitorCountersAMD = ffiuintPtrintPtrintsizeiPtruintIOV glGetPerfMonitorCountersAMDFunPtr

glGetPerfMonitorCountersAMDFunPtr :: FunPtr (GLuint -> Ptr GLint -> Ptr GLint -> GLsizei -> Ptr GLuint -> IO ())
glGetPerfMonitorCountersAMDFunPtr = unsafePerformIO (getProcAddress "glGetPerfMonitorCountersAMD")

{-# NOINLINE glGetPerfMonitorCountersAMDFunPtr #-}

-- | Usage: @'glGetPerfMonitorGroupStringAMD' group bufSize length groupString@
--
-- The length of @length@ should be @1@.
--
-- The length of @groupString@ should be @bufSize@.


glGetPerfMonitorGroupStringAMD :: MonadIO m => GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
glGetPerfMonitorGroupStringAMD = ffiuintsizeiPtrsizeiPtrcharIOV glGetPerfMonitorGroupStringAMDFunPtr

glGetPerfMonitorGroupStringAMDFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
glGetPerfMonitorGroupStringAMDFunPtr = unsafePerformIO (getProcAddress "glGetPerfMonitorGroupStringAMD")

{-# NOINLINE glGetPerfMonitorGroupStringAMDFunPtr #-}

-- | Usage: @'glGetPerfMonitorGroupsAMD' numGroups groupsSize groups@
--
-- The length of @numGroups@ should be @1@.
--
-- The length of @groups@ should be @groupsSize@.


glGetPerfMonitorGroupsAMD :: MonadIO m => Ptr GLint -> GLsizei -> Ptr GLuint -> m ()
glGetPerfMonitorGroupsAMD = ffiPtrintsizeiPtruintIOV glGetPerfMonitorGroupsAMDFunPtr

glGetPerfMonitorGroupsAMDFunPtr :: FunPtr (Ptr GLint -> GLsizei -> Ptr GLuint -> IO ())
glGetPerfMonitorGroupsAMDFunPtr = unsafePerformIO (getProcAddress "glGetPerfMonitorGroupsAMD")

{-# NOINLINE glGetPerfMonitorGroupsAMDFunPtr #-}

-- | Usage: @'glSelectPerfMonitorCountersAMD' monitor enable group numCounters counterList@
--
-- The parameter @enable@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @counterList@ should be @numCounters@.


glSelectPerfMonitorCountersAMD :: MonadIO m => GLuint -> GLboolean -> GLuint -> GLint -> Ptr GLuint -> m ()
glSelectPerfMonitorCountersAMD = ffiuintbooleanuintintPtruintIOV glSelectPerfMonitorCountersAMDFunPtr

glSelectPerfMonitorCountersAMDFunPtr :: FunPtr (GLuint -> GLboolean -> GLuint -> GLint -> Ptr GLuint -> IO ())
glSelectPerfMonitorCountersAMDFunPtr = unsafePerformIO (getProcAddress "glSelectPerfMonitorCountersAMD")

{-# NOINLINE glSelectPerfMonitorCountersAMDFunPtr #-}

pattern GL_COUNTER_RANGE_AMD  :: (Eq a, Num a) => a

pattern GL_COUNTER_RANGE_AMD = 0x8BC1

pattern GL_COUNTER_TYPE_AMD  :: (Eq a, Num a) => a

pattern GL_COUNTER_TYPE_AMD = 0x8BC0

pattern GL_PERCENTAGE_AMD  :: (Eq a, Num a) => a

pattern GL_PERCENTAGE_AMD = 0x8BC3

pattern GL_PERFMON_RESULT_AMD  :: (Eq a, Num a) => a

pattern GL_PERFMON_RESULT_AMD = 0x8BC6

pattern GL_PERFMON_RESULT_AVAILABLE_AMD  :: (Eq a, Num a) => a

pattern GL_PERFMON_RESULT_AVAILABLE_AMD = 0x8BC4

pattern GL_PERFMON_RESULT_SIZE_AMD  :: (Eq a, Num a) => a

pattern GL_PERFMON_RESULT_SIZE_AMD = 0x8BC5

pattern GL_UNSIGNED_INT64_AMD  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT64_AMD = 0x8BC2