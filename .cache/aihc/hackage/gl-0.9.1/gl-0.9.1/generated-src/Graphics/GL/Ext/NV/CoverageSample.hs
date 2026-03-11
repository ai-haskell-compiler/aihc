-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.CoverageSample (
  -- * Extension Support
    gl_NV_coverage_sample

  -- * GL_NV_coverage_sample
  , glCoverageMaskNV
  , glCoverageOperationNV
  , pattern GL_COVERAGE_ALL_FRAGMENTS_NV
  , pattern GL_COVERAGE_ATTACHMENT_NV
  , pattern GL_COVERAGE_AUTOMATIC_NV
  , pattern GL_COVERAGE_BUFFERS_NV
  , pattern GL_COVERAGE_BUFFER_BIT_NV
  , pattern GL_COVERAGE_COMPONENT4_NV
  , pattern GL_COVERAGE_COMPONENT_NV
  , pattern GL_COVERAGE_EDGE_FRAGMENTS_NV
  , pattern GL_COVERAGE_SAMPLES_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_coverage_sample.txt GL_NV_coverage_sample> extension is available.

gl_NV_coverage_sample :: Bool
gl_NV_coverage_sample = member "GL_NV_coverage_sample" extensions
{-# NOINLINE gl_NV_coverage_sample #-}

-- | Usage: @'glCoverageMaskNV' mask@


glCoverageMaskNV :: MonadIO m => GLboolean -> m ()
glCoverageMaskNV = ffibooleanIOV glCoverageMaskNVFunPtr

glCoverageMaskNVFunPtr :: FunPtr (GLboolean -> IO ())
glCoverageMaskNVFunPtr = unsafePerformIO (getProcAddress "glCoverageMaskNV")

{-# NOINLINE glCoverageMaskNVFunPtr #-}

-- | Usage: @'glCoverageOperationNV' operation@


glCoverageOperationNV :: MonadIO m => GLenum -> m ()
glCoverageOperationNV = ffienumIOV glCoverageOperationNVFunPtr

glCoverageOperationNVFunPtr :: FunPtr (GLenum -> IO ())
glCoverageOperationNVFunPtr = unsafePerformIO (getProcAddress "glCoverageOperationNV")

{-# NOINLINE glCoverageOperationNVFunPtr #-}

pattern GL_COVERAGE_ALL_FRAGMENTS_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_ALL_FRAGMENTS_NV = 0x8ED5

pattern GL_COVERAGE_ATTACHMENT_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_ATTACHMENT_NV = 0x8ED2

pattern GL_COVERAGE_AUTOMATIC_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_AUTOMATIC_NV = 0x8ED7

pattern GL_COVERAGE_BUFFERS_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_BUFFERS_NV = 0x8ED3

pattern GL_COVERAGE_BUFFER_BIT_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_BUFFER_BIT_NV = 0x00008000

pattern GL_COVERAGE_COMPONENT4_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_COMPONENT4_NV = 0x8ED1

pattern GL_COVERAGE_COMPONENT_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_COMPONENT_NV = 0x8ED0

pattern GL_COVERAGE_EDGE_FRAGMENTS_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_EDGE_FRAGMENTS_NV = 0x8ED6

pattern GL_COVERAGE_SAMPLES_NV  :: (Eq a, Num a) => a

pattern GL_COVERAGE_SAMPLES_NV = 0x8ED4