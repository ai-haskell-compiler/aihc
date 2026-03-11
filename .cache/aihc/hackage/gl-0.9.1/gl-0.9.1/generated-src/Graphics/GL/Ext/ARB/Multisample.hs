-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.Multisample (
  -- * Extension Support
    gl_ARB_multisample

  -- * GL_ARB_multisample
  , glSampleCoverageARB
  , pattern GL_MULTISAMPLE_ARB
  , pattern GL_MULTISAMPLE_BIT_ARB
  , pattern GL_SAMPLES_ARB
  , pattern GL_SAMPLE_ALPHA_TO_COVERAGE_ARB
  , pattern GL_SAMPLE_ALPHA_TO_ONE_ARB
  , pattern GL_SAMPLE_BUFFERS_ARB
  , pattern GL_SAMPLE_COVERAGE_ARB
  , pattern GL_SAMPLE_COVERAGE_INVERT_ARB
  , pattern GL_SAMPLE_COVERAGE_VALUE_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/multisample.txt GL_ARB_multisample> extension is available.

gl_ARB_multisample :: Bool
gl_ARB_multisample = member "GL_ARB_multisample" extensions
{-# NOINLINE gl_ARB_multisample #-}

-- | Usage: @'glSampleCoverageARB' value invert@
--
-- The parameter @invert@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSampleCoverage'.


glSampleCoverageARB :: MonadIO m => GLfloat -> GLboolean -> m ()
glSampleCoverageARB = ffifloatbooleanIOV glSampleCoverageARBFunPtr

glSampleCoverageARBFunPtr :: FunPtr (GLfloat -> GLboolean -> IO ())
glSampleCoverageARBFunPtr = unsafePerformIO (getProcAddress "glSampleCoverageARB")

{-# NOINLINE glSampleCoverageARBFunPtr #-}

pattern GL_MULTISAMPLE_ARB  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_ARB = 0x809D

pattern GL_MULTISAMPLE_BIT_ARB  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_BIT_ARB = 0x20000000

pattern GL_SAMPLE_ALPHA_TO_COVERAGE_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLE_ALPHA_TO_COVERAGE_ARB = 0x809E

pattern GL_SAMPLE_ALPHA_TO_ONE_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLE_ALPHA_TO_ONE_ARB = 0x809F

pattern GL_SAMPLE_BUFFERS_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLE_BUFFERS_ARB = 0x80A8

pattern GL_SAMPLE_COVERAGE_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLE_COVERAGE_ARB = 0x80A0

pattern GL_SAMPLE_COVERAGE_INVERT_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLE_COVERAGE_INVERT_ARB = 0x80AB

pattern GL_SAMPLE_COVERAGE_VALUE_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLE_COVERAGE_VALUE_ARB = 0x80AA