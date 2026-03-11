-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.SampleShading (
  -- * Extension Support
    gl_ARB_sample_shading

  -- * GL_ARB_sample_shading
  , glMinSampleShadingARB
  , pattern GL_MIN_SAMPLE_SHADING_VALUE_ARB
  , pattern GL_SAMPLE_SHADING_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/sample_shading.txt GL_ARB_sample_shading> extension is available.

gl_ARB_sample_shading :: Bool
gl_ARB_sample_shading = member "GL_ARB_sample_shading" extensions
{-# NOINLINE gl_ARB_sample_shading #-}

-- | Usage: @'glMinSampleShadingARB' value@
--
-- The parameter @value@ is a @ColorF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMinSampleShading'.


glMinSampleShadingARB :: MonadIO m => GLfloat -> m ()
glMinSampleShadingARB = ffifloatIOV glMinSampleShadingARBFunPtr

glMinSampleShadingARBFunPtr :: FunPtr (GLfloat -> IO ())
glMinSampleShadingARBFunPtr = unsafePerformIO (getProcAddress "glMinSampleShadingARB")

{-# NOINLINE glMinSampleShadingARBFunPtr #-}

pattern GL_MIN_SAMPLE_SHADING_VALUE_ARB  :: (Eq a, Num a) => a

pattern GL_MIN_SAMPLE_SHADING_VALUE_ARB = 0x8C37

pattern GL_SAMPLE_SHADING_ARB  :: (Eq a, Num a) => a

pattern GL_SAMPLE_SHADING_ARB = 0x8C36