-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.SampleShading (
  -- * Extension Support
    gl_OES_sample_shading

  -- * GL_OES_sample_shading
  , glMinSampleShadingOES
  , pattern GL_MIN_SAMPLE_SHADING_VALUE_OES
  , pattern GL_SAMPLE_SHADING_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_sample_shading.txt GL_OES_sample_shading> extension is available.

gl_OES_sample_shading :: Bool
gl_OES_sample_shading = member "GL_OES_sample_shading" extensions
{-# NOINLINE gl_OES_sample_shading #-}

-- | Usage: @'glMinSampleShadingOES' value@
--
-- The parameter @value@ is a @ColorF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glMinSampleShading'.


glMinSampleShadingOES :: MonadIO m => GLfloat -> m ()
glMinSampleShadingOES = ffifloatIOV glMinSampleShadingOESFunPtr

glMinSampleShadingOESFunPtr :: FunPtr (GLfloat -> IO ())
glMinSampleShadingOESFunPtr = unsafePerformIO (getProcAddress "glMinSampleShadingOES")

{-# NOINLINE glMinSampleShadingOESFunPtr #-}

pattern GL_MIN_SAMPLE_SHADING_VALUE_OES  :: (Eq a, Num a) => a

pattern GL_MIN_SAMPLE_SHADING_VALUE_OES = 0x8C37

pattern GL_SAMPLE_SHADING_OES  :: (Eq a, Num a) => a

pattern GL_SAMPLE_SHADING_OES = 0x8C36