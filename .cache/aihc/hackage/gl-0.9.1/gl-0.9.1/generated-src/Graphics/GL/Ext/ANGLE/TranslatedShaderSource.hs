-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ANGLE.TranslatedShaderSource (
  -- * Extension Support
    gl_ANGLE_translated_shader_source

  -- * GL_ANGLE_translated_shader_source
  , glGetTranslatedShaderSourceANGLE
  , pattern GL_TRANSLATED_SHADER_SOURCE_LENGTH_ANGLE
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ANGLE/ANGLE_translated_shader_source.txt GL_ANGLE_translated_shader_source> extension is available.

gl_ANGLE_translated_shader_source :: Bool
gl_ANGLE_translated_shader_source = member "GL_ANGLE_translated_shader_source" extensions
{-# NOINLINE gl_ANGLE_translated_shader_source #-}

-- | Usage: @'glGetTranslatedShaderSourceANGLE' shader bufsize length source@
--
-- The length of @length@ should be @1@.


glGetTranslatedShaderSourceANGLE :: MonadIO m => GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
glGetTranslatedShaderSourceANGLE = ffiuintsizeiPtrsizeiPtrcharIOV glGetTranslatedShaderSourceANGLEFunPtr

glGetTranslatedShaderSourceANGLEFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
glGetTranslatedShaderSourceANGLEFunPtr = unsafePerformIO (getProcAddress "glGetTranslatedShaderSourceANGLE")

{-# NOINLINE glGetTranslatedShaderSourceANGLEFunPtr #-}

pattern GL_TRANSLATED_SHADER_SOURCE_LENGTH_ANGLE  :: (Eq a, Num a) => a

pattern GL_TRANSLATED_SHADER_SOURCE_LENGTH_ANGLE = 0x93A0