-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ColorBufferFloat (
  -- * Extension Support
    gl_ARB_color_buffer_float

  -- * GL_ARB_color_buffer_float
  , glClampColorARB
  , pattern GL_CLAMP_FRAGMENT_COLOR_ARB
  , pattern GL_CLAMP_READ_COLOR_ARB
  , pattern GL_CLAMP_VERTEX_COLOR_ARB
  , pattern GL_FIXED_ONLY_ARB
  , pattern GL_RGBA_FLOAT_MODE_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/color_buffer_float.txt GL_ARB_color_buffer_float> extension is available.

gl_ARB_color_buffer_float :: Bool
gl_ARB_color_buffer_float = member "GL_ARB_color_buffer_float" extensions
{-# NOINLINE gl_ARB_color_buffer_float #-}

-- | Usage: @'glClampColorARB' target clamp@
--
-- The parameter @target@ is a @ClampColorTargetARB@.
--
-- The parameter @clamp@ is a @ClampColorModeARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glClampColor'.


glClampColorARB :: MonadIO m => GLenum -> GLenum -> m ()
glClampColorARB = ffienumenumIOV glClampColorARBFunPtr

glClampColorARBFunPtr :: FunPtr (GLenum -> GLenum -> IO ())
glClampColorARBFunPtr = unsafePerformIO (getProcAddress "glClampColorARB")

{-# NOINLINE glClampColorARBFunPtr #-}

pattern GL_CLAMP_FRAGMENT_COLOR_ARB  :: (Eq a, Num a) => a

pattern GL_CLAMP_FRAGMENT_COLOR_ARB = 0x891B

pattern GL_CLAMP_READ_COLOR_ARB  :: (Eq a, Num a) => a

pattern GL_CLAMP_READ_COLOR_ARB = 0x891C

pattern GL_CLAMP_VERTEX_COLOR_ARB  :: (Eq a, Num a) => a

pattern GL_CLAMP_VERTEX_COLOR_ARB = 0x891A

pattern GL_FIXED_ONLY_ARB  :: (Eq a, Num a) => a

pattern GL_FIXED_ONLY_ARB = 0x891D

pattern GL_RGBA_FLOAT_MODE_ARB  :: (Eq a, Num a) => a

pattern GL_RGBA_FLOAT_MODE_ARB = 0x8820