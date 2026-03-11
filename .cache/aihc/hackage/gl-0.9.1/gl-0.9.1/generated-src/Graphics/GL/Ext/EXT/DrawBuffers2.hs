-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.DrawBuffers2 (
  -- * Extension Support
    gl_EXT_draw_buffers2

  -- * GL_EXT_draw_buffers2
  , glColorMaskIndexedEXT
  , glDisableIndexedEXT
  , glEnableIndexedEXT
  , glGetBooleanIndexedvEXT
  , glGetIntegerIndexedvEXT
  , glIsEnabledIndexedEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/draw_buffers2.txt GL_EXT_draw_buffers2> extension is available.

gl_EXT_draw_buffers2 :: Bool
gl_EXT_draw_buffers2 = member "GL_EXT_draw_buffers2" extensions
{-# NOINLINE gl_EXT_draw_buffers2 #-}

-- | Usage: @'glColorMaskIndexedEXT' index r g b a@
--
-- The parameter @r@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @g@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @b@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @a@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glColorMaski'.


glColorMaskIndexedEXT :: MonadIO m => GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> m ()
glColorMaskIndexedEXT = ffiuintbooleanbooleanbooleanbooleanIOV glColorMaskIndexedEXTFunPtr

glColorMaskIndexedEXTFunPtr :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
glColorMaskIndexedEXTFunPtr = unsafePerformIO (getProcAddress "glColorMaskIndexedEXT")

{-# NOINLINE glColorMaskIndexedEXTFunPtr #-}