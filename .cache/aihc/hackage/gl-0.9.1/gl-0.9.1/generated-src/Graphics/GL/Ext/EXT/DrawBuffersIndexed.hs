-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.DrawBuffersIndexed (
  -- * Extension Support
    gl_EXT_draw_buffers_indexed

  -- * GL_EXT_draw_buffers_indexed
  , glBlendEquationSeparateiEXT
  , glBlendEquationiEXT
  , glBlendFuncSeparateiEXT
  , glBlendFunciEXT
  , glColorMaskiEXT
  , glDisableiEXT
  , glEnableiEXT
  , glIsEnablediEXT
  , pattern GL_BLEND
  , pattern GL_BLEND_DST_ALPHA
  , pattern GL_BLEND_DST_RGB
  , pattern GL_BLEND_EQUATION_ALPHA
  , pattern GL_BLEND_EQUATION_RGB
  , pattern GL_BLEND_SRC_ALPHA
  , pattern GL_BLEND_SRC_RGB
  , pattern GL_COLOR_WRITEMASK
  , pattern GL_CONSTANT_ALPHA
  , pattern GL_CONSTANT_COLOR
  , pattern GL_DST_ALPHA
  , pattern GL_DST_COLOR
  , pattern GL_FUNC_ADD
  , pattern GL_FUNC_REVERSE_SUBTRACT
  , pattern GL_FUNC_SUBTRACT
  , pattern GL_MAX
  , pattern GL_MIN
  , pattern GL_ONE
  , pattern GL_ONE_MINUS_CONSTANT_ALPHA
  , pattern GL_ONE_MINUS_CONSTANT_COLOR
  , pattern GL_ONE_MINUS_DST_ALPHA
  , pattern GL_ONE_MINUS_DST_COLOR
  , pattern GL_ONE_MINUS_SRC_ALPHA
  , pattern GL_ONE_MINUS_SRC_COLOR
  , pattern GL_SRC_ALPHA
  , pattern GL_SRC_ALPHA_SATURATE
  , pattern GL_SRC_COLOR
  , pattern GL_ZERO
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_draw_buffers_indexed.txt GL_EXT_draw_buffers_indexed> extension is available.

gl_EXT_draw_buffers_indexed :: Bool
gl_EXT_draw_buffers_indexed = member "GL_EXT_draw_buffers_indexed" extensions
{-# NOINLINE gl_EXT_draw_buffers_indexed #-}

-- | Usage: @'glBlendEquationSeparateiEXT' buf modeRGB modeAlpha@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendEquationSeparatei'.


glBlendEquationSeparateiEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> m ()
glBlendEquationSeparateiEXT = ffiuintenumenumIOV glBlendEquationSeparateiEXTFunPtr

glBlendEquationSeparateiEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
glBlendEquationSeparateiEXTFunPtr = unsafePerformIO (getProcAddress "glBlendEquationSeparateiEXT")

{-# NOINLINE glBlendEquationSeparateiEXTFunPtr #-}

-- | Usage: @'glBlendEquationiEXT' buf mode@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendEquationi'.


glBlendEquationiEXT :: MonadIO m => GLuint -> GLenum -> m ()
glBlendEquationiEXT = ffiuintenumIOV glBlendEquationiEXTFunPtr

glBlendEquationiEXTFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glBlendEquationiEXTFunPtr = unsafePerformIO (getProcAddress "glBlendEquationiEXT")

{-# NOINLINE glBlendEquationiEXTFunPtr #-}

-- | Usage: @'glBlendFuncSeparateiEXT' buf srcRGB dstRGB srcAlpha dstAlpha@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendFuncSeparatei'.


glBlendFuncSeparateiEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
glBlendFuncSeparateiEXT = ffiuintenumenumenumenumIOV glBlendFuncSeparateiEXTFunPtr

glBlendFuncSeparateiEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glBlendFuncSeparateiEXTFunPtr = unsafePerformIO (getProcAddress "glBlendFuncSeparateiEXT")

{-# NOINLINE glBlendFuncSeparateiEXTFunPtr #-}

-- | Usage: @'glBlendFunciEXT' buf src dst@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendFunci'.


glBlendFunciEXT :: MonadIO m => GLuint -> GLenum -> GLenum -> m ()
glBlendFunciEXT = ffiuintenumenumIOV glBlendFunciEXTFunPtr

glBlendFunciEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
glBlendFunciEXTFunPtr = unsafePerformIO (getProcAddress "glBlendFunciEXT")

{-# NOINLINE glBlendFunciEXTFunPtr #-}

-- | Usage: @'glColorMaskiEXT' index r g b a@
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


glColorMaskiEXT :: MonadIO m => GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> m ()
glColorMaskiEXT = ffiuintbooleanbooleanbooleanbooleanIOV glColorMaskiEXTFunPtr

glColorMaskiEXTFunPtr :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
glColorMaskiEXTFunPtr = unsafePerformIO (getProcAddress "glColorMaskiEXT")

{-# NOINLINE glColorMaskiEXTFunPtr #-}

-- | Usage: @'glDisableiEXT' target index@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDisablei'.


glDisableiEXT :: MonadIO m => GLenum -> GLuint -> m ()
glDisableiEXT = ffienumuintIOV glDisableiEXTFunPtr

glDisableiEXTFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glDisableiEXTFunPtr = unsafePerformIO (getProcAddress "glDisableiEXT")

{-# NOINLINE glDisableiEXTFunPtr #-}

-- | Usage: @'glEnableiEXT' target index@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glEnablei'.


glEnableiEXT :: MonadIO m => GLenum -> GLuint -> m ()
glEnableiEXT = ffienumuintIOV glEnableiEXTFunPtr

glEnableiEXTFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glEnableiEXTFunPtr = unsafePerformIO (getProcAddress "glEnableiEXT")

{-# NOINLINE glEnableiEXTFunPtr #-}

-- | Usage: @'glIsEnablediEXT' target index@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glIsEnabledi'.


glIsEnablediEXT :: MonadIO m => GLenum -> GLuint -> m GLboolean
glIsEnablediEXT = ffienumuintIOboolean glIsEnablediEXTFunPtr

glIsEnablediEXTFunPtr :: FunPtr (GLenum -> GLuint -> IO GLboolean)
glIsEnablediEXTFunPtr = unsafePerformIO (getProcAddress "glIsEnablediEXT")

{-# NOINLINE glIsEnablediEXTFunPtr #-}