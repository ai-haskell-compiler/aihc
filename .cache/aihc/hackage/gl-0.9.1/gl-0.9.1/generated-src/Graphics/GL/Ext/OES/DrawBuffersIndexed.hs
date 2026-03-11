-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.DrawBuffersIndexed (
  -- * Extension Support
    gl_OES_draw_buffers_indexed

  -- * GL_OES_draw_buffers_indexed
  , glBlendEquationSeparateiOES
  , glBlendEquationiOES
  , glBlendFuncSeparateiOES
  , glBlendFunciOES
  , glColorMaskiOES
  , glDisableiOES
  , glEnableiOES
  , glIsEnablediOES
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

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_draw_buffers_indexed.txt GL_OES_draw_buffers_indexed> extension is available.

gl_OES_draw_buffers_indexed :: Bool
gl_OES_draw_buffers_indexed = member "GL_OES_draw_buffers_indexed" extensions
{-# NOINLINE gl_OES_draw_buffers_indexed #-}

-- | Usage: @'glBlendEquationSeparateiOES' buf modeRGB modeAlpha@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendEquationSeparatei'.


glBlendEquationSeparateiOES :: MonadIO m => GLuint -> GLenum -> GLenum -> m ()
glBlendEquationSeparateiOES = ffiuintenumenumIOV glBlendEquationSeparateiOESFunPtr

glBlendEquationSeparateiOESFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
glBlendEquationSeparateiOESFunPtr = unsafePerformIO (getProcAddress "glBlendEquationSeparateiOES")

{-# NOINLINE glBlendEquationSeparateiOESFunPtr #-}

-- | Usage: @'glBlendEquationiOES' buf mode@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendEquationi'.


glBlendEquationiOES :: MonadIO m => GLuint -> GLenum -> m ()
glBlendEquationiOES = ffiuintenumIOV glBlendEquationiOESFunPtr

glBlendEquationiOESFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glBlendEquationiOESFunPtr = unsafePerformIO (getProcAddress "glBlendEquationiOES")

{-# NOINLINE glBlendEquationiOESFunPtr #-}

-- | Usage: @'glBlendFuncSeparateiOES' buf srcRGB dstRGB srcAlpha dstAlpha@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendFuncSeparatei'.


glBlendFuncSeparateiOES :: MonadIO m => GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
glBlendFuncSeparateiOES = ffiuintenumenumenumenumIOV glBlendFuncSeparateiOESFunPtr

glBlendFuncSeparateiOESFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glBlendFuncSeparateiOESFunPtr = unsafePerformIO (getProcAddress "glBlendFuncSeparateiOES")

{-# NOINLINE glBlendFuncSeparateiOESFunPtr #-}

-- | Usage: @'glBlendFunciOES' buf src dst@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendFunci'.


glBlendFunciOES :: MonadIO m => GLuint -> GLenum -> GLenum -> m ()
glBlendFunciOES = ffiuintenumenumIOV glBlendFunciOESFunPtr

glBlendFunciOESFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
glBlendFunciOESFunPtr = unsafePerformIO (getProcAddress "glBlendFunciOES")

{-# NOINLINE glBlendFunciOESFunPtr #-}

-- | Usage: @'glColorMaskiOES' index r g b a@
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


glColorMaskiOES :: MonadIO m => GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> m ()
glColorMaskiOES = ffiuintbooleanbooleanbooleanbooleanIOV glColorMaskiOESFunPtr

glColorMaskiOESFunPtr :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
glColorMaskiOESFunPtr = unsafePerformIO (getProcAddress "glColorMaskiOES")

{-# NOINLINE glColorMaskiOESFunPtr #-}