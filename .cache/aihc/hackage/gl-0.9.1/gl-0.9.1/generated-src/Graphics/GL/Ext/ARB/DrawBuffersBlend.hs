-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.DrawBuffersBlend (
  -- * Extension Support
    gl_ARB_draw_buffers_blend

  -- * GL_ARB_draw_buffers_blend
  , glBlendEquationSeparateiARB
  , glBlendEquationiARB
  , glBlendFuncSeparateiARB
  , glBlendFunciARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/draw_buffers_blend.txt GL_ARB_draw_buffers_blend> extension is available.

gl_ARB_draw_buffers_blend :: Bool
gl_ARB_draw_buffers_blend = member "GL_ARB_draw_buffers_blend" extensions
{-# NOINLINE gl_ARB_draw_buffers_blend #-}

-- | Usage: @'glBlendEquationSeparateiARB' buf modeRGB modeAlpha@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendEquationSeparatei'.


glBlendEquationSeparateiARB :: MonadIO m => GLuint -> GLenum -> GLenum -> m ()
glBlendEquationSeparateiARB = ffiuintenumenumIOV glBlendEquationSeparateiARBFunPtr

glBlendEquationSeparateiARBFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
glBlendEquationSeparateiARBFunPtr = unsafePerformIO (getProcAddress "glBlendEquationSeparateiARB")

{-# NOINLINE glBlendEquationSeparateiARBFunPtr #-}

-- | Usage: @'glBlendEquationiARB' buf mode@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendEquationi'.


glBlendEquationiARB :: MonadIO m => GLuint -> GLenum -> m ()
glBlendEquationiARB = ffiuintenumIOV glBlendEquationiARBFunPtr

glBlendEquationiARBFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glBlendEquationiARBFunPtr = unsafePerformIO (getProcAddress "glBlendEquationiARB")

{-# NOINLINE glBlendEquationiARBFunPtr #-}

-- | Usage: @'glBlendFuncSeparateiARB' buf srcRGB dstRGB srcAlpha dstAlpha@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendFuncSeparatei'.


glBlendFuncSeparateiARB :: MonadIO m => GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
glBlendFuncSeparateiARB = ffiuintenumenumenumenumIOV glBlendFuncSeparateiARBFunPtr

glBlendFuncSeparateiARBFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glBlendFuncSeparateiARBFunPtr = unsafePerformIO (getProcAddress "glBlendFuncSeparateiARB")

{-# NOINLINE glBlendFuncSeparateiARBFunPtr #-}

-- | Usage: @'glBlendFunciARB' buf src dst@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendFunci'.


glBlendFunciARB :: MonadIO m => GLuint -> GLenum -> GLenum -> m ()
glBlendFunciARB = ffiuintenumenumIOV glBlendFunciARBFunPtr

glBlendFunciARBFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
glBlendFunciARBFunPtr = unsafePerformIO (getProcAddress "glBlendFunciARB")

{-# NOINLINE glBlendFunciARBFunPtr #-}