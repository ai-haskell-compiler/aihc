-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.DrawBuffersBlend (
  -- * Extension Support
    gl_AMD_draw_buffers_blend

  -- * GL_AMD_draw_buffers_blend
  , glBlendEquationIndexedAMD
  , glBlendEquationSeparateIndexedAMD
  , glBlendFuncIndexedAMD
  , glBlendFuncSeparateIndexedAMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/draw_buffers_blend.txt GL_AMD_draw_buffers_blend> extension is available.

gl_AMD_draw_buffers_blend :: Bool
gl_AMD_draw_buffers_blend = member "GL_AMD_draw_buffers_blend" extensions
{-# NOINLINE gl_AMD_draw_buffers_blend #-}

-- | Usage: @'glBlendEquationIndexedAMD' buf mode@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendEquationi'.


glBlendEquationIndexedAMD :: MonadIO m => GLuint -> GLenum -> m ()
glBlendEquationIndexedAMD = ffiuintenumIOV glBlendEquationIndexedAMDFunPtr

glBlendEquationIndexedAMDFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glBlendEquationIndexedAMDFunPtr = unsafePerformIO (getProcAddress "glBlendEquationIndexedAMD")

{-# NOINLINE glBlendEquationIndexedAMDFunPtr #-}

-- | Usage: @'glBlendEquationSeparateIndexedAMD' buf modeRGB modeAlpha@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendEquationSeparatei'.


glBlendEquationSeparateIndexedAMD :: MonadIO m => GLuint -> GLenum -> GLenum -> m ()
glBlendEquationSeparateIndexedAMD = ffiuintenumenumIOV glBlendEquationSeparateIndexedAMDFunPtr

glBlendEquationSeparateIndexedAMDFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
glBlendEquationSeparateIndexedAMDFunPtr = unsafePerformIO (getProcAddress "glBlendEquationSeparateIndexedAMD")

{-# NOINLINE glBlendEquationSeparateIndexedAMDFunPtr #-}

-- | Usage: @'glBlendFuncIndexedAMD' buf src dst@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendFunci'.


glBlendFuncIndexedAMD :: MonadIO m => GLuint -> GLenum -> GLenum -> m ()
glBlendFuncIndexedAMD = ffiuintenumenumIOV glBlendFuncIndexedAMDFunPtr

glBlendFuncIndexedAMDFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
glBlendFuncIndexedAMDFunPtr = unsafePerformIO (getProcAddress "glBlendFuncIndexedAMD")

{-# NOINLINE glBlendFuncIndexedAMDFunPtr #-}

-- | Usage: @'glBlendFuncSeparateIndexedAMD' buf srcRGB dstRGB srcAlpha dstAlpha@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendFuncSeparatei'.


glBlendFuncSeparateIndexedAMD :: MonadIO m => GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
glBlendFuncSeparateIndexedAMD = ffiuintenumenumenumenumIOV glBlendFuncSeparateIndexedAMDFunPtr

glBlendFuncSeparateIndexedAMDFunPtr :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glBlendFuncSeparateIndexedAMDFunPtr = unsafePerformIO (getProcAddress "glBlendFuncSeparateIndexedAMD")

{-# NOINLINE glBlendFuncSeparateIndexedAMDFunPtr #-}