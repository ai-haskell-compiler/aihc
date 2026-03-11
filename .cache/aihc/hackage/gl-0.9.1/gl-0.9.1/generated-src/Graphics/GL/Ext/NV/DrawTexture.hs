-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.DrawTexture (
  -- * Extension Support
    gl_NV_draw_texture

  -- * GL_NV_draw_texture
  , glDrawTextureNV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_draw_texture.txt GL_NV_draw_texture> extension is available.

gl_NV_draw_texture :: Bool
gl_NV_draw_texture = member "GL_NV_draw_texture" extensions
{-# NOINLINE gl_NV_draw_texture #-}

-- | Usage: @'glDrawTextureNV' texture sampler x0 y0 x1 y1 z s0 t0 s1 t1@


glDrawTextureNV :: MonadIO m => GLuint -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glDrawTextureNV = ffiuintuintfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV glDrawTextureNVFunPtr

glDrawTextureNVFunPtr :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glDrawTextureNVFunPtr = unsafePerformIO (getProcAddress "glDrawTextureNV")

{-# NOINLINE glDrawTextureNVFunPtr #-}