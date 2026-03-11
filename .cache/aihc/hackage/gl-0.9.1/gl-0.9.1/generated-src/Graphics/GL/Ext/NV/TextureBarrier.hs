-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TextureBarrier (
  -- * Extension Support
    gl_NV_texture_barrier

  -- * GL_NV_texture_barrier
  , glTextureBarrierNV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/texture_barrier.txt GL_NV_texture_barrier> extension is available.

gl_NV_texture_barrier :: Bool
gl_NV_texture_barrier = member "GL_NV_texture_barrier" extensions
{-# NOINLINE gl_NV_texture_barrier #-}

-- | Usage: @'glTextureBarrierNV'@


glTextureBarrierNV :: MonadIO m => m ()
glTextureBarrierNV = ffiIOV glTextureBarrierNVFunPtr

glTextureBarrierNVFunPtr :: FunPtr (IO ())
glTextureBarrierNVFunPtr = unsafePerformIO (getProcAddress "glTextureBarrierNV")

{-# NOINLINE glTextureBarrierNVFunPtr #-}