-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.MESA.ResizeBuffers (
  -- * Extension Support
    gl_MESA_resize_buffers

  -- * GL_MESA_resize_buffers
  , glResizeBuffersMESA
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/MESA/resize_buffers.txt GL_MESA_resize_buffers> extension is available.

gl_MESA_resize_buffers :: Bool
gl_MESA_resize_buffers = member "GL_MESA_resize_buffers" extensions
{-# NOINLINE gl_MESA_resize_buffers #-}

-- | Usage: @'glResizeBuffersMESA'@


glResizeBuffersMESA :: MonadIO m => m ()
glResizeBuffersMESA = ffiIOV glResizeBuffersMESAFunPtr

glResizeBuffersMESAFunPtr :: FunPtr (IO ())
glResizeBuffersMESAFunPtr = unsafePerformIO (getProcAddress "glResizeBuffersMESA")

{-# NOINLINE glResizeBuffersMESAFunPtr #-}