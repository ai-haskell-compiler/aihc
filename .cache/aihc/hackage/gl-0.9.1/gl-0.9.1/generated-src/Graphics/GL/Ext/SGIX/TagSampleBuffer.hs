-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.TagSampleBuffer (
  -- * Extension Support
    gl_SGIX_tag_sample_buffer

  -- * GL_SGIX_tag_sample_buffer
  , glTagSampleBufferSGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/tag_sample_buffer.txt GL_SGIX_tag_sample_buffer> extension is available.

gl_SGIX_tag_sample_buffer :: Bool
gl_SGIX_tag_sample_buffer = member "GL_SGIX_tag_sample_buffer" extensions
{-# NOINLINE gl_SGIX_tag_sample_buffer #-}

-- | Usage: @'glTagSampleBufferSGIX'@


glTagSampleBufferSGIX :: MonadIO m => m ()
glTagSampleBufferSGIX = ffiIOV glTagSampleBufferSGIXFunPtr

glTagSampleBufferSGIXFunPtr :: FunPtr (IO ())
glTagSampleBufferSGIXFunPtr = unsafePerformIO (getProcAddress "glTagSampleBufferSGIX")

{-# NOINLINE glTagSampleBufferSGIXFunPtr #-}