-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.DrawBuffers (
  -- * Extension Support
    gl_NV_draw_buffers

  -- * GL_NV_draw_buffers
  , glDrawBuffersNV
  , pattern GL_COLOR_ATTACHMENT0_NV
  , pattern GL_COLOR_ATTACHMENT10_NV
  , pattern GL_COLOR_ATTACHMENT11_NV
  , pattern GL_COLOR_ATTACHMENT12_NV
  , pattern GL_COLOR_ATTACHMENT13_NV
  , pattern GL_COLOR_ATTACHMENT14_NV
  , pattern GL_COLOR_ATTACHMENT15_NV
  , pattern GL_COLOR_ATTACHMENT1_NV
  , pattern GL_COLOR_ATTACHMENT2_NV
  , pattern GL_COLOR_ATTACHMENT3_NV
  , pattern GL_COLOR_ATTACHMENT4_NV
  , pattern GL_COLOR_ATTACHMENT5_NV
  , pattern GL_COLOR_ATTACHMENT6_NV
  , pattern GL_COLOR_ATTACHMENT7_NV
  , pattern GL_COLOR_ATTACHMENT8_NV
  , pattern GL_COLOR_ATTACHMENT9_NV
  , pattern GL_DRAW_BUFFER0_NV
  , pattern GL_DRAW_BUFFER10_NV
  , pattern GL_DRAW_BUFFER11_NV
  , pattern GL_DRAW_BUFFER12_NV
  , pattern GL_DRAW_BUFFER13_NV
  , pattern GL_DRAW_BUFFER14_NV
  , pattern GL_DRAW_BUFFER15_NV
  , pattern GL_DRAW_BUFFER1_NV
  , pattern GL_DRAW_BUFFER2_NV
  , pattern GL_DRAW_BUFFER3_NV
  , pattern GL_DRAW_BUFFER4_NV
  , pattern GL_DRAW_BUFFER5_NV
  , pattern GL_DRAW_BUFFER6_NV
  , pattern GL_DRAW_BUFFER7_NV
  , pattern GL_DRAW_BUFFER8_NV
  , pattern GL_DRAW_BUFFER9_NV
  , pattern GL_MAX_DRAW_BUFFERS_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_draw_buffers.txt GL_NV_draw_buffers> extension is available.

gl_NV_draw_buffers :: Bool
gl_NV_draw_buffers = member "GL_NV_draw_buffers" extensions
{-# NOINLINE gl_NV_draw_buffers #-}

-- | Usage: @'glDrawBuffersNV' n bufs@
--
-- The length of @bufs@ should be @n@.


glDrawBuffersNV :: MonadIO m => GLsizei -> Ptr GLenum -> m ()
glDrawBuffersNV = ffisizeiPtrenumIOV glDrawBuffersNVFunPtr

glDrawBuffersNVFunPtr :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
glDrawBuffersNVFunPtr = unsafePerformIO (getProcAddress "glDrawBuffersNV")

{-# NOINLINE glDrawBuffersNVFunPtr #-}

pattern GL_DRAW_BUFFER0_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER0_NV = 0x8825

pattern GL_DRAW_BUFFER10_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER10_NV = 0x882F

pattern GL_DRAW_BUFFER11_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER11_NV = 0x8830

pattern GL_DRAW_BUFFER12_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER12_NV = 0x8831

pattern GL_DRAW_BUFFER13_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER13_NV = 0x8832

pattern GL_DRAW_BUFFER14_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER14_NV = 0x8833

pattern GL_DRAW_BUFFER15_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER15_NV = 0x8834

pattern GL_DRAW_BUFFER1_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER1_NV = 0x8826

pattern GL_DRAW_BUFFER2_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER2_NV = 0x8827

pattern GL_DRAW_BUFFER3_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER3_NV = 0x8828

pattern GL_DRAW_BUFFER4_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER4_NV = 0x8829

pattern GL_DRAW_BUFFER5_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER5_NV = 0x882A

pattern GL_DRAW_BUFFER6_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER6_NV = 0x882B

pattern GL_DRAW_BUFFER7_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER7_NV = 0x882C

pattern GL_DRAW_BUFFER8_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER8_NV = 0x882D

pattern GL_DRAW_BUFFER9_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER9_NV = 0x882E

pattern GL_MAX_DRAW_BUFFERS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_DRAW_BUFFERS_NV = 0x8824