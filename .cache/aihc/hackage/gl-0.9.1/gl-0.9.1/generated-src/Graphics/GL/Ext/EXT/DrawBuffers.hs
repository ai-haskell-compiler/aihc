-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.DrawBuffers (
  -- * Extension Support
    gl_EXT_draw_buffers

  -- * GL_EXT_draw_buffers
  , glDrawBuffersEXT
  , pattern GL_COLOR_ATTACHMENT0_EXT
  , pattern GL_COLOR_ATTACHMENT10_EXT
  , pattern GL_COLOR_ATTACHMENT11_EXT
  , pattern GL_COLOR_ATTACHMENT12_EXT
  , pattern GL_COLOR_ATTACHMENT13_EXT
  , pattern GL_COLOR_ATTACHMENT14_EXT
  , pattern GL_COLOR_ATTACHMENT15_EXT
  , pattern GL_COLOR_ATTACHMENT1_EXT
  , pattern GL_COLOR_ATTACHMENT2_EXT
  , pattern GL_COLOR_ATTACHMENT3_EXT
  , pattern GL_COLOR_ATTACHMENT4_EXT
  , pattern GL_COLOR_ATTACHMENT5_EXT
  , pattern GL_COLOR_ATTACHMENT6_EXT
  , pattern GL_COLOR_ATTACHMENT7_EXT
  , pattern GL_COLOR_ATTACHMENT8_EXT
  , pattern GL_COLOR_ATTACHMENT9_EXT
  , pattern GL_DRAW_BUFFER0_EXT
  , pattern GL_DRAW_BUFFER10_EXT
  , pattern GL_DRAW_BUFFER11_EXT
  , pattern GL_DRAW_BUFFER12_EXT
  , pattern GL_DRAW_BUFFER13_EXT
  , pattern GL_DRAW_BUFFER14_EXT
  , pattern GL_DRAW_BUFFER15_EXT
  , pattern GL_DRAW_BUFFER1_EXT
  , pattern GL_DRAW_BUFFER2_EXT
  , pattern GL_DRAW_BUFFER3_EXT
  , pattern GL_DRAW_BUFFER4_EXT
  , pattern GL_DRAW_BUFFER5_EXT
  , pattern GL_DRAW_BUFFER6_EXT
  , pattern GL_DRAW_BUFFER7_EXT
  , pattern GL_DRAW_BUFFER8_EXT
  , pattern GL_DRAW_BUFFER9_EXT
  , pattern GL_MAX_COLOR_ATTACHMENTS_EXT
  , pattern GL_MAX_DRAW_BUFFERS_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_draw_buffers.txt GL_EXT_draw_buffers> extension is available.

gl_EXT_draw_buffers :: Bool
gl_EXT_draw_buffers = member "GL_EXT_draw_buffers" extensions
{-# NOINLINE gl_EXT_draw_buffers #-}

-- | Usage: @'glDrawBuffersEXT' n bufs@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawBuffers'.


glDrawBuffersEXT :: MonadIO m => GLsizei -> Ptr GLenum -> m ()
glDrawBuffersEXT = ffisizeiPtrenumIOV glDrawBuffersEXTFunPtr

glDrawBuffersEXTFunPtr :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
glDrawBuffersEXTFunPtr = unsafePerformIO (getProcAddress "glDrawBuffersEXT")

{-# NOINLINE glDrawBuffersEXTFunPtr #-}

pattern GL_DRAW_BUFFER0_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER0_EXT = 0x8825

pattern GL_DRAW_BUFFER10_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER10_EXT = 0x882F

pattern GL_DRAW_BUFFER11_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER11_EXT = 0x8830

pattern GL_DRAW_BUFFER12_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER12_EXT = 0x8831

pattern GL_DRAW_BUFFER13_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER13_EXT = 0x8832

pattern GL_DRAW_BUFFER14_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER14_EXT = 0x8833

pattern GL_DRAW_BUFFER15_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER15_EXT = 0x8834

pattern GL_DRAW_BUFFER1_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER1_EXT = 0x8826

pattern GL_DRAW_BUFFER2_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER2_EXT = 0x8827

pattern GL_DRAW_BUFFER3_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER3_EXT = 0x8828

pattern GL_DRAW_BUFFER4_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER4_EXT = 0x8829

pattern GL_DRAW_BUFFER5_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER5_EXT = 0x882A

pattern GL_DRAW_BUFFER6_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER6_EXT = 0x882B

pattern GL_DRAW_BUFFER7_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER7_EXT = 0x882C

pattern GL_DRAW_BUFFER8_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER8_EXT = 0x882D

pattern GL_DRAW_BUFFER9_EXT  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER9_EXT = 0x882E

pattern GL_MAX_DRAW_BUFFERS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_DRAW_BUFFERS_EXT = 0x8824