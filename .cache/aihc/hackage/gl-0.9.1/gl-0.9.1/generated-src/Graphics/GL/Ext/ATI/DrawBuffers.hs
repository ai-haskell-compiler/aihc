-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.DrawBuffers (
  -- * Extension Support
    gl_ATI_draw_buffers

  -- * GL_ATI_draw_buffers
  , glDrawBuffersATI
  , pattern GL_DRAW_BUFFER0_ATI
  , pattern GL_DRAW_BUFFER10_ATI
  , pattern GL_DRAW_BUFFER11_ATI
  , pattern GL_DRAW_BUFFER12_ATI
  , pattern GL_DRAW_BUFFER13_ATI
  , pattern GL_DRAW_BUFFER14_ATI
  , pattern GL_DRAW_BUFFER15_ATI
  , pattern GL_DRAW_BUFFER1_ATI
  , pattern GL_DRAW_BUFFER2_ATI
  , pattern GL_DRAW_BUFFER3_ATI
  , pattern GL_DRAW_BUFFER4_ATI
  , pattern GL_DRAW_BUFFER5_ATI
  , pattern GL_DRAW_BUFFER6_ATI
  , pattern GL_DRAW_BUFFER7_ATI
  , pattern GL_DRAW_BUFFER8_ATI
  , pattern GL_DRAW_BUFFER9_ATI
  , pattern GL_MAX_DRAW_BUFFERS_ATI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/draw_buffers.txt GL_ATI_draw_buffers> extension is available.

gl_ATI_draw_buffers :: Bool
gl_ATI_draw_buffers = member "GL_ATI_draw_buffers" extensions
{-# NOINLINE gl_ATI_draw_buffers #-}

-- | Usage: @'glDrawBuffersATI' n bufs@
--
-- The parameter @bufs@ is a @DrawBufferModeATI@.
--
-- The length of @bufs@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawBuffers'.


glDrawBuffersATI :: MonadIO m => GLsizei -> Ptr GLenum -> m ()
glDrawBuffersATI = ffisizeiPtrenumIOV glDrawBuffersATIFunPtr

glDrawBuffersATIFunPtr :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
glDrawBuffersATIFunPtr = unsafePerformIO (getProcAddress "glDrawBuffersATI")

{-# NOINLINE glDrawBuffersATIFunPtr #-}

pattern GL_DRAW_BUFFER0_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER0_ATI = 0x8825

pattern GL_DRAW_BUFFER10_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER10_ATI = 0x882F

pattern GL_DRAW_BUFFER11_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER11_ATI = 0x8830

pattern GL_DRAW_BUFFER12_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER12_ATI = 0x8831

pattern GL_DRAW_BUFFER13_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER13_ATI = 0x8832

pattern GL_DRAW_BUFFER14_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER14_ATI = 0x8833

pattern GL_DRAW_BUFFER15_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER15_ATI = 0x8834

pattern GL_DRAW_BUFFER1_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER1_ATI = 0x8826

pattern GL_DRAW_BUFFER2_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER2_ATI = 0x8827

pattern GL_DRAW_BUFFER3_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER3_ATI = 0x8828

pattern GL_DRAW_BUFFER4_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER4_ATI = 0x8829

pattern GL_DRAW_BUFFER5_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER5_ATI = 0x882A

pattern GL_DRAW_BUFFER6_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER6_ATI = 0x882B

pattern GL_DRAW_BUFFER7_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER7_ATI = 0x882C

pattern GL_DRAW_BUFFER8_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER8_ATI = 0x882D

pattern GL_DRAW_BUFFER9_ATI  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER9_ATI = 0x882E

pattern GL_MAX_DRAW_BUFFERS_ATI  :: (Eq a, Num a) => a

pattern GL_MAX_DRAW_BUFFERS_ATI = 0x8824