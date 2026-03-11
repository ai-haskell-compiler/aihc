-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.DrawBuffers (
  -- * Extension Support
    gl_ARB_draw_buffers

  -- * GL_ARB_draw_buffers
  , glDrawBuffersARB
  , pattern GL_DRAW_BUFFER0_ARB
  , pattern GL_DRAW_BUFFER10_ARB
  , pattern GL_DRAW_BUFFER11_ARB
  , pattern GL_DRAW_BUFFER12_ARB
  , pattern GL_DRAW_BUFFER13_ARB
  , pattern GL_DRAW_BUFFER14_ARB
  , pattern GL_DRAW_BUFFER15_ARB
  , pattern GL_DRAW_BUFFER1_ARB
  , pattern GL_DRAW_BUFFER2_ARB
  , pattern GL_DRAW_BUFFER3_ARB
  , pattern GL_DRAW_BUFFER4_ARB
  , pattern GL_DRAW_BUFFER5_ARB
  , pattern GL_DRAW_BUFFER6_ARB
  , pattern GL_DRAW_BUFFER7_ARB
  , pattern GL_DRAW_BUFFER8_ARB
  , pattern GL_DRAW_BUFFER9_ARB
  , pattern GL_MAX_DRAW_BUFFERS_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/draw_buffers.txt GL_ARB_draw_buffers> extension is available.

gl_ARB_draw_buffers :: Bool
gl_ARB_draw_buffers = member "GL_ARB_draw_buffers" extensions
{-# NOINLINE gl_ARB_draw_buffers #-}

-- | Usage: @'glDrawBuffersARB' n bufs@
--
-- The parameter @bufs@ is a @DrawBufferModeATI@.
--
-- The length of @bufs@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawBuffers'.


glDrawBuffersARB :: MonadIO m => GLsizei -> Ptr GLenum -> m ()
glDrawBuffersARB = ffisizeiPtrenumIOV glDrawBuffersARBFunPtr

glDrawBuffersARBFunPtr :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
glDrawBuffersARBFunPtr = unsafePerformIO (getProcAddress "glDrawBuffersARB")

{-# NOINLINE glDrawBuffersARBFunPtr #-}

pattern GL_DRAW_BUFFER0_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER0_ARB = 0x8825

pattern GL_DRAW_BUFFER10_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER10_ARB = 0x882F

pattern GL_DRAW_BUFFER11_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER11_ARB = 0x8830

pattern GL_DRAW_BUFFER12_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER12_ARB = 0x8831

pattern GL_DRAW_BUFFER13_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER13_ARB = 0x8832

pattern GL_DRAW_BUFFER14_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER14_ARB = 0x8833

pattern GL_DRAW_BUFFER15_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER15_ARB = 0x8834

pattern GL_DRAW_BUFFER1_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER1_ARB = 0x8826

pattern GL_DRAW_BUFFER2_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER2_ARB = 0x8827

pattern GL_DRAW_BUFFER3_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER3_ARB = 0x8828

pattern GL_DRAW_BUFFER4_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER4_ARB = 0x8829

pattern GL_DRAW_BUFFER5_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER5_ARB = 0x882A

pattern GL_DRAW_BUFFER6_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER6_ARB = 0x882B

pattern GL_DRAW_BUFFER7_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER7_ARB = 0x882C

pattern GL_DRAW_BUFFER8_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER8_ARB = 0x882D

pattern GL_DRAW_BUFFER9_ARB  :: (Eq a, Num a) => a

pattern GL_DRAW_BUFFER9_ARB = 0x882E

pattern GL_MAX_DRAW_BUFFERS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_DRAW_BUFFERS_ARB = 0x8824