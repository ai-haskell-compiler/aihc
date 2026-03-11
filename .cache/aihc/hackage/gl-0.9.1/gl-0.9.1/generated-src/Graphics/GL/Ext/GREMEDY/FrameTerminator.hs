-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.GREMEDY.FrameTerminator (
  -- * Extension Support
    gl_GREMEDY_frame_terminator

  -- * GL_GREMEDY_frame_terminator
  , glFrameTerminatorGREMEDY
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/GREMEDY/frame_terminator.txt GL_GREMEDY_frame_terminator> extension is available.

gl_GREMEDY_frame_terminator :: Bool
gl_GREMEDY_frame_terminator = member "GL_GREMEDY_frame_terminator" extensions
{-# NOINLINE gl_GREMEDY_frame_terminator #-}

-- | Usage: @'glFrameTerminatorGREMEDY'@


glFrameTerminatorGREMEDY :: MonadIO m => m ()
glFrameTerminatorGREMEDY = ffiIOV glFrameTerminatorGREMEDYFunPtr

glFrameTerminatorGREMEDYFunPtr :: FunPtr (IO ())
glFrameTerminatorGREMEDYFunPtr = unsafePerformIO (getProcAddress "glFrameTerminatorGREMEDY")

{-# NOINLINE glFrameTerminatorGREMEDYFunPtr #-}