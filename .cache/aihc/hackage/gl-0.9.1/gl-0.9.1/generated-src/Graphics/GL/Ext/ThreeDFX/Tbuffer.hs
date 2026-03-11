-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ThreeDFX.Tbuffer (
  -- * Extension Support
    gl_3DFX_tbuffer

  -- * GL_3DFX_tbuffer
  , glTbufferMask3DFX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/3DFX/tbuffer.txt GL_3DFX_tbuffer> extension is available.

gl_3DFX_tbuffer :: Bool
gl_3DFX_tbuffer = member "GL_3DFX_tbuffer" extensions
{-# NOINLINE gl_3DFX_tbuffer #-}

-- | Usage: @'glTbufferMask3DFX' mask@


glTbufferMask3DFX :: MonadIO m => GLuint -> m ()
glTbufferMask3DFX = ffiuintIOV glTbufferMask3DFXFunPtr

glTbufferMask3DFXFunPtr :: FunPtr (GLuint -> IO ())
glTbufferMask3DFXFunPtr = unsafePerformIO (getProcAddress "glTbufferMask3DFX")

{-# NOINLINE glTbufferMask3DFXFunPtr #-}