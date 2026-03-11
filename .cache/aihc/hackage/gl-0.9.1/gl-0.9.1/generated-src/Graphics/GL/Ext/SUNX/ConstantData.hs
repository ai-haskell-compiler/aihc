-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SUNX.ConstantData (
  -- * Extension Support
    gl_SUNX_constant_data

  -- * GL_SUNX_constant_data
  , glFinishTextureSUNX
  , pattern GL_TEXTURE_CONSTANT_DATA_SUNX
  , pattern GL_UNPACK_CONSTANT_DATA_SUNX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SUNX/constant_data.txt GL_SUNX_constant_data> extension is available.

gl_SUNX_constant_data :: Bool
gl_SUNX_constant_data = member "GL_SUNX_constant_data" extensions
{-# NOINLINE gl_SUNX_constant_data #-}

-- | Usage: @'glFinishTextureSUNX'@


glFinishTextureSUNX :: MonadIO m => m ()
glFinishTextureSUNX = ffiIOV glFinishTextureSUNXFunPtr

glFinishTextureSUNXFunPtr :: FunPtr (IO ())
glFinishTextureSUNXFunPtr = unsafePerformIO (getProcAddress "glFinishTextureSUNX")

{-# NOINLINE glFinishTextureSUNXFunPtr #-}

pattern GL_TEXTURE_CONSTANT_DATA_SUNX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CONSTANT_DATA_SUNX = 0x81D6

pattern GL_UNPACK_CONSTANT_DATA_SUNX  :: (Eq a, Num a) => a

pattern GL_UNPACK_CONSTANT_DATA_SUNX = 0x81D5