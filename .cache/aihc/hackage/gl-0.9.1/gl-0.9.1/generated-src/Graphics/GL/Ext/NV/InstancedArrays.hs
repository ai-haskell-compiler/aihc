-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.InstancedArrays (
  -- * Extension Support
    gl_NV_instanced_arrays

  -- * GL_NV_instanced_arrays
  , glVertexAttribDivisorNV
  , pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_instanced_arrays.txt GL_NV_instanced_arrays> extension is available.

gl_NV_instanced_arrays :: Bool
gl_NV_instanced_arrays = member "GL_NV_instanced_arrays" extensions
{-# NOINLINE gl_NV_instanced_arrays #-}

-- | Usage: @'glVertexAttribDivisorNV' index divisor@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribDivisor'.


glVertexAttribDivisorNV :: MonadIO m => GLuint -> GLuint -> m ()
glVertexAttribDivisorNV = ffiuintuintIOV glVertexAttribDivisorNVFunPtr

glVertexAttribDivisorNVFunPtr :: FunPtr (GLuint -> GLuint -> IO ())
glVertexAttribDivisorNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribDivisorNV")

{-# NOINLINE glVertexAttribDivisorNVFunPtr #-}

pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_NV = 0x88FE