-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.InstancedArrays (
  -- * Extension Support
    gl_EXT_instanced_arrays

  -- * GL_EXT_instanced_arrays
  , glDrawArraysInstancedEXT
  , glDrawElementsInstancedEXT
  , glVertexAttribDivisorEXT
  , pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_instanced_arrays.txt GL_EXT_instanced_arrays> extension is available.

gl_EXT_instanced_arrays :: Bool
gl_EXT_instanced_arrays = member "GL_EXT_instanced_arrays" extensions
{-# NOINLINE gl_EXT_instanced_arrays #-}

-- | Usage: @'glVertexAttribDivisorEXT' index divisor@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribDivisor'.


glVertexAttribDivisorEXT :: MonadIO m => GLuint -> GLuint -> m ()
glVertexAttribDivisorEXT = ffiuintuintIOV glVertexAttribDivisorEXTFunPtr

glVertexAttribDivisorEXTFunPtr :: FunPtr (GLuint -> GLuint -> IO ())
glVertexAttribDivisorEXTFunPtr = unsafePerformIO (getProcAddress "glVertexAttribDivisorEXT")

{-# NOINLINE glVertexAttribDivisorEXTFunPtr #-}

pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_EXT = 0x88FE