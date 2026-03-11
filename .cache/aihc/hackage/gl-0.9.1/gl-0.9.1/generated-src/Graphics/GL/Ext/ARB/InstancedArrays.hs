-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.InstancedArrays (
  -- * Extension Support
    gl_ARB_instanced_arrays

  -- * GL_ARB_instanced_arrays
  , glVertexAttribDivisorARB
  , pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/instanced_arrays.txt GL_ARB_instanced_arrays> extension is available.

gl_ARB_instanced_arrays :: Bool
gl_ARB_instanced_arrays = member "GL_ARB_instanced_arrays" extensions
{-# NOINLINE gl_ARB_instanced_arrays #-}

-- | Usage: @'glVertexAttribDivisorARB' index divisor@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glVertexAttribDivisor'.


glVertexAttribDivisorARB :: MonadIO m => GLuint -> GLuint -> m ()
glVertexAttribDivisorARB = ffiuintuintIOV glVertexAttribDivisorARBFunPtr

glVertexAttribDivisorARBFunPtr :: FunPtr (GLuint -> GLuint -> IO ())
glVertexAttribDivisorARBFunPtr = unsafePerformIO (getProcAddress "glVertexAttribDivisorARB")

{-# NOINLINE glVertexAttribDivisorARBFunPtr #-}

pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB = 0x88FE