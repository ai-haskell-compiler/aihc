-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.UniformBufferUnifiedMemory (
  -- * Extension Support
    gl_NV_uniform_buffer_unified_memory

  -- * GL_NV_uniform_buffer_unified_memory
  , pattern GL_UNIFORM_BUFFER_ADDRESS_NV
  , pattern GL_UNIFORM_BUFFER_LENGTH_NV
  , pattern GL_UNIFORM_BUFFER_UNIFIED_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/uniform_buffer_unified_memory.txt GL_NV_uniform_buffer_unified_memory> extension is available.

gl_NV_uniform_buffer_unified_memory :: Bool
gl_NV_uniform_buffer_unified_memory = member "GL_NV_uniform_buffer_unified_memory" extensions
{-# NOINLINE gl_NV_uniform_buffer_unified_memory #-}

pattern GL_UNIFORM_BUFFER_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_UNIFORM_BUFFER_ADDRESS_NV = 0x936F

pattern GL_UNIFORM_BUFFER_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_UNIFORM_BUFFER_LENGTH_NV = 0x9370

pattern GL_UNIFORM_BUFFER_UNIFIED_NV  :: (Eq a, Num a) => a

pattern GL_UNIFORM_BUFFER_UNIFIED_NV = 0x936E