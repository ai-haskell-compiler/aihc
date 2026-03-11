-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.PinnedMemory (
  -- * Extension Support
    gl_AMD_pinned_memory

  -- * GL_AMD_pinned_memory
  , pattern GL_EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/pinned_memory.txt GL_AMD_pinned_memory> extension is available.

gl_AMD_pinned_memory :: Bool
gl_AMD_pinned_memory = member "GL_AMD_pinned_memory" extensions
{-# NOINLINE gl_AMD_pinned_memory #-}

pattern GL_EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD  :: (Eq a, Num a) => a

pattern GL_EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD = 0x9160