-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.Meminfo (
  -- * Extension Support
    gl_ATI_meminfo

  -- * GL_ATI_meminfo
  , pattern GL_RENDERBUFFER_FREE_MEMORY_ATI
  , pattern GL_TEXTURE_FREE_MEMORY_ATI
  , pattern GL_VBO_FREE_MEMORY_ATI
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/meminfo.txt GL_ATI_meminfo> extension is available.

gl_ATI_meminfo :: Bool
gl_ATI_meminfo = member "GL_ATI_meminfo" extensions
{-# NOINLINE gl_ATI_meminfo #-}

pattern GL_RENDERBUFFER_FREE_MEMORY_ATI  :: (Eq a, Num a) => a

pattern GL_RENDERBUFFER_FREE_MEMORY_ATI = 0x87FD

pattern GL_TEXTURE_FREE_MEMORY_ATI  :: (Eq a, Num a) => a

pattern GL_TEXTURE_FREE_MEMORY_ATI = 0x87FC

pattern GL_VBO_FREE_MEMORY_ATI  :: (Eq a, Num a) => a

pattern GL_VBO_FREE_MEMORY_ATI = 0x87FB