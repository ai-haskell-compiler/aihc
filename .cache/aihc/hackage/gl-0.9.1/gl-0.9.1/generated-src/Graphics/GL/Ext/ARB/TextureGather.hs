-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureGather (
  -- * Extension Support
    gl_ARB_texture_gather

  -- * GL_ARB_texture_gather
  , pattern GL_MAX_PROGRAM_TEXTURE_GATHER_COMPONENTS_ARB
  , pattern GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_ARB
  , pattern GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_gather.txt GL_ARB_texture_gather> extension is available.

gl_ARB_texture_gather :: Bool
gl_ARB_texture_gather = member "GL_ARB_texture_gather" extensions
{-# NOINLINE gl_ARB_texture_gather #-}

pattern GL_MAX_PROGRAM_TEXTURE_GATHER_COMPONENTS_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_TEXTURE_GATHER_COMPONENTS_ARB = 0x8F9F

pattern GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_ARB = 0x8E5F

pattern GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_ARB  :: (Eq a, Num a) => a

pattern GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_ARB = 0x8E5E