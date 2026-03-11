-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TexgenReflection (
  -- * Extension Support
    gl_NV_texgen_reflection

  -- * GL_NV_texgen_reflection
  , pattern GL_NORMAL_MAP_NV
  , pattern GL_REFLECTION_MAP_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/texgen_reflection.txt GL_NV_texgen_reflection> extension is available.

gl_NV_texgen_reflection :: Bool
gl_NV_texgen_reflection = member "GL_NV_texgen_reflection" extensions
{-# NOINLINE gl_NV_texgen_reflection #-}

pattern GL_NORMAL_MAP_NV  :: (Eq a, Num a) => a

pattern GL_NORMAL_MAP_NV = 0x8511

pattern GL_REFLECTION_MAP_NV  :: (Eq a, Num a) => a

pattern GL_REFLECTION_MAP_NV = 0x8512