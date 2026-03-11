-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TextureExpandNormal (
  -- * Extension Support
    gl_NV_texture_expand_normal

  -- * GL_NV_texture_expand_normal
  , pattern GL_TEXTURE_UNSIGNED_REMAP_MODE_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/texture_expand_normal.txt GL_NV_texture_expand_normal> extension is available.

gl_NV_texture_expand_normal :: Bool
gl_NV_texture_expand_normal = member "GL_NV_texture_expand_normal" extensions
{-# NOINLINE gl_NV_texture_expand_normal #-}

pattern GL_TEXTURE_UNSIGNED_REMAP_MODE_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_UNSIGNED_REMAP_MODE_NV = 0x888F