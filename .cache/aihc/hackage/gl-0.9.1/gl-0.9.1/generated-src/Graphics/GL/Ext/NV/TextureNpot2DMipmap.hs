-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TextureNpot2DMipmap (
  -- * Extension Support
    gl_NV_texture_npot_2D_mipmap
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_texture_npot_2D_mipmap.txt GL_NV_texture_npot_2D_mipmap> extension is available.

gl_NV_texture_npot_2D_mipmap :: Bool
gl_NV_texture_npot_2D_mipmap = member "GL_NV_texture_npot_2D_mipmap" extensions
{-# NOINLINE gl_NV_texture_npot_2D_mipmap #-}