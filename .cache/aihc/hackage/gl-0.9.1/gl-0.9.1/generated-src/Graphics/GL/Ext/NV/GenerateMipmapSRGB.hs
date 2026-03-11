-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.GenerateMipmapSRGB (
  -- * Extension Support
    gl_NV_generate_mipmap_sRGB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_generate_mipmap_sRGB.txt GL_NV_generate_mipmap_sRGB> extension is available.

gl_NV_generate_mipmap_sRGB :: Bool
gl_NV_generate_mipmap_sRGB = member "GL_NV_generate_mipmap_sRGB" extensions
{-# NOINLINE gl_NV_generate_mipmap_sRGB #-}