-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TextureBorderClamp (
  -- * Extension Support
    gl_NV_texture_border_clamp

  -- * GL_NV_texture_border_clamp
  , pattern GL_CLAMP_TO_BORDER_NV
  , pattern GL_TEXTURE_BORDER_COLOR_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_texture_border_clamp.txt GL_NV_texture_border_clamp> extension is available.

gl_NV_texture_border_clamp :: Bool
gl_NV_texture_border_clamp = member "GL_NV_texture_border_clamp" extensions
{-# NOINLINE gl_NV_texture_border_clamp #-}

pattern GL_CLAMP_TO_BORDER_NV  :: (Eq a, Num a) => a

pattern GL_CLAMP_TO_BORDER_NV = 0x812D

pattern GL_TEXTURE_BORDER_COLOR_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BORDER_COLOR_NV = 0x1004