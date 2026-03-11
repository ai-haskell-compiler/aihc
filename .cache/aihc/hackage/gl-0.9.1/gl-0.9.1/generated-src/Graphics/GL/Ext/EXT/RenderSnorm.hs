-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.RenderSnorm (
  -- * Extension Support
    gl_EXT_render_snorm

  -- * GL_EXT_render_snorm
  , pattern GL_BYTE
  , pattern GL_R16_SNORM_EXT
  , pattern GL_R8_SNORM
  , pattern GL_RG16_SNORM_EXT
  , pattern GL_RG8_SNORM
  , pattern GL_RGBA16_SNORM_EXT
  , pattern GL_RGBA8_SNORM
  , pattern GL_SHORT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the GL_EXT_render_snorm extension is available.

gl_EXT_render_snorm :: Bool
gl_EXT_render_snorm = member "GL_EXT_render_snorm" extensions
{-# NOINLINE gl_EXT_render_snorm #-}