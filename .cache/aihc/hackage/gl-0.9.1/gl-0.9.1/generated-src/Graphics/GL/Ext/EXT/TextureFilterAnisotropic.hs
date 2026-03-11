-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureFilterAnisotropic (
  -- * Extension Support
    gl_EXT_texture_filter_anisotropic

  -- * GL_EXT_texture_filter_anisotropic
  , pattern GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT
  , pattern GL_TEXTURE_MAX_ANISOTROPY_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_filter_anisotropic.txt GL_EXT_texture_filter_anisotropic> extension is available.

gl_EXT_texture_filter_anisotropic :: Bool
gl_EXT_texture_filter_anisotropic = member "GL_EXT_texture_filter_anisotropic" extensions
{-# NOINLINE gl_EXT_texture_filter_anisotropic #-}

pattern GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = 0x84FF

pattern GL_TEXTURE_MAX_ANISOTROPY_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MAX_ANISOTROPY_EXT = 0x84FE