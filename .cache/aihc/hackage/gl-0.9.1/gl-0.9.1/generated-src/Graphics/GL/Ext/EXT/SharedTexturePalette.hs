-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.SharedTexturePalette (
  -- * Extension Support
    gl_EXT_shared_texture_palette

  -- * GL_EXT_shared_texture_palette
  , pattern GL_SHARED_TEXTURE_PALETTE_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/shared_texture_palette.txt GL_EXT_shared_texture_palette> extension is available.

gl_EXT_shared_texture_palette :: Bool
gl_EXT_shared_texture_palette = member "GL_EXT_shared_texture_palette" extensions
{-# NOINLINE gl_EXT_shared_texture_palette #-}

pattern GL_SHARED_TEXTURE_PALETTE_EXT  :: (Eq a, Num a) => a

pattern GL_SHARED_TEXTURE_PALETTE_EXT = 0x81FB