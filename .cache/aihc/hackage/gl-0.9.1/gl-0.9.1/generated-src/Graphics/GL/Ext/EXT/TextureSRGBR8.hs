-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureSRGBR8 (
  -- * Extension Support
    gl_EXT_texture_sRGB_R8

  -- * GL_EXT_texture_sRGB_R8
  , pattern GL_SR8_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_sRGB_R8.txt GL_EXT_texture_sRGB_R8> extension is available.

gl_EXT_texture_sRGB_R8 :: Bool
gl_EXT_texture_sRGB_R8 = member "GL_EXT_texture_sRGB_R8" extensions
{-# NOINLINE gl_EXT_texture_sRGB_R8 #-}

pattern GL_SR8_EXT  :: (Eq a, Num a) => a

pattern GL_SR8_EXT = 0x8FBD