-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureSRGBDecode (
  -- * Extension Support
    gl_EXT_texture_sRGB_decode

  -- * GL_EXT_texture_sRGB_decode
  , pattern GL_DECODE_EXT
  , pattern GL_SKIP_DECODE_EXT
  , pattern GL_TEXTURE_SRGB_DECODE_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_sRGB_decode.txt GL_EXT_texture_sRGB_decode> extension is available.

gl_EXT_texture_sRGB_decode :: Bool
gl_EXT_texture_sRGB_decode = member "GL_EXT_texture_sRGB_decode" extensions
{-# NOINLINE gl_EXT_texture_sRGB_decode #-}

pattern GL_DECODE_EXT  :: (Eq a, Num a) => a

pattern GL_DECODE_EXT = 0x8A49

pattern GL_SKIP_DECODE_EXT  :: (Eq a, Num a) => a

pattern GL_SKIP_DECODE_EXT = 0x8A4A

pattern GL_TEXTURE_SRGB_DECODE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_SRGB_DECODE_EXT = 0x8A48