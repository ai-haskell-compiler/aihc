-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.CompressedPalettedTexture (
  -- * Extension Support
    gl_OES_compressed_paletted_texture

  -- * GL_OES_compressed_paletted_texture
  , pattern GL_PALETTE4_R5_G6_B5_OES
  , pattern GL_PALETTE4_RGB5_A1_OES
  , pattern GL_PALETTE4_RGB8_OES
  , pattern GL_PALETTE4_RGBA4_OES
  , pattern GL_PALETTE4_RGBA8_OES
  , pattern GL_PALETTE8_R5_G6_B5_OES
  , pattern GL_PALETTE8_RGB5_A1_OES
  , pattern GL_PALETTE8_RGB8_OES
  , pattern GL_PALETTE8_RGBA4_OES
  , pattern GL_PALETTE8_RGBA8_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_compressed_paletted_texture.txt GL_OES_compressed_paletted_texture> extension is available.

gl_OES_compressed_paletted_texture :: Bool
gl_OES_compressed_paletted_texture = member "GL_OES_compressed_paletted_texture" extensions
{-# NOINLINE gl_OES_compressed_paletted_texture #-}

pattern GL_PALETTE4_R5_G6_B5_OES  :: (Eq a, Num a) => a

pattern GL_PALETTE4_R5_G6_B5_OES = 0x8B92

pattern GL_PALETTE4_RGB5_A1_OES  :: (Eq a, Num a) => a

pattern GL_PALETTE4_RGB5_A1_OES = 0x8B94

pattern GL_PALETTE4_RGB8_OES  :: (Eq a, Num a) => a

pattern GL_PALETTE4_RGB8_OES = 0x8B90

pattern GL_PALETTE4_RGBA4_OES  :: (Eq a, Num a) => a

pattern GL_PALETTE4_RGBA4_OES = 0x8B93

pattern GL_PALETTE4_RGBA8_OES  :: (Eq a, Num a) => a

pattern GL_PALETTE4_RGBA8_OES = 0x8B91

pattern GL_PALETTE8_R5_G6_B5_OES  :: (Eq a, Num a) => a

pattern GL_PALETTE8_R5_G6_B5_OES = 0x8B97

pattern GL_PALETTE8_RGB5_A1_OES  :: (Eq a, Num a) => a

pattern GL_PALETTE8_RGB5_A1_OES = 0x8B99

pattern GL_PALETTE8_RGB8_OES  :: (Eq a, Num a) => a

pattern GL_PALETTE8_RGB8_OES = 0x8B95

pattern GL_PALETTE8_RGBA4_OES  :: (Eq a, Num a) => a

pattern GL_PALETTE8_RGBA4_OES = 0x8B98

pattern GL_PALETTE8_RGBA8_OES  :: (Eq a, Num a) => a

pattern GL_PALETTE8_RGBA8_OES = 0x8B96