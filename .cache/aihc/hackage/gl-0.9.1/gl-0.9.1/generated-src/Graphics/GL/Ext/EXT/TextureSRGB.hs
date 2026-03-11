-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureSRGB (
  -- * Extension Support
    gl_EXT_texture_sRGB

  -- * GL_EXT_texture_sRGB
  , pattern GL_COMPRESSED_SLUMINANCE_ALPHA_EXT
  , pattern GL_COMPRESSED_SLUMINANCE_EXT
  , pattern GL_COMPRESSED_SRGB_ALPHA_EXT
  , pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT
  , pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT
  , pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT
  , pattern GL_COMPRESSED_SRGB_EXT
  , pattern GL_COMPRESSED_SRGB_S3TC_DXT1_EXT
  , pattern GL_SLUMINANCE8_ALPHA8_EXT
  , pattern GL_SLUMINANCE8_EXT
  , pattern GL_SLUMINANCE_ALPHA_EXT
  , pattern GL_SLUMINANCE_EXT
  , pattern GL_SRGB8_ALPHA8_EXT
  , pattern GL_SRGB8_EXT
  , pattern GL_SRGB_ALPHA_EXT
  , pattern GL_SRGB_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_sRGB.txt GL_EXT_texture_sRGB> extension is available.

gl_EXT_texture_sRGB :: Bool
gl_EXT_texture_sRGB = member "GL_EXT_texture_sRGB" extensions
{-# NOINLINE gl_EXT_texture_sRGB #-}

pattern GL_COMPRESSED_SLUMINANCE_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SLUMINANCE_ALPHA_EXT = 0x8C4B

pattern GL_COMPRESSED_SLUMINANCE_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SLUMINANCE_EXT = 0x8C4A

pattern GL_COMPRESSED_SRGB_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_EXT = 0x8C49

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT = 0x8C4D

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT = 0x8C4E

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT = 0x8C4F

pattern GL_COMPRESSED_SRGB_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_EXT = 0x8C48

pattern GL_COMPRESSED_SRGB_S3TC_DXT1_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_S3TC_DXT1_EXT = 0x8C4C

pattern GL_SLUMINANCE8_ALPHA8_EXT  :: (Eq a, Num a) => a

pattern GL_SLUMINANCE8_ALPHA8_EXT = 0x8C45

pattern GL_SLUMINANCE8_EXT  :: (Eq a, Num a) => a

pattern GL_SLUMINANCE8_EXT = 0x8C47

pattern GL_SLUMINANCE_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_SLUMINANCE_ALPHA_EXT = 0x8C44

pattern GL_SLUMINANCE_EXT  :: (Eq a, Num a) => a

pattern GL_SLUMINANCE_EXT = 0x8C46

pattern GL_SRGB8_EXT  :: (Eq a, Num a) => a

pattern GL_SRGB8_EXT = 0x8C41