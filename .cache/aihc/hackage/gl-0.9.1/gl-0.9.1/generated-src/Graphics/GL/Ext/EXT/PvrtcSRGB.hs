-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PvrtcSRGB (
  -- * Extension Support
    gl_EXT_pvrtc_sRGB

  -- * GL_EXT_pvrtc_sRGB
  , pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV1_EXT
  , pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV2_IMG
  , pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV1_EXT
  , pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV2_IMG
  , pattern GL_COMPRESSED_SRGB_PVRTC_2BPPV1_EXT
  , pattern GL_COMPRESSED_SRGB_PVRTC_4BPPV1_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_pvrtc_sRGB.txt GL_EXT_pvrtc_sRGB> extension is available.

gl_EXT_pvrtc_sRGB :: Bool
gl_EXT_pvrtc_sRGB = member "GL_EXT_pvrtc_sRGB" extensions
{-# NOINLINE gl_EXT_pvrtc_sRGB #-}

pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV1_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV1_EXT = 0x8A56

pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV2_IMG  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV2_IMG = 0x93F0

pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV1_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV1_EXT = 0x8A57

pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV2_IMG  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV2_IMG = 0x93F1

pattern GL_COMPRESSED_SRGB_PVRTC_2BPPV1_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_PVRTC_2BPPV1_EXT = 0x8A54

pattern GL_COMPRESSED_SRGB_PVRTC_4BPPV1_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SRGB_PVRTC_4BPPV1_EXT = 0x8A55