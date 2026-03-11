-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.Texture (
  -- * Extension Support
    gl_EXT_texture

  -- * GL_EXT_texture
  , pattern GL_ALPHA12_EXT
  , pattern GL_ALPHA16_EXT
  , pattern GL_ALPHA4_EXT
  , pattern GL_ALPHA8_EXT
  , pattern GL_INTENSITY12_EXT
  , pattern GL_INTENSITY16_EXT
  , pattern GL_INTENSITY4_EXT
  , pattern GL_INTENSITY8_EXT
  , pattern GL_INTENSITY_EXT
  , pattern GL_LUMINANCE12_ALPHA12_EXT
  , pattern GL_LUMINANCE12_ALPHA4_EXT
  , pattern GL_LUMINANCE12_EXT
  , pattern GL_LUMINANCE16_ALPHA16_EXT
  , pattern GL_LUMINANCE16_EXT
  , pattern GL_LUMINANCE4_ALPHA4_EXT
  , pattern GL_LUMINANCE4_EXT
  , pattern GL_LUMINANCE6_ALPHA2_EXT
  , pattern GL_LUMINANCE8_ALPHA8_EXT
  , pattern GL_LUMINANCE8_EXT
  , pattern GL_PROXY_TEXTURE_1D_EXT
  , pattern GL_PROXY_TEXTURE_2D_EXT
  , pattern GL_REPLACE_EXT
  , pattern GL_RGB10_A2_EXT
  , pattern GL_RGB10_EXT
  , pattern GL_RGB12_EXT
  , pattern GL_RGB16_EXT
  , pattern GL_RGB2_EXT
  , pattern GL_RGB4_EXT
  , pattern GL_RGB5_A1_EXT
  , pattern GL_RGB5_EXT
  , pattern GL_RGB8_EXT
  , pattern GL_RGBA12_EXT
  , pattern GL_RGBA16_EXT
  , pattern GL_RGBA2_EXT
  , pattern GL_RGBA4_EXT
  , pattern GL_RGBA8_EXT
  , pattern GL_TEXTURE_ALPHA_SIZE_EXT
  , pattern GL_TEXTURE_BLUE_SIZE_EXT
  , pattern GL_TEXTURE_GREEN_SIZE_EXT
  , pattern GL_TEXTURE_INTENSITY_SIZE_EXT
  , pattern GL_TEXTURE_LUMINANCE_SIZE_EXT
  , pattern GL_TEXTURE_RED_SIZE_EXT
  , pattern GL_TEXTURE_TOO_LARGE_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture.txt GL_EXT_texture> extension is available.

gl_EXT_texture :: Bool
gl_EXT_texture = member "GL_EXT_texture" extensions
{-# NOINLINE gl_EXT_texture #-}

pattern GL_ALPHA12_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA12_EXT = 0x803D

pattern GL_ALPHA16_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA16_EXT = 0x803E

pattern GL_ALPHA4_EXT  :: (Eq a, Num a) => a

pattern GL_ALPHA4_EXT = 0x803B

pattern GL_INTENSITY12_EXT  :: (Eq a, Num a) => a

pattern GL_INTENSITY12_EXT = 0x804C

pattern GL_INTENSITY16_EXT  :: (Eq a, Num a) => a

pattern GL_INTENSITY16_EXT = 0x804D

pattern GL_INTENSITY4_EXT  :: (Eq a, Num a) => a

pattern GL_INTENSITY4_EXT = 0x804A

pattern GL_INTENSITY8_EXT  :: (Eq a, Num a) => a

pattern GL_INTENSITY8_EXT = 0x804B

pattern GL_INTENSITY_EXT  :: (Eq a, Num a) => a

pattern GL_INTENSITY_EXT = 0x8049

pattern GL_LUMINANCE12_ALPHA12_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE12_ALPHA12_EXT = 0x8047

pattern GL_LUMINANCE12_ALPHA4_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE12_ALPHA4_EXT = 0x8046

pattern GL_LUMINANCE12_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE12_EXT = 0x8041

pattern GL_LUMINANCE16_ALPHA16_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE16_ALPHA16_EXT = 0x8048

pattern GL_LUMINANCE16_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE16_EXT = 0x8042

pattern GL_LUMINANCE4_ALPHA4_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE4_ALPHA4_EXT = 0x8043

pattern GL_LUMINANCE4_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE4_EXT = 0x803F

pattern GL_LUMINANCE6_ALPHA2_EXT  :: (Eq a, Num a) => a

pattern GL_LUMINANCE6_ALPHA2_EXT = 0x8044

pattern GL_PROXY_TEXTURE_1D_EXT  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_1D_EXT = 0x8063

pattern GL_PROXY_TEXTURE_2D_EXT  :: (Eq a, Num a) => a

pattern GL_PROXY_TEXTURE_2D_EXT = 0x8064

pattern GL_REPLACE_EXT  :: (Eq a, Num a) => a

pattern GL_REPLACE_EXT = 0x8062

pattern GL_RGB12_EXT  :: (Eq a, Num a) => a

pattern GL_RGB12_EXT = 0x8053

pattern GL_RGB2_EXT  :: (Eq a, Num a) => a

pattern GL_RGB2_EXT = 0x804E

pattern GL_RGB4_EXT  :: (Eq a, Num a) => a

pattern GL_RGB4_EXT = 0x804F

pattern GL_RGB5_A1_EXT  :: (Eq a, Num a) => a

pattern GL_RGB5_A1_EXT = 0x8057

pattern GL_RGB5_EXT  :: (Eq a, Num a) => a

pattern GL_RGB5_EXT = 0x8050

pattern GL_RGB8_EXT  :: (Eq a, Num a) => a

pattern GL_RGB8_EXT = 0x8051

pattern GL_RGBA12_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA12_EXT = 0x805A

pattern GL_RGBA2_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA2_EXT = 0x8055

pattern GL_RGBA4_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA4_EXT = 0x8056

pattern GL_RGBA8_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA8_EXT = 0x8058

pattern GL_TEXTURE_ALPHA_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_ALPHA_SIZE_EXT = 0x805F

pattern GL_TEXTURE_BLUE_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BLUE_SIZE_EXT = 0x805E

pattern GL_TEXTURE_GREEN_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_GREEN_SIZE_EXT = 0x805D

pattern GL_TEXTURE_INTENSITY_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_INTENSITY_SIZE_EXT = 0x8061

pattern GL_TEXTURE_LUMINANCE_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_LUMINANCE_SIZE_EXT = 0x8060

pattern GL_TEXTURE_RED_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_RED_SIZE_EXT = 0x805C

pattern GL_TEXTURE_TOO_LARGE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_TOO_LARGE_EXT = 0x8065