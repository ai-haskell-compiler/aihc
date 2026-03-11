-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.TextureFloat (
  -- * Extension Support
    gl_ATI_texture_float

  -- * GL_ATI_texture_float
  , pattern GL_ALPHA_FLOAT16_ATI
  , pattern GL_ALPHA_FLOAT32_ATI
  , pattern GL_INTENSITY_FLOAT16_ATI
  , pattern GL_INTENSITY_FLOAT32_ATI
  , pattern GL_LUMINANCE_ALPHA_FLOAT16_ATI
  , pattern GL_LUMINANCE_ALPHA_FLOAT32_ATI
  , pattern GL_LUMINANCE_FLOAT16_ATI
  , pattern GL_LUMINANCE_FLOAT32_ATI
  , pattern GL_RGBA_FLOAT16_ATI
  , pattern GL_RGBA_FLOAT32_ATI
  , pattern GL_RGB_FLOAT16_ATI
  , pattern GL_RGB_FLOAT32_ATI
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/texture_float.txt GL_ATI_texture_float> extension is available.

gl_ATI_texture_float :: Bool
gl_ATI_texture_float = member "GL_ATI_texture_float" extensions
{-# NOINLINE gl_ATI_texture_float #-}

pattern GL_ALPHA_FLOAT16_ATI  :: (Eq a, Num a) => a

pattern GL_ALPHA_FLOAT16_ATI = 0x881C

pattern GL_ALPHA_FLOAT32_ATI  :: (Eq a, Num a) => a

pattern GL_ALPHA_FLOAT32_ATI = 0x8816

pattern GL_INTENSITY_FLOAT16_ATI  :: (Eq a, Num a) => a

pattern GL_INTENSITY_FLOAT16_ATI = 0x881D

pattern GL_INTENSITY_FLOAT32_ATI  :: (Eq a, Num a) => a

pattern GL_INTENSITY_FLOAT32_ATI = 0x8817

pattern GL_LUMINANCE_ALPHA_FLOAT16_ATI  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA_FLOAT16_ATI = 0x881F

pattern GL_LUMINANCE_ALPHA_FLOAT32_ATI  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA_FLOAT32_ATI = 0x8819

pattern GL_LUMINANCE_FLOAT16_ATI  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_FLOAT16_ATI = 0x881E

pattern GL_LUMINANCE_FLOAT32_ATI  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_FLOAT32_ATI = 0x8818

pattern GL_RGBA_FLOAT16_ATI  :: (Eq a, Num a) => a

pattern GL_RGBA_FLOAT16_ATI = 0x881A

pattern GL_RGBA_FLOAT32_ATI  :: (Eq a, Num a) => a

pattern GL_RGBA_FLOAT32_ATI = 0x8814

pattern GL_RGB_FLOAT16_ATI  :: (Eq a, Num a) => a

pattern GL_RGB_FLOAT16_ATI = 0x881B

pattern GL_RGB_FLOAT32_ATI  :: (Eq a, Num a) => a

pattern GL_RGB_FLOAT32_ATI = 0x8815