-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.FloatPixels (
  -- * Extension Support
    gl_APPLE_float_pixels

  -- * GL_APPLE_float_pixels
  , pattern GL_ALPHA_FLOAT16_APPLE
  , pattern GL_ALPHA_FLOAT32_APPLE
  , pattern GL_COLOR_FLOAT_APPLE
  , pattern GL_HALF_APPLE
  , pattern GL_INTENSITY_FLOAT16_APPLE
  , pattern GL_INTENSITY_FLOAT32_APPLE
  , pattern GL_LUMINANCE_ALPHA_FLOAT16_APPLE
  , pattern GL_LUMINANCE_ALPHA_FLOAT32_APPLE
  , pattern GL_LUMINANCE_FLOAT16_APPLE
  , pattern GL_LUMINANCE_FLOAT32_APPLE
  , pattern GL_RGBA_FLOAT16_APPLE
  , pattern GL_RGBA_FLOAT32_APPLE
  , pattern GL_RGB_FLOAT16_APPLE
  , pattern GL_RGB_FLOAT32_APPLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/float_pixels.txt GL_APPLE_float_pixels> extension is available.

gl_APPLE_float_pixels :: Bool
gl_APPLE_float_pixels = member "GL_APPLE_float_pixels" extensions
{-# NOINLINE gl_APPLE_float_pixels #-}

pattern GL_ALPHA_FLOAT16_APPLE  :: (Eq a, Num a) => a

pattern GL_ALPHA_FLOAT16_APPLE = 0x881C

pattern GL_ALPHA_FLOAT32_APPLE  :: (Eq a, Num a) => a

pattern GL_ALPHA_FLOAT32_APPLE = 0x8816

pattern GL_COLOR_FLOAT_APPLE  :: (Eq a, Num a) => a

pattern GL_COLOR_FLOAT_APPLE = 0x8A0F

pattern GL_HALF_APPLE  :: (Eq a, Num a) => a

pattern GL_HALF_APPLE = 0x140B

pattern GL_INTENSITY_FLOAT16_APPLE  :: (Eq a, Num a) => a

pattern GL_INTENSITY_FLOAT16_APPLE = 0x881D

pattern GL_INTENSITY_FLOAT32_APPLE  :: (Eq a, Num a) => a

pattern GL_INTENSITY_FLOAT32_APPLE = 0x8817

pattern GL_LUMINANCE_ALPHA_FLOAT16_APPLE  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA_FLOAT16_APPLE = 0x881F

pattern GL_LUMINANCE_ALPHA_FLOAT32_APPLE  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA_FLOAT32_APPLE = 0x8819

pattern GL_LUMINANCE_FLOAT16_APPLE  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_FLOAT16_APPLE = 0x881E

pattern GL_LUMINANCE_FLOAT32_APPLE  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_FLOAT32_APPLE = 0x8818

pattern GL_RGBA_FLOAT16_APPLE  :: (Eq a, Num a) => a

pattern GL_RGBA_FLOAT16_APPLE = 0x881A

pattern GL_RGBA_FLOAT32_APPLE  :: (Eq a, Num a) => a

pattern GL_RGBA_FLOAT32_APPLE = 0x8814

pattern GL_RGB_FLOAT16_APPLE  :: (Eq a, Num a) => a

pattern GL_RGB_FLOAT16_APPLE = 0x881B

pattern GL_RGB_FLOAT32_APPLE  :: (Eq a, Num a) => a

pattern GL_RGB_FLOAT32_APPLE = 0x8815