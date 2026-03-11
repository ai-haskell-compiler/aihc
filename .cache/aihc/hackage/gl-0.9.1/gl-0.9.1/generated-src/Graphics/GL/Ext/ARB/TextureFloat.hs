-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureFloat (
  -- * Extension Support
    gl_ARB_texture_float

  -- * GL_ARB_texture_float
  , pattern GL_ALPHA16F_ARB
  , pattern GL_ALPHA32F_ARB
  , pattern GL_INTENSITY16F_ARB
  , pattern GL_INTENSITY32F_ARB
  , pattern GL_LUMINANCE16F_ARB
  , pattern GL_LUMINANCE32F_ARB
  , pattern GL_LUMINANCE_ALPHA16F_ARB
  , pattern GL_LUMINANCE_ALPHA32F_ARB
  , pattern GL_RGB16F_ARB
  , pattern GL_RGB32F_ARB
  , pattern GL_RGBA16F_ARB
  , pattern GL_RGBA32F_ARB
  , pattern GL_TEXTURE_ALPHA_TYPE_ARB
  , pattern GL_TEXTURE_BLUE_TYPE_ARB
  , pattern GL_TEXTURE_DEPTH_TYPE_ARB
  , pattern GL_TEXTURE_GREEN_TYPE_ARB
  , pattern GL_TEXTURE_INTENSITY_TYPE_ARB
  , pattern GL_TEXTURE_LUMINANCE_TYPE_ARB
  , pattern GL_TEXTURE_RED_TYPE_ARB
  , pattern GL_UNSIGNED_NORMALIZED_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_float.txt GL_ARB_texture_float> extension is available.

gl_ARB_texture_float :: Bool
gl_ARB_texture_float = member "GL_ARB_texture_float" extensions
{-# NOINLINE gl_ARB_texture_float #-}

pattern GL_ALPHA16F_ARB  :: (Eq a, Num a) => a

pattern GL_ALPHA16F_ARB = 0x881C

pattern GL_ALPHA32F_ARB  :: (Eq a, Num a) => a

pattern GL_ALPHA32F_ARB = 0x8816

pattern GL_INTENSITY16F_ARB  :: (Eq a, Num a) => a

pattern GL_INTENSITY16F_ARB = 0x881D

pattern GL_INTENSITY32F_ARB  :: (Eq a, Num a) => a

pattern GL_INTENSITY32F_ARB = 0x8817

pattern GL_LUMINANCE16F_ARB  :: (Eq a, Num a) => a

pattern GL_LUMINANCE16F_ARB = 0x881E

pattern GL_LUMINANCE32F_ARB  :: (Eq a, Num a) => a

pattern GL_LUMINANCE32F_ARB = 0x8818

pattern GL_LUMINANCE_ALPHA16F_ARB  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA16F_ARB = 0x881F

pattern GL_LUMINANCE_ALPHA32F_ARB  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA32F_ARB = 0x8819

pattern GL_RGB16F_ARB  :: (Eq a, Num a) => a

pattern GL_RGB16F_ARB = 0x881B

pattern GL_RGB32F_ARB  :: (Eq a, Num a) => a

pattern GL_RGB32F_ARB = 0x8815

pattern GL_RGBA16F_ARB  :: (Eq a, Num a) => a

pattern GL_RGBA16F_ARB = 0x881A

pattern GL_RGBA32F_ARB  :: (Eq a, Num a) => a

pattern GL_RGBA32F_ARB = 0x8814

pattern GL_TEXTURE_ALPHA_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_ALPHA_TYPE_ARB = 0x8C13

pattern GL_TEXTURE_BLUE_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BLUE_TYPE_ARB = 0x8C12

pattern GL_TEXTURE_DEPTH_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_DEPTH_TYPE_ARB = 0x8C16

pattern GL_TEXTURE_GREEN_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_GREEN_TYPE_ARB = 0x8C11

pattern GL_TEXTURE_INTENSITY_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_INTENSITY_TYPE_ARB = 0x8C15

pattern GL_TEXTURE_LUMINANCE_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_LUMINANCE_TYPE_ARB = 0x8C14

pattern GL_TEXTURE_RED_TYPE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_RED_TYPE_ARB = 0x8C10

pattern GL_UNSIGNED_NORMALIZED_ARB  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_NORMALIZED_ARB = 0x8C17