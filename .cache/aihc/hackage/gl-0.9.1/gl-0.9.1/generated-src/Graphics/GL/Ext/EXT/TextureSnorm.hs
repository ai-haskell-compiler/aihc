-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureSnorm (
  -- * Extension Support
    gl_EXT_texture_snorm

  -- * GL_EXT_texture_snorm
  , pattern GL_ALPHA16_SNORM
  , pattern GL_ALPHA8_SNORM
  , pattern GL_ALPHA_SNORM
  , pattern GL_INTENSITY16_SNORM
  , pattern GL_INTENSITY8_SNORM
  , pattern GL_INTENSITY_SNORM
  , pattern GL_LUMINANCE16_ALPHA16_SNORM
  , pattern GL_LUMINANCE16_SNORM
  , pattern GL_LUMINANCE8_ALPHA8_SNORM
  , pattern GL_LUMINANCE8_SNORM
  , pattern GL_LUMINANCE_ALPHA_SNORM
  , pattern GL_LUMINANCE_SNORM
  , pattern GL_R16_SNORM
  , pattern GL_R8_SNORM
  , pattern GL_RED_SNORM
  , pattern GL_RG16_SNORM
  , pattern GL_RG8_SNORM
  , pattern GL_RGB16_SNORM
  , pattern GL_RGB8_SNORM
  , pattern GL_RGBA16_SNORM
  , pattern GL_RGBA8_SNORM
  , pattern GL_RGBA_SNORM
  , pattern GL_RGB_SNORM
  , pattern GL_RG_SNORM
  , pattern GL_SIGNED_NORMALIZED
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_snorm.txt GL_EXT_texture_snorm> extension is available.

gl_EXT_texture_snorm :: Bool
gl_EXT_texture_snorm = member "GL_EXT_texture_snorm" extensions
{-# NOINLINE gl_EXT_texture_snorm #-}

pattern GL_ALPHA16_SNORM  :: (Eq a, Num a) => a

pattern GL_ALPHA16_SNORM = 0x9018

pattern GL_ALPHA8_SNORM  :: (Eq a, Num a) => a

pattern GL_ALPHA8_SNORM = 0x9014

pattern GL_ALPHA_SNORM  :: (Eq a, Num a) => a

pattern GL_ALPHA_SNORM = 0x9010

pattern GL_INTENSITY16_SNORM  :: (Eq a, Num a) => a

pattern GL_INTENSITY16_SNORM = 0x901B

pattern GL_INTENSITY8_SNORM  :: (Eq a, Num a) => a

pattern GL_INTENSITY8_SNORM = 0x9017

pattern GL_INTENSITY_SNORM  :: (Eq a, Num a) => a

pattern GL_INTENSITY_SNORM = 0x9013

pattern GL_LUMINANCE16_ALPHA16_SNORM  :: (Eq a, Num a) => a

pattern GL_LUMINANCE16_ALPHA16_SNORM = 0x901A

pattern GL_LUMINANCE16_SNORM  :: (Eq a, Num a) => a

pattern GL_LUMINANCE16_SNORM = 0x9019

pattern GL_LUMINANCE8_ALPHA8_SNORM  :: (Eq a, Num a) => a

pattern GL_LUMINANCE8_ALPHA8_SNORM = 0x9016

pattern GL_LUMINANCE8_SNORM  :: (Eq a, Num a) => a

pattern GL_LUMINANCE8_SNORM = 0x9015

pattern GL_LUMINANCE_ALPHA_SNORM  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_ALPHA_SNORM = 0x9012

pattern GL_LUMINANCE_SNORM  :: (Eq a, Num a) => a

pattern GL_LUMINANCE_SNORM = 0x9011

pattern GL_RED_SNORM  :: (Eq a, Num a) => a

pattern GL_RED_SNORM = 0x8F90

pattern GL_RGBA_SNORM  :: (Eq a, Num a) => a

pattern GL_RGBA_SNORM = 0x8F93

pattern GL_RGB_SNORM  :: (Eq a, Num a) => a

pattern GL_RGB_SNORM = 0x8F92

pattern GL_RG_SNORM  :: (Eq a, Num a) => a

pattern GL_RG_SNORM = 0x8F91