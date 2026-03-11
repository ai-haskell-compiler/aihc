-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.TextureSelect (
  -- * Extension Support
    gl_SGIS_texture_select

  -- * GL_SGIS_texture_select
  , pattern GL_DUAL_ALPHA12_SGIS
  , pattern GL_DUAL_ALPHA16_SGIS
  , pattern GL_DUAL_ALPHA4_SGIS
  , pattern GL_DUAL_ALPHA8_SGIS
  , pattern GL_DUAL_INTENSITY12_SGIS
  , pattern GL_DUAL_INTENSITY16_SGIS
  , pattern GL_DUAL_INTENSITY4_SGIS
  , pattern GL_DUAL_INTENSITY8_SGIS
  , pattern GL_DUAL_LUMINANCE12_SGIS
  , pattern GL_DUAL_LUMINANCE16_SGIS
  , pattern GL_DUAL_LUMINANCE4_SGIS
  , pattern GL_DUAL_LUMINANCE8_SGIS
  , pattern GL_DUAL_LUMINANCE_ALPHA4_SGIS
  , pattern GL_DUAL_LUMINANCE_ALPHA8_SGIS
  , pattern GL_DUAL_TEXTURE_SELECT_SGIS
  , pattern GL_QUAD_ALPHA4_SGIS
  , pattern GL_QUAD_ALPHA8_SGIS
  , pattern GL_QUAD_INTENSITY4_SGIS
  , pattern GL_QUAD_INTENSITY8_SGIS
  , pattern GL_QUAD_LUMINANCE4_SGIS
  , pattern GL_QUAD_LUMINANCE8_SGIS
  , pattern GL_QUAD_TEXTURE_SELECT_SGIS
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIS/texture_select.txt GL_SGIS_texture_select> extension is available.

gl_SGIS_texture_select :: Bool
gl_SGIS_texture_select = member "GL_SGIS_texture_select" extensions
{-# NOINLINE gl_SGIS_texture_select #-}

pattern GL_DUAL_ALPHA12_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_ALPHA12_SGIS = 0x8112

pattern GL_DUAL_ALPHA16_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_ALPHA16_SGIS = 0x8113

pattern GL_DUAL_ALPHA4_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_ALPHA4_SGIS = 0x8110

pattern GL_DUAL_ALPHA8_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_ALPHA8_SGIS = 0x8111

pattern GL_DUAL_INTENSITY12_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_INTENSITY12_SGIS = 0x811A

pattern GL_DUAL_INTENSITY16_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_INTENSITY16_SGIS = 0x811B

pattern GL_DUAL_INTENSITY4_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_INTENSITY4_SGIS = 0x8118

pattern GL_DUAL_INTENSITY8_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_INTENSITY8_SGIS = 0x8119

pattern GL_DUAL_LUMINANCE12_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_LUMINANCE12_SGIS = 0x8116

pattern GL_DUAL_LUMINANCE16_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_LUMINANCE16_SGIS = 0x8117

pattern GL_DUAL_LUMINANCE4_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_LUMINANCE4_SGIS = 0x8114

pattern GL_DUAL_LUMINANCE8_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_LUMINANCE8_SGIS = 0x8115

pattern GL_DUAL_LUMINANCE_ALPHA4_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_LUMINANCE_ALPHA4_SGIS = 0x811C

pattern GL_DUAL_LUMINANCE_ALPHA8_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_LUMINANCE_ALPHA8_SGIS = 0x811D

pattern GL_DUAL_TEXTURE_SELECT_SGIS  :: (Eq a, Num a) => a

pattern GL_DUAL_TEXTURE_SELECT_SGIS = 0x8124

pattern GL_QUAD_ALPHA4_SGIS  :: (Eq a, Num a) => a

pattern GL_QUAD_ALPHA4_SGIS = 0x811E

pattern GL_QUAD_ALPHA8_SGIS  :: (Eq a, Num a) => a

pattern GL_QUAD_ALPHA8_SGIS = 0x811F

pattern GL_QUAD_INTENSITY4_SGIS  :: (Eq a, Num a) => a

pattern GL_QUAD_INTENSITY4_SGIS = 0x8122

pattern GL_QUAD_INTENSITY8_SGIS  :: (Eq a, Num a) => a

pattern GL_QUAD_INTENSITY8_SGIS = 0x8123

pattern GL_QUAD_LUMINANCE4_SGIS  :: (Eq a, Num a) => a

pattern GL_QUAD_LUMINANCE4_SGIS = 0x8120

pattern GL_QUAD_LUMINANCE8_SGIS  :: (Eq a, Num a) => a

pattern GL_QUAD_LUMINANCE8_SGIS = 0x8121

pattern GL_QUAD_TEXTURE_SELECT_SGIS  :: (Eq a, Num a) => a

pattern GL_QUAD_TEXTURE_SELECT_SGIS = 0x8125