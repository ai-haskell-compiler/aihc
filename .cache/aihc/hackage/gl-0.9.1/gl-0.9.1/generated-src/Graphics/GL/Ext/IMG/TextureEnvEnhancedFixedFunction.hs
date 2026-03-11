-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IMG.TextureEnvEnhancedFixedFunction (
  -- * Extension Support
    gl_IMG_texture_env_enhanced_fixed_function

  -- * GL_IMG_texture_env_enhanced_fixed_function
  , pattern GL_ADD_BLEND_IMG
  , pattern GL_DOT3_RGBA_IMG
  , pattern GL_FACTOR_ALPHA_MODULATE_IMG
  , pattern GL_FRAGMENT_ALPHA_MODULATE_IMG
  , pattern GL_MODULATE_COLOR_IMG
  , pattern GL_RECIP_ADD_SIGNED_ALPHA_IMG
  , pattern GL_TEXTURE_ALPHA_MODULATE_IMG
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/IMG/IMG_texture_env_enhanced_fixed_function.txt GL_IMG_texture_env_enhanced_fixed_function> extension is available.

gl_IMG_texture_env_enhanced_fixed_function :: Bool
gl_IMG_texture_env_enhanced_fixed_function = member "GL_IMG_texture_env_enhanced_fixed_function" extensions
{-# NOINLINE gl_IMG_texture_env_enhanced_fixed_function #-}

pattern GL_ADD_BLEND_IMG  :: (Eq a, Num a) => a

pattern GL_ADD_BLEND_IMG = 0x8C09

pattern GL_DOT3_RGBA_IMG  :: (Eq a, Num a) => a

pattern GL_DOT3_RGBA_IMG = 0x86AF

pattern GL_FACTOR_ALPHA_MODULATE_IMG  :: (Eq a, Num a) => a

pattern GL_FACTOR_ALPHA_MODULATE_IMG = 0x8C07

pattern GL_FRAGMENT_ALPHA_MODULATE_IMG  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_ALPHA_MODULATE_IMG = 0x8C08

pattern GL_MODULATE_COLOR_IMG  :: (Eq a, Num a) => a

pattern GL_MODULATE_COLOR_IMG = 0x8C04

pattern GL_RECIP_ADD_SIGNED_ALPHA_IMG  :: (Eq a, Num a) => a

pattern GL_RECIP_ADD_SIGNED_ALPHA_IMG = 0x8C05

pattern GL_TEXTURE_ALPHA_MODULATE_IMG  :: (Eq a, Num a) => a

pattern GL_TEXTURE_ALPHA_MODULATE_IMG = 0x8C06