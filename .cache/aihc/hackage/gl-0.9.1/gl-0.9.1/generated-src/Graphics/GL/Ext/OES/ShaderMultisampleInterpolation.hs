-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.ShaderMultisampleInterpolation (
  -- * Extension Support
    gl_OES_shader_multisample_interpolation

  -- * GL_OES_shader_multisample_interpolation
  , pattern GL_FRAGMENT_INTERPOLATION_OFFSET_BITS_OES
  , pattern GL_MAX_FRAGMENT_INTERPOLATION_OFFSET_OES
  , pattern GL_MIN_FRAGMENT_INTERPOLATION_OFFSET_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_shader_multisample_interpolation.txt GL_OES_shader_multisample_interpolation> extension is available.

gl_OES_shader_multisample_interpolation :: Bool
gl_OES_shader_multisample_interpolation = member "GL_OES_shader_multisample_interpolation" extensions
{-# NOINLINE gl_OES_shader_multisample_interpolation #-}

pattern GL_FRAGMENT_INTERPOLATION_OFFSET_BITS_OES  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_INTERPOLATION_OFFSET_BITS_OES = 0x8E5D

pattern GL_MAX_FRAGMENT_INTERPOLATION_OFFSET_OES  :: (Eq a, Num a) => a

pattern GL_MAX_FRAGMENT_INTERPOLATION_OFFSET_OES = 0x8E5C

pattern GL_MIN_FRAGMENT_INTERPOLATION_OFFSET_OES  :: (Eq a, Num a) => a

pattern GL_MIN_FRAGMENT_INTERPOLATION_OFFSET_OES = 0x8E5B