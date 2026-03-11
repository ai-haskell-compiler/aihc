-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShadowSamplers (
  -- * Extension Support
    gl_EXT_shadow_samplers

  -- * GL_EXT_shadow_samplers
  , pattern GL_COMPARE_REF_TO_TEXTURE_EXT
  , pattern GL_SAMPLER_2D_SHADOW_EXT
  , pattern GL_TEXTURE_COMPARE_FUNC_EXT
  , pattern GL_TEXTURE_COMPARE_MODE_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_shadow_samplers.txt GL_EXT_shadow_samplers> extension is available.

gl_EXT_shadow_samplers :: Bool
gl_EXT_shadow_samplers = member "GL_EXT_shadow_samplers" extensions
{-# NOINLINE gl_EXT_shadow_samplers #-}

pattern GL_COMPARE_REF_TO_TEXTURE_EXT  :: (Eq a, Num a) => a

pattern GL_COMPARE_REF_TO_TEXTURE_EXT = 0x884E

pattern GL_SAMPLER_2D_SHADOW_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLER_2D_SHADOW_EXT = 0x8B62

pattern GL_TEXTURE_COMPARE_FUNC_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COMPARE_FUNC_EXT = 0x884D

pattern GL_TEXTURE_COMPARE_MODE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COMPARE_MODE_EXT = 0x884C