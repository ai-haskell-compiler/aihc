-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ShadowSamplersArray (
  -- * Extension Support
    gl_NV_shadow_samplers_array

  -- * GL_NV_shadow_samplers_array
  , pattern GL_SAMPLER_2D_ARRAY_SHADOW_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_shadow_samplers_array.txt GL_NV_shadow_samplers_array> extension is available.

gl_NV_shadow_samplers_array :: Bool
gl_NV_shadow_samplers_array = member "GL_NV_shadow_samplers_array" extensions
{-# NOINLINE gl_NV_shadow_samplers_array #-}

pattern GL_SAMPLER_2D_ARRAY_SHADOW_NV  :: (Eq a, Num a) => a

pattern GL_SAMPLER_2D_ARRAY_SHADOW_NV = 0x8DC4