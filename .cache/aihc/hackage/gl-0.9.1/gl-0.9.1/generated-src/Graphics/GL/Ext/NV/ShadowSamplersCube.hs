-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ShadowSamplersCube (
  -- * Extension Support
    gl_NV_shadow_samplers_cube

  -- * GL_NV_shadow_samplers_cube
  , pattern GL_SAMPLER_CUBE_SHADOW_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_shadow_samplers_cube.txt GL_NV_shadow_samplers_cube> extension is available.

gl_NV_shadow_samplers_cube :: Bool
gl_NV_shadow_samplers_cube = member "GL_NV_shadow_samplers_cube" extensions
{-# NOINLINE gl_NV_shadow_samplers_cube #-}

pattern GL_SAMPLER_CUBE_SHADOW_NV  :: (Eq a, Num a) => a

pattern GL_SAMPLER_CUBE_SHADOW_NV = 0x8DC5