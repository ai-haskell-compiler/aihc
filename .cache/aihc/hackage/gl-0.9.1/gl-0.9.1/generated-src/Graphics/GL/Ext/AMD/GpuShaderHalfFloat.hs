-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.GpuShaderHalfFloat (
  -- * Extension Support
    gl_AMD_gpu_shader_half_float

  -- * GL_AMD_gpu_shader_half_float
  , pattern GL_FLOAT16_MAT2_AMD
  , pattern GL_FLOAT16_MAT2x3_AMD
  , pattern GL_FLOAT16_MAT2x4_AMD
  , pattern GL_FLOAT16_MAT3_AMD
  , pattern GL_FLOAT16_MAT3x2_AMD
  , pattern GL_FLOAT16_MAT3x4_AMD
  , pattern GL_FLOAT16_MAT4_AMD
  , pattern GL_FLOAT16_MAT4x2_AMD
  , pattern GL_FLOAT16_MAT4x3_AMD
  , pattern GL_FLOAT16_NV
  , pattern GL_FLOAT16_VEC2_NV
  , pattern GL_FLOAT16_VEC3_NV
  , pattern GL_FLOAT16_VEC4_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/gpu_shader_half_float.txt GL_AMD_gpu_shader_half_float> extension is available.

gl_AMD_gpu_shader_half_float :: Bool
gl_AMD_gpu_shader_half_float = member "GL_AMD_gpu_shader_half_float" extensions
{-# NOINLINE gl_AMD_gpu_shader_half_float #-}

pattern GL_FLOAT16_MAT2_AMD  :: (Eq a, Num a) => a

pattern GL_FLOAT16_MAT2_AMD = 0x91C5

pattern GL_FLOAT16_MAT2x3_AMD  :: (Eq a, Num a) => a

pattern GL_FLOAT16_MAT2x3_AMD = 0x91C8

pattern GL_FLOAT16_MAT2x4_AMD  :: (Eq a, Num a) => a

pattern GL_FLOAT16_MAT2x4_AMD = 0x91C9

pattern GL_FLOAT16_MAT3_AMD  :: (Eq a, Num a) => a

pattern GL_FLOAT16_MAT3_AMD = 0x91C6

pattern GL_FLOAT16_MAT3x2_AMD  :: (Eq a, Num a) => a

pattern GL_FLOAT16_MAT3x2_AMD = 0x91CA

pattern GL_FLOAT16_MAT3x4_AMD  :: (Eq a, Num a) => a

pattern GL_FLOAT16_MAT3x4_AMD = 0x91CB

pattern GL_FLOAT16_MAT4_AMD  :: (Eq a, Num a) => a

pattern GL_FLOAT16_MAT4_AMD = 0x91C7

pattern GL_FLOAT16_MAT4x2_AMD  :: (Eq a, Num a) => a

pattern GL_FLOAT16_MAT4x2_AMD = 0x91CC

pattern GL_FLOAT16_MAT4x3_AMD  :: (Eq a, Num a) => a

pattern GL_FLOAT16_MAT4x3_AMD = 0x91CD