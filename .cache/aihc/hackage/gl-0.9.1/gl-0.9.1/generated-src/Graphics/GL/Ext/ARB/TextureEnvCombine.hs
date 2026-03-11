-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureEnvCombine (
  -- * Extension Support
    gl_ARB_texture_env_combine

  -- * GL_ARB_texture_env_combine
  , pattern GL_ADD_SIGNED_ARB
  , pattern GL_COMBINE_ALPHA_ARB
  , pattern GL_COMBINE_ARB
  , pattern GL_COMBINE_RGB_ARB
  , pattern GL_CONSTANT_ARB
  , pattern GL_INTERPOLATE_ARB
  , pattern GL_OPERAND0_ALPHA_ARB
  , pattern GL_OPERAND0_RGB_ARB
  , pattern GL_OPERAND1_ALPHA_ARB
  , pattern GL_OPERAND1_RGB_ARB
  , pattern GL_OPERAND2_ALPHA_ARB
  , pattern GL_OPERAND2_RGB_ARB
  , pattern GL_PREVIOUS_ARB
  , pattern GL_PRIMARY_COLOR_ARB
  , pattern GL_RGB_SCALE_ARB
  , pattern GL_SOURCE0_ALPHA_ARB
  , pattern GL_SOURCE0_RGB_ARB
  , pattern GL_SOURCE1_ALPHA_ARB
  , pattern GL_SOURCE1_RGB_ARB
  , pattern GL_SOURCE2_ALPHA_ARB
  , pattern GL_SOURCE2_RGB_ARB
  , pattern GL_SUBTRACT_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_env_combine.txt GL_ARB_texture_env_combine> extension is available.

gl_ARB_texture_env_combine :: Bool
gl_ARB_texture_env_combine = member "GL_ARB_texture_env_combine" extensions
{-# NOINLINE gl_ARB_texture_env_combine #-}

pattern GL_ADD_SIGNED_ARB  :: (Eq a, Num a) => a

pattern GL_ADD_SIGNED_ARB = 0x8574

pattern GL_COMBINE_ALPHA_ARB  :: (Eq a, Num a) => a

pattern GL_COMBINE_ALPHA_ARB = 0x8572

pattern GL_COMBINE_ARB  :: (Eq a, Num a) => a

pattern GL_COMBINE_ARB = 0x8570

pattern GL_COMBINE_RGB_ARB  :: (Eq a, Num a) => a

pattern GL_COMBINE_RGB_ARB = 0x8571

pattern GL_CONSTANT_ARB  :: (Eq a, Num a) => a

pattern GL_CONSTANT_ARB = 0x8576

pattern GL_INTERPOLATE_ARB  :: (Eq a, Num a) => a

pattern GL_INTERPOLATE_ARB = 0x8575

pattern GL_OPERAND0_ALPHA_ARB  :: (Eq a, Num a) => a

pattern GL_OPERAND0_ALPHA_ARB = 0x8598

pattern GL_OPERAND0_RGB_ARB  :: (Eq a, Num a) => a

pattern GL_OPERAND0_RGB_ARB = 0x8590

pattern GL_OPERAND1_ALPHA_ARB  :: (Eq a, Num a) => a

pattern GL_OPERAND1_ALPHA_ARB = 0x8599

pattern GL_OPERAND1_RGB_ARB  :: (Eq a, Num a) => a

pattern GL_OPERAND1_RGB_ARB = 0x8591

pattern GL_OPERAND2_ALPHA_ARB  :: (Eq a, Num a) => a

pattern GL_OPERAND2_ALPHA_ARB = 0x859A

pattern GL_OPERAND2_RGB_ARB  :: (Eq a, Num a) => a

pattern GL_OPERAND2_RGB_ARB = 0x8592

pattern GL_PREVIOUS_ARB  :: (Eq a, Num a) => a

pattern GL_PREVIOUS_ARB = 0x8578

pattern GL_PRIMARY_COLOR_ARB  :: (Eq a, Num a) => a

pattern GL_PRIMARY_COLOR_ARB = 0x8577

pattern GL_RGB_SCALE_ARB  :: (Eq a, Num a) => a

pattern GL_RGB_SCALE_ARB = 0x8573

pattern GL_SOURCE0_ALPHA_ARB  :: (Eq a, Num a) => a

pattern GL_SOURCE0_ALPHA_ARB = 0x8588

pattern GL_SOURCE0_RGB_ARB  :: (Eq a, Num a) => a

pattern GL_SOURCE0_RGB_ARB = 0x8580

pattern GL_SOURCE1_ALPHA_ARB  :: (Eq a, Num a) => a

pattern GL_SOURCE1_ALPHA_ARB = 0x8589

pattern GL_SOURCE1_RGB_ARB  :: (Eq a, Num a) => a

pattern GL_SOURCE1_RGB_ARB = 0x8581

pattern GL_SOURCE2_ALPHA_ARB  :: (Eq a, Num a) => a

pattern GL_SOURCE2_ALPHA_ARB = 0x858A

pattern GL_SOURCE2_RGB_ARB  :: (Eq a, Num a) => a

pattern GL_SOURCE2_RGB_ARB = 0x8582

pattern GL_SUBTRACT_ARB  :: (Eq a, Num a) => a

pattern GL_SUBTRACT_ARB = 0x84E7