-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureEnvCombine (
  -- * Extension Support
    gl_EXT_texture_env_combine

  -- * GL_EXT_texture_env_combine
  , pattern GL_ADD_SIGNED_EXT
  , pattern GL_COMBINE_ALPHA_EXT
  , pattern GL_COMBINE_EXT
  , pattern GL_COMBINE_RGB_EXT
  , pattern GL_CONSTANT_EXT
  , pattern GL_INTERPOLATE_EXT
  , pattern GL_OPERAND0_ALPHA_EXT
  , pattern GL_OPERAND0_RGB_EXT
  , pattern GL_OPERAND1_ALPHA_EXT
  , pattern GL_OPERAND1_RGB_EXT
  , pattern GL_OPERAND2_ALPHA_EXT
  , pattern GL_OPERAND2_RGB_EXT
  , pattern GL_PREVIOUS_EXT
  , pattern GL_PRIMARY_COLOR_EXT
  , pattern GL_RGB_SCALE_EXT
  , pattern GL_SOURCE0_ALPHA_EXT
  , pattern GL_SOURCE0_RGB_EXT
  , pattern GL_SOURCE1_ALPHA_EXT
  , pattern GL_SOURCE1_RGB_EXT
  , pattern GL_SOURCE2_ALPHA_EXT
  , pattern GL_SOURCE2_RGB_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_env_combine.txt GL_EXT_texture_env_combine> extension is available.

gl_EXT_texture_env_combine :: Bool
gl_EXT_texture_env_combine = member "GL_EXT_texture_env_combine" extensions
{-# NOINLINE gl_EXT_texture_env_combine #-}

pattern GL_ADD_SIGNED_EXT  :: (Eq a, Num a) => a

pattern GL_ADD_SIGNED_EXT = 0x8574

pattern GL_COMBINE_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_COMBINE_ALPHA_EXT = 0x8572

pattern GL_COMBINE_EXT  :: (Eq a, Num a) => a

pattern GL_COMBINE_EXT = 0x8570

pattern GL_COMBINE_RGB_EXT  :: (Eq a, Num a) => a

pattern GL_COMBINE_RGB_EXT = 0x8571

pattern GL_CONSTANT_EXT  :: (Eq a, Num a) => a

pattern GL_CONSTANT_EXT = 0x8576

pattern GL_INTERPOLATE_EXT  :: (Eq a, Num a) => a

pattern GL_INTERPOLATE_EXT = 0x8575

pattern GL_OPERAND0_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_OPERAND0_ALPHA_EXT = 0x8598

pattern GL_OPERAND0_RGB_EXT  :: (Eq a, Num a) => a

pattern GL_OPERAND0_RGB_EXT = 0x8590

pattern GL_OPERAND1_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_OPERAND1_ALPHA_EXT = 0x8599

pattern GL_OPERAND1_RGB_EXT  :: (Eq a, Num a) => a

pattern GL_OPERAND1_RGB_EXT = 0x8591

pattern GL_OPERAND2_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_OPERAND2_ALPHA_EXT = 0x859A

pattern GL_OPERAND2_RGB_EXT  :: (Eq a, Num a) => a

pattern GL_OPERAND2_RGB_EXT = 0x8592

pattern GL_PREVIOUS_EXT  :: (Eq a, Num a) => a

pattern GL_PREVIOUS_EXT = 0x8578

pattern GL_PRIMARY_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_PRIMARY_COLOR_EXT = 0x8577

pattern GL_RGB_SCALE_EXT  :: (Eq a, Num a) => a

pattern GL_RGB_SCALE_EXT = 0x8573

pattern GL_SOURCE0_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_SOURCE0_ALPHA_EXT = 0x8588

pattern GL_SOURCE0_RGB_EXT  :: (Eq a, Num a) => a

pattern GL_SOURCE0_RGB_EXT = 0x8580

pattern GL_SOURCE1_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_SOURCE1_ALPHA_EXT = 0x8589

pattern GL_SOURCE1_RGB_EXT  :: (Eq a, Num a) => a

pattern GL_SOURCE1_RGB_EXT = 0x8581

pattern GL_SOURCE2_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_SOURCE2_ALPHA_EXT = 0x858A

pattern GL_SOURCE2_RGB_EXT  :: (Eq a, Num a) => a

pattern GL_SOURCE2_RGB_EXT = 0x8582