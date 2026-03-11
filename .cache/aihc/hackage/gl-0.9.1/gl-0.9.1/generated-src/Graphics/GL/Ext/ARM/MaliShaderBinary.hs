-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARM.MaliShaderBinary (
  -- * Extension Support
    gl_ARM_mali_shader_binary

  -- * GL_ARM_mali_shader_binary
  , pattern GL_MALI_SHADER_BINARY_ARM
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ARM/ARM_mali_shader_binary.txt GL_ARM_mali_shader_binary> extension is available.

gl_ARM_mali_shader_binary :: Bool
gl_ARM_mali_shader_binary = member "GL_ARM_mali_shader_binary" extensions
{-# NOINLINE gl_ARM_mali_shader_binary #-}

pattern GL_MALI_SHADER_BINARY_ARM  :: (Eq a, Num a) => a

pattern GL_MALI_SHADER_BINARY_ARM = 0x8F60