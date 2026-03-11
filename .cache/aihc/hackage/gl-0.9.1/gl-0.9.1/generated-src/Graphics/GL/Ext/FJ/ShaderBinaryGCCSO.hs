-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.FJ.ShaderBinaryGCCSO (
  -- * Extension Support
    gl_FJ_shader_binary_GCCSO

  -- * GL_FJ_shader_binary_GCCSO
  , pattern GL_GCCSO_SHADER_BINARY_FJ
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/FJ/FJ_shader_binary_GCCSO.txt GL_FJ_shader_binary_GCCSO> extension is available.

gl_FJ_shader_binary_GCCSO :: Bool
gl_FJ_shader_binary_GCCSO = member "GL_FJ_shader_binary_GCCSO" extensions
{-# NOINLINE gl_FJ_shader_binary_GCCSO #-}

pattern GL_GCCSO_SHADER_BINARY_FJ  :: (Eq a, Num a) => a

pattern GL_GCCSO_SHADER_BINARY_FJ = 0x9260