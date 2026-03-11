-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TextureEnvCombine4 (
  -- * Extension Support
    gl_NV_texture_env_combine4

  -- * GL_NV_texture_env_combine4
  , pattern GL_COMBINE4_NV
  , pattern GL_OPERAND3_ALPHA_NV
  , pattern GL_OPERAND3_RGB_NV
  , pattern GL_SOURCE3_ALPHA_NV
  , pattern GL_SOURCE3_RGB_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/texture_env_combine4.txt GL_NV_texture_env_combine4> extension is available.

gl_NV_texture_env_combine4 :: Bool
gl_NV_texture_env_combine4 = member "GL_NV_texture_env_combine4" extensions
{-# NOINLINE gl_NV_texture_env_combine4 #-}

pattern GL_COMBINE4_NV  :: (Eq a, Num a) => a

pattern GL_COMBINE4_NV = 0x8503

pattern GL_OPERAND3_ALPHA_NV  :: (Eq a, Num a) => a

pattern GL_OPERAND3_ALPHA_NV = 0x859B

pattern GL_OPERAND3_RGB_NV  :: (Eq a, Num a) => a

pattern GL_OPERAND3_RGB_NV = 0x8593

pattern GL_SOURCE3_ALPHA_NV  :: (Eq a, Num a) => a

pattern GL_SOURCE3_ALPHA_NV = 0x858B

pattern GL_SOURCE3_RGB_NV  :: (Eq a, Num a) => a

pattern GL_SOURCE3_RGB_NV = 0x8583