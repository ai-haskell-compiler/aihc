-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.TextureEnvCombine3 (
  -- * Extension Support
    gl_ATI_texture_env_combine3

  -- * GL_ATI_texture_env_combine3
  , pattern GL_MODULATE_ADD_ATI
  , pattern GL_MODULATE_SIGNED_ADD_ATI
  , pattern GL_MODULATE_SUBTRACT_ATI
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/texture_env_combine3.txt GL_ATI_texture_env_combine3> extension is available.

gl_ATI_texture_env_combine3 :: Bool
gl_ATI_texture_env_combine3 = member "GL_ATI_texture_env_combine3" extensions
{-# NOINLINE gl_ATI_texture_env_combine3 #-}

pattern GL_MODULATE_ADD_ATI  :: (Eq a, Num a) => a

pattern GL_MODULATE_ADD_ATI = 0x8744

pattern GL_MODULATE_SIGNED_ADD_ATI  :: (Eq a, Num a) => a

pattern GL_MODULATE_SIGNED_ADD_ATI = 0x8745

pattern GL_MODULATE_SUBTRACT_ATI  :: (Eq a, Num a) => a

pattern GL_MODULATE_SUBTRACT_ATI = 0x8746