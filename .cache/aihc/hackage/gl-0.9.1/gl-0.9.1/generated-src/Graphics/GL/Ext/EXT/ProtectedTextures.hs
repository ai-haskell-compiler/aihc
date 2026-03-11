-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ProtectedTextures (
  -- * Extension Support
    gl_EXT_protected_textures

  -- * GL_EXT_protected_textures
  , pattern GL_CONTEXT_FLAG_PROTECTED_CONTENT_BIT_EXT
  , pattern GL_TEXTURE_PROTECTED_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_protected_textures.txt GL_EXT_protected_textures> extension is available.

gl_EXT_protected_textures :: Bool
gl_EXT_protected_textures = member "GL_EXT_protected_textures" extensions
{-# NOINLINE gl_EXT_protected_textures #-}

pattern GL_CONTEXT_FLAG_PROTECTED_CONTENT_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_CONTEXT_FLAG_PROTECTED_CONTENT_BIT_EXT = 0x00000010

pattern GL_TEXTURE_PROTECTED_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_PROTECTED_EXT = 0x8BFA