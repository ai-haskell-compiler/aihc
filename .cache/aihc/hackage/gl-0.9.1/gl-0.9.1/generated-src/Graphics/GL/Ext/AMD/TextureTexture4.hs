-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.TextureTexture4 (
  -- * Extension Support
    gl_AMD_texture_texture4
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/texture_texture4.txt GL_AMD_texture_texture4> extension is available.

gl_AMD_texture_texture4 :: Bool
gl_AMD_texture_texture4 = member "GL_AMD_texture_texture4" extensions
{-# NOINLINE gl_AMD_texture_texture4 #-}