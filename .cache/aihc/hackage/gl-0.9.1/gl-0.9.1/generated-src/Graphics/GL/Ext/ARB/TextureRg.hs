-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureRg (
  -- * Extension Support
    gl_ARB_texture_rg

  -- * GL_ARB_texture_rg
  , pattern GL_R16
  , pattern GL_R16F
  , pattern GL_R16I
  , pattern GL_R16UI
  , pattern GL_R32F
  , pattern GL_R32I
  , pattern GL_R32UI
  , pattern GL_R8
  , pattern GL_R8I
  , pattern GL_R8UI
  , pattern GL_RG
  , pattern GL_RG16
  , pattern GL_RG16F
  , pattern GL_RG16I
  , pattern GL_RG16UI
  , pattern GL_RG32F
  , pattern GL_RG32I
  , pattern GL_RG32UI
  , pattern GL_RG8
  , pattern GL_RG8I
  , pattern GL_RG8UI
  , pattern GL_RG_INTEGER
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_rg.txt GL_ARB_texture_rg> extension is available.

gl_ARB_texture_rg :: Bool
gl_ARB_texture_rg = member "GL_ARB_texture_rg" extensions
{-# NOINLINE gl_ARB_texture_rg #-}