-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShadingLanguage420pack (
  -- * Extension Support
    gl_ARB_shading_language_420pack
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shading_language_420pack.txt GL_ARB_shading_language_420pack> extension is available.

gl_ARB_shading_language_420pack :: Bool
gl_ARB_shading_language_420pack = member "GL_ARB_shading_language_420pack" extensions
{-# NOINLINE gl_ARB_shading_language_420pack #-}