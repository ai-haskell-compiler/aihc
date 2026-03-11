-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShadingLanguagePacking (
  -- * Extension Support
    gl_ARB_shading_language_packing
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shading_language_packing.txt GL_ARB_shading_language_packing> extension is available.

gl_ARB_shading_language_packing :: Bool
gl_ARB_shading_language_packing = member "GL_ARB_shading_language_packing" extensions
{-# NOINLINE gl_ARB_shading_language_packing #-}