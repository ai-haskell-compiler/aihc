-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.FragmentCoordConventions (
  -- * Extension Support
    gl_ARB_fragment_coord_conventions
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/fragment_coord_conventions.txt GL_ARB_fragment_coord_conventions> extension is available.

gl_ARB_fragment_coord_conventions :: Bool
gl_ARB_fragment_coord_conventions = member "GL_ARB_fragment_coord_conventions" extensions
{-# NOINLINE gl_ARB_fragment_coord_conventions #-}