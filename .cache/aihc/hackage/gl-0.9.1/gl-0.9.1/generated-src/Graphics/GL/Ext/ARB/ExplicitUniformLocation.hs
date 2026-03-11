-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ExplicitUniformLocation (
  -- * Extension Support
    gl_ARB_explicit_uniform_location

  -- * GL_ARB_explicit_uniform_location
  , pattern GL_MAX_UNIFORM_LOCATIONS
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/explicit_uniform_location.txt GL_ARB_explicit_uniform_location> extension is available.

gl_ARB_explicit_uniform_location :: Bool
gl_ARB_explicit_uniform_location = member "GL_ARB_explicit_uniform_location" extensions
{-# NOINLINE gl_ARB_explicit_uniform_location #-}