-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ExplicitAttribLocation (
  -- * Extension Support
    gl_ARB_explicit_attrib_location
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/explicit_attrib_location.txt GL_ARB_explicit_attrib_location> extension is available.

gl_ARB_explicit_attrib_location :: Bool
gl_ARB_explicit_attrib_location = member "GL_ARB_explicit_attrib_location" extensions
{-# NOINLINE gl_ARB_explicit_attrib_location #-}