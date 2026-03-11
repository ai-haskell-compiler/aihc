-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.Compatibility (
  -- * Extension Support
    gl_ARB_compatibility
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/compatibility.txt GL_ARB_compatibility> extension is available.

gl_ARB_compatibility :: Bool
gl_ARB_compatibility = member "GL_ARB_compatibility" extensions
{-# NOINLINE gl_ARB_compatibility #-}