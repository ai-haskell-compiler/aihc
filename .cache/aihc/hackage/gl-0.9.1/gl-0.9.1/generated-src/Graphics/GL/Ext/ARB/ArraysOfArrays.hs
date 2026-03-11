-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ArraysOfArrays (
  -- * Extension Support
    gl_ARB_arrays_of_arrays
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/arrays_of_arrays.txt GL_ARB_arrays_of_arrays> extension is available.

gl_ARB_arrays_of_arrays :: Bool
gl_ARB_arrays_of_arrays = member "GL_ARB_arrays_of_arrays" extensions
{-# NOINLINE gl_ARB_arrays_of_arrays #-}