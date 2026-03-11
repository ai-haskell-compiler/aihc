-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.DerivativeControl (
  -- * Extension Support
    gl_ARB_derivative_control
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/derivative_control.txt GL_ARB_derivative_control> extension is available.

gl_ARB_derivative_control :: Bool
gl_ARB_derivative_control = member "GL_ARB_derivative_control" extensions
{-# NOINLINE gl_ARB_derivative_control #-}