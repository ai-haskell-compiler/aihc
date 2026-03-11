-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderBallot (
  -- * Extension Support
    gl_ARB_shader_ballot
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_ballot.txt GL_ARB_shader_ballot> extension is available.

gl_ARB_shader_ballot :: Bool
gl_ARB_shader_ballot = member "GL_ARB_shader_ballot" extensions
{-# NOINLINE gl_ARB_shader_ballot #-}