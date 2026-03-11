-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.ShaderBallot (
  -- * Extension Support
    gl_AMD_shader_ballot
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/shader_ballot.txt GL_AMD_shader_ballot> extension is available.

gl_AMD_shader_ballot :: Bool
gl_AMD_shader_ballot = member "GL_AMD_shader_ballot" extensions
{-# NOINLINE gl_AMD_shader_ballot #-}