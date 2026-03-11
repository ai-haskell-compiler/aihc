-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderGroupVote (
  -- * Extension Support
    gl_ARB_shader_group_vote
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_group_vote.txt GL_ARB_shader_group_vote> extension is available.

gl_ARB_shader_group_vote :: Bool
gl_ARB_shader_group_vote = member "GL_ARB_shader_group_vote" extensions
{-# NOINLINE gl_ARB_shader_group_vote #-}