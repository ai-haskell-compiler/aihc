-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShaderGroupVote (
  -- * Extension Support
    gl_EXT_shader_group_vote
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_shader_group_vote.txt GL_EXT_shader_group_vote> extension is available.

gl_EXT_shader_group_vote :: Bool
gl_EXT_shader_group_vote = member "GL_EXT_shader_group_vote" extensions
{-# NOINLINE gl_EXT_shader_group_vote #-}