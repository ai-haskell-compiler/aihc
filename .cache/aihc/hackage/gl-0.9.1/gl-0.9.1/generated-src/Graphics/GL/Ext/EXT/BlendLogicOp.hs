-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.BlendLogicOp (
  -- * Extension Support
    gl_EXT_blend_logic_op
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/blend_logic_op.txt GL_EXT_blend_logic_op> extension is available.

gl_EXT_blend_logic_op :: Bool
gl_EXT_blend_logic_op = member "GL_EXT_blend_logic_op" extensions
{-# NOINLINE gl_EXT_blend_logic_op #-}