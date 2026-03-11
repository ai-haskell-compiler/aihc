-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.DrawInstanced (
  -- * Extension Support
    gl_EXT_draw_instanced

  -- * GL_EXT_draw_instanced
  , glDrawArraysInstancedEXT
  , glDrawElementsInstancedEXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_draw_instanced.txt GL_EXT_draw_instanced> extension is available.

gl_EXT_draw_instanced :: Bool
gl_EXT_draw_instanced = member "GL_EXT_draw_instanced" extensions
{-# NOINLINE gl_EXT_draw_instanced #-}