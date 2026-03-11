-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.MapBufferAlignment (
  -- * Extension Support
    gl_ARB_map_buffer_alignment

  -- * GL_ARB_map_buffer_alignment
  , pattern GL_MIN_MAP_BUFFER_ALIGNMENT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/map_buffer_alignment.txt GL_ARB_map_buffer_alignment> extension is available.

gl_ARB_map_buffer_alignment :: Bool
gl_ARB_map_buffer_alignment = member "GL_ARB_map_buffer_alignment" extensions
{-# NOINLINE gl_ARB_map_buffer_alignment #-}