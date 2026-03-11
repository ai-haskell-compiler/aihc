-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.MapBufferRange (
  -- * Extension Support
    gl_ARB_map_buffer_range

  -- * GL_ARB_map_buffer_range
  , glFlushMappedBufferRange
  , glMapBufferRange
  , pattern GL_MAP_FLUSH_EXPLICIT_BIT
  , pattern GL_MAP_INVALIDATE_BUFFER_BIT
  , pattern GL_MAP_INVALIDATE_RANGE_BIT
  , pattern GL_MAP_READ_BIT
  , pattern GL_MAP_UNSYNCHRONIZED_BIT
  , pattern GL_MAP_WRITE_BIT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/map_buffer_range.txt GL_ARB_map_buffer_range> extension is available.

gl_ARB_map_buffer_range :: Bool
gl_ARB_map_buffer_range = member "GL_ARB_map_buffer_range" extensions
{-# NOINLINE gl_ARB_map_buffer_range #-}