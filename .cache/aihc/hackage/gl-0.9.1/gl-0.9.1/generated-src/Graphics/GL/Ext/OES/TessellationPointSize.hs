-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TessellationPointSize (
  -- * Extension Support
    gl_OES_tessellation_point_size
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_tessellation_shader.txt GL_OES_tessellation_point_size> extension is available.

gl_OES_tessellation_point_size :: Bool
gl_OES_tessellation_point_size = member "GL_OES_tessellation_point_size" extensions
{-# NOINLINE gl_OES_tessellation_point_size #-}