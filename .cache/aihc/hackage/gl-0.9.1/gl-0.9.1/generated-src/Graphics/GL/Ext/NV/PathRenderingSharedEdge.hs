-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.PathRenderingSharedEdge (
  -- * Extension Support
    gl_NV_path_rendering_shared_edge

  -- * GL_NV_path_rendering_shared_edge
  , pattern GL_SHARED_EDGE_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/path_rendering_shared_edge.txt GL_NV_path_rendering_shared_edge> extension is available.

gl_NV_path_rendering_shared_edge :: Bool
gl_NV_path_rendering_shared_edge = member "GL_NV_path_rendering_shared_edge" extensions
{-# NOINLINE gl_NV_path_rendering_shared_edge #-}

pattern GL_SHARED_EDGE_NV  :: (Eq a, Num a) => a

pattern GL_SHARED_EDGE_NV = 0xC0