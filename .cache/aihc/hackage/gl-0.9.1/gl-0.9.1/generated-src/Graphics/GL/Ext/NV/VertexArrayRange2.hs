-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.VertexArrayRange2 (
  -- * Extension Support
    gl_NV_vertex_array_range2

  -- * GL_NV_vertex_array_range2
  , pattern GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/vertex_array_range2.txt GL_NV_vertex_array_range2> extension is available.

gl_NV_vertex_array_range2 :: Bool
gl_NV_vertex_array_range2 = member "GL_NV_vertex_array_range2" extensions
{-# NOINLINE gl_NV_vertex_array_range2 #-}

pattern GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV = 0x8533