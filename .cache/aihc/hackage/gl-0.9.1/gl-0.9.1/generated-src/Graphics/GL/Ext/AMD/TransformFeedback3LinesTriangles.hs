-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.TransformFeedback3LinesTriangles (
  -- * Extension Support
    gl_AMD_transform_feedback3_lines_triangles
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/transform_feedback3_lines_triangles.txt GL_AMD_transform_feedback3_lines_triangles> extension is available.

gl_AMD_transform_feedback3_lines_triangles :: Bool
gl_AMD_transform_feedback3_lines_triangles = member "GL_AMD_transform_feedback3_lines_triangles" extensions
{-# NOINLINE gl_AMD_transform_feedback3_lines_triangles #-}