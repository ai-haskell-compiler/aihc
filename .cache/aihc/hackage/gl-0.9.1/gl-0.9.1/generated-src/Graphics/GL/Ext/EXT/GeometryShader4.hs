-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.GeometryShader4 (
  -- * Extension Support
    gl_EXT_geometry_shader4

  -- * GL_EXT_geometry_shader4
  , glProgramParameteriEXT
  , pattern GL_FRAMEBUFFER_ATTACHMENT_LAYERED_EXT
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_EXT
  , pattern GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_EXT
  , pattern GL_GEOMETRY_INPUT_TYPE_EXT
  , pattern GL_GEOMETRY_OUTPUT_TYPE_EXT
  , pattern GL_GEOMETRY_SHADER_EXT
  , pattern GL_GEOMETRY_VERTICES_OUT_EXT
  , pattern GL_LINES_ADJACENCY_EXT
  , pattern GL_LINE_STRIP_ADJACENCY_EXT
  , pattern GL_MAX_GEOMETRY_OUTPUT_VERTICES_EXT
  , pattern GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_EXT
  , pattern GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_EXT
  , pattern GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_EXT
  , pattern GL_MAX_GEOMETRY_VARYING_COMPONENTS_EXT
  , pattern GL_MAX_VARYING_COMPONENTS_EXT
  , pattern GL_MAX_VERTEX_VARYING_COMPONENTS_EXT
  , pattern GL_PROGRAM_POINT_SIZE_EXT
  , pattern GL_TRIANGLES_ADJACENCY_EXT
  , pattern GL_TRIANGLE_STRIP_ADJACENCY_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/geometry_shader4.txt GL_EXT_geometry_shader4> extension is available.

gl_EXT_geometry_shader4 :: Bool
gl_EXT_geometry_shader4 = member "GL_EXT_geometry_shader4" extensions
{-# NOINLINE gl_EXT_geometry_shader4 #-}

pattern GL_MAX_GEOMETRY_VARYING_COMPONENTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_GEOMETRY_VARYING_COMPONENTS_EXT = 0x8DDD

pattern GL_MAX_VARYING_COMPONENTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_VARYING_COMPONENTS_EXT = 0x8B4B

pattern GL_MAX_VERTEX_VARYING_COMPONENTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_VARYING_COMPONENTS_EXT = 0x8DDE