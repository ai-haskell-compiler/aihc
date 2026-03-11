-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.PipelineStatisticsQuery (
  -- * Extension Support
    gl_ARB_pipeline_statistics_query

  -- * GL_ARB_pipeline_statistics_query
  , pattern GL_CLIPPING_INPUT_PRIMITIVES_ARB
  , pattern GL_CLIPPING_OUTPUT_PRIMITIVES_ARB
  , pattern GL_COMPUTE_SHADER_INVOCATIONS_ARB
  , pattern GL_FRAGMENT_SHADER_INVOCATIONS_ARB
  , pattern GL_GEOMETRY_SHADER_INVOCATIONS
  , pattern GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED_ARB
  , pattern GL_PRIMITIVES_SUBMITTED_ARB
  , pattern GL_TESS_CONTROL_SHADER_PATCHES_ARB
  , pattern GL_TESS_EVALUATION_SHADER_INVOCATIONS_ARB
  , pattern GL_VERTEX_SHADER_INVOCATIONS_ARB
  , pattern GL_VERTICES_SUBMITTED_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/pipeline_statistics_query.txt GL_ARB_pipeline_statistics_query> extension is available.

gl_ARB_pipeline_statistics_query :: Bool
gl_ARB_pipeline_statistics_query = member "GL_ARB_pipeline_statistics_query" extensions
{-# NOINLINE gl_ARB_pipeline_statistics_query #-}

pattern GL_CLIPPING_INPUT_PRIMITIVES_ARB  :: (Eq a, Num a) => a

pattern GL_CLIPPING_INPUT_PRIMITIVES_ARB = 0x82F6

pattern GL_CLIPPING_OUTPUT_PRIMITIVES_ARB  :: (Eq a, Num a) => a

pattern GL_CLIPPING_OUTPUT_PRIMITIVES_ARB = 0x82F7

pattern GL_COMPUTE_SHADER_INVOCATIONS_ARB  :: (Eq a, Num a) => a

pattern GL_COMPUTE_SHADER_INVOCATIONS_ARB = 0x82F5

pattern GL_FRAGMENT_SHADER_INVOCATIONS_ARB  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_SHADER_INVOCATIONS_ARB = 0x82F4

pattern GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED_ARB  :: (Eq a, Num a) => a

pattern GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED_ARB = 0x82F3

pattern GL_PRIMITIVES_SUBMITTED_ARB  :: (Eq a, Num a) => a

pattern GL_PRIMITIVES_SUBMITTED_ARB = 0x82EF

pattern GL_TESS_CONTROL_SHADER_PATCHES_ARB  :: (Eq a, Num a) => a

pattern GL_TESS_CONTROL_SHADER_PATCHES_ARB = 0x82F1

pattern GL_TESS_EVALUATION_SHADER_INVOCATIONS_ARB  :: (Eq a, Num a) => a

pattern GL_TESS_EVALUATION_SHADER_INVOCATIONS_ARB = 0x82F2

pattern GL_VERTEX_SHADER_INVOCATIONS_ARB  :: (Eq a, Num a) => a

pattern GL_VERTEX_SHADER_INVOCATIONS_ARB = 0x82F0

pattern GL_VERTICES_SUBMITTED_ARB  :: (Eq a, Num a) => a

pattern GL_VERTICES_SUBMITTED_ARB = 0x82EE