-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TessellationShader (
  -- * Extension Support
    gl_ARB_tessellation_shader

  -- * GL_ARB_tessellation_shader
  , glPatchParameterfv
  , glPatchParameteri
  , pattern GL_CCW
  , pattern GL_CW
  , pattern GL_EQUAL
  , pattern GL_FRACTIONAL_EVEN
  , pattern GL_FRACTIONAL_ODD
  , pattern GL_ISOLINES
  , pattern GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS
  , pattern GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS
  , pattern GL_MAX_PATCH_VERTICES
  , pattern GL_MAX_TESS_CONTROL_INPUT_COMPONENTS
  , pattern GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS
  , pattern GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS
  , pattern GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS
  , pattern GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS
  , pattern GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS
  , pattern GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS
  , pattern GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS
  , pattern GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS
  , pattern GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS
  , pattern GL_MAX_TESS_GEN_LEVEL
  , pattern GL_MAX_TESS_PATCH_COMPONENTS
  , pattern GL_PATCHES
  , pattern GL_PATCH_DEFAULT_INNER_LEVEL
  , pattern GL_PATCH_DEFAULT_OUTER_LEVEL
  , pattern GL_PATCH_VERTICES
  , pattern GL_QUADS
  , pattern GL_TESS_CONTROL_OUTPUT_VERTICES
  , pattern GL_TESS_CONTROL_SHADER
  , pattern GL_TESS_EVALUATION_SHADER
  , pattern GL_TESS_GEN_MODE
  , pattern GL_TESS_GEN_POINT_MODE
  , pattern GL_TESS_GEN_SPACING
  , pattern GL_TESS_GEN_VERTEX_ORDER
  , pattern GL_TRIANGLES
  , pattern GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER
  , pattern GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/tessellation_shader.txt GL_ARB_tessellation_shader> extension is available.

gl_ARB_tessellation_shader :: Bool
gl_ARB_tessellation_shader = member "GL_ARB_tessellation_shader" extensions
{-# NOINLINE gl_ARB_tessellation_shader #-}