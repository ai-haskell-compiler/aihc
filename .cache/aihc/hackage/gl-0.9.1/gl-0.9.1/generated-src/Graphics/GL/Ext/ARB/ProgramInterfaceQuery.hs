-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ProgramInterfaceQuery (
  -- * Extension Support
    gl_ARB_program_interface_query

  -- * GL_ARB_program_interface_query
  , glGetProgramInterfaceiv
  , glGetProgramResourceIndex
  , glGetProgramResourceLocation
  , glGetProgramResourceLocationIndex
  , glGetProgramResourceName
  , glGetProgramResourceiv
  , pattern GL_ACTIVE_RESOURCES
  , pattern GL_ACTIVE_VARIABLES
  , pattern GL_ARRAY_SIZE
  , pattern GL_ARRAY_STRIDE
  , pattern GL_ATOMIC_COUNTER_BUFFER
  , pattern GL_ATOMIC_COUNTER_BUFFER_INDEX
  , pattern GL_BLOCK_INDEX
  , pattern GL_BUFFER_BINDING
  , pattern GL_BUFFER_DATA_SIZE
  , pattern GL_BUFFER_VARIABLE
  , pattern GL_COMPATIBLE_SUBROUTINES
  , pattern GL_COMPUTE_SUBROUTINE
  , pattern GL_COMPUTE_SUBROUTINE_UNIFORM
  , pattern GL_FRAGMENT_SUBROUTINE
  , pattern GL_FRAGMENT_SUBROUTINE_UNIFORM
  , pattern GL_GEOMETRY_SUBROUTINE
  , pattern GL_GEOMETRY_SUBROUTINE_UNIFORM
  , pattern GL_IS_PER_PATCH
  , pattern GL_IS_ROW_MAJOR
  , pattern GL_LOCATION
  , pattern GL_LOCATION_INDEX
  , pattern GL_MATRIX_STRIDE
  , pattern GL_MAX_NAME_LENGTH
  , pattern GL_MAX_NUM_ACTIVE_VARIABLES
  , pattern GL_MAX_NUM_COMPATIBLE_SUBROUTINES
  , pattern GL_NAME_LENGTH
  , pattern GL_NUM_ACTIVE_VARIABLES
  , pattern GL_NUM_COMPATIBLE_SUBROUTINES
  , pattern GL_OFFSET
  , pattern GL_PROGRAM_INPUT
  , pattern GL_PROGRAM_OUTPUT
  , pattern GL_REFERENCED_BY_COMPUTE_SHADER
  , pattern GL_REFERENCED_BY_FRAGMENT_SHADER
  , pattern GL_REFERENCED_BY_GEOMETRY_SHADER
  , pattern GL_REFERENCED_BY_TESS_CONTROL_SHADER
  , pattern GL_REFERENCED_BY_TESS_EVALUATION_SHADER
  , pattern GL_REFERENCED_BY_VERTEX_SHADER
  , pattern GL_SHADER_STORAGE_BLOCK
  , pattern GL_TESS_CONTROL_SUBROUTINE
  , pattern GL_TESS_CONTROL_SUBROUTINE_UNIFORM
  , pattern GL_TESS_EVALUATION_SUBROUTINE
  , pattern GL_TESS_EVALUATION_SUBROUTINE_UNIFORM
  , pattern GL_TOP_LEVEL_ARRAY_SIZE
  , pattern GL_TOP_LEVEL_ARRAY_STRIDE
  , pattern GL_TRANSFORM_FEEDBACK_VARYING
  , pattern GL_TYPE
  , pattern GL_UNIFORM
  , pattern GL_UNIFORM_BLOCK
  , pattern GL_VERTEX_SUBROUTINE
  , pattern GL_VERTEX_SUBROUTINE_UNIFORM
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/program_interface_query.txt GL_ARB_program_interface_query> extension is available.

gl_ARB_program_interface_query :: Bool
gl_ARB_program_interface_query = member "GL_ARB_program_interface_query" extensions
{-# NOINLINE gl_ARB_program_interface_query #-}