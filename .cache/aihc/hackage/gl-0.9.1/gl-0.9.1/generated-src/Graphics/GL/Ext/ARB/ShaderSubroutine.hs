-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShaderSubroutine (
  -- * Extension Support
    gl_ARB_shader_subroutine

  -- * GL_ARB_shader_subroutine
  , glGetActiveSubroutineName
  , glGetActiveSubroutineUniformName
  , glGetActiveSubroutineUniformiv
  , glGetProgramStageiv
  , glGetSubroutineIndex
  , glGetSubroutineUniformLocation
  , glGetUniformSubroutineuiv
  , glUniformSubroutinesuiv
  , pattern GL_ACTIVE_SUBROUTINES
  , pattern GL_ACTIVE_SUBROUTINE_MAX_LENGTH
  , pattern GL_ACTIVE_SUBROUTINE_UNIFORMS
  , pattern GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS
  , pattern GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH
  , pattern GL_COMPATIBLE_SUBROUTINES
  , pattern GL_MAX_SUBROUTINES
  , pattern GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS
  , pattern GL_NUM_COMPATIBLE_SUBROUTINES
  , pattern GL_UNIFORM_NAME_LENGTH
  , pattern GL_UNIFORM_SIZE
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shader_subroutine.txt GL_ARB_shader_subroutine> extension is available.

gl_ARB_shader_subroutine :: Bool
gl_ARB_shader_subroutine = member "GL_ARB_shader_subroutine" extensions
{-# NOINLINE gl_ARB_shader_subroutine #-}