-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ES2Compatibility (
  -- * Extension Support
    gl_ARB_ES2_compatibility

  -- * GL_ARB_ES2_compatibility
  , glClearDepthf
  , glDepthRangef
  , glGetShaderPrecisionFormat
  , glReleaseShaderCompiler
  , glShaderBinary
  , pattern GL_FIXED
  , pattern GL_HIGH_FLOAT
  , pattern GL_HIGH_INT
  , pattern GL_IMPLEMENTATION_COLOR_READ_FORMAT
  , pattern GL_IMPLEMENTATION_COLOR_READ_TYPE
  , pattern GL_LOW_FLOAT
  , pattern GL_LOW_INT
  , pattern GL_MAX_FRAGMENT_UNIFORM_VECTORS
  , pattern GL_MAX_VARYING_VECTORS
  , pattern GL_MAX_VERTEX_UNIFORM_VECTORS
  , pattern GL_MEDIUM_FLOAT
  , pattern GL_MEDIUM_INT
  , pattern GL_NUM_SHADER_BINARY_FORMATS
  , pattern GL_RGB565
  , pattern GL_SHADER_BINARY_FORMATS
  , pattern GL_SHADER_COMPILER
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/ES2_compatibility.txt GL_ARB_ES2_compatibility> extension is available.

gl_ARB_ES2_compatibility :: Bool
gl_ARB_ES2_compatibility = member "GL_ARB_ES2_compatibility" extensions
{-# NOINLINE gl_ARB_ES2_compatibility #-}