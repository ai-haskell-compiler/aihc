-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.GetProgramBinary (
  -- * Extension Support
    gl_ARB_get_program_binary

  -- * GL_ARB_get_program_binary
  , glGetProgramBinary
  , glProgramBinary
  , glProgramParameteri
  , pattern GL_NUM_PROGRAM_BINARY_FORMATS
  , pattern GL_PROGRAM_BINARY_FORMATS
  , pattern GL_PROGRAM_BINARY_LENGTH
  , pattern GL_PROGRAM_BINARY_RETRIEVABLE_HINT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/get_program_binary.txt GL_ARB_get_program_binary> extension is available.

gl_ARB_get_program_binary :: Bool
gl_ARB_get_program_binary = member "GL_ARB_get_program_binary" extensions
{-# NOINLINE gl_ARB_get_program_binary #-}