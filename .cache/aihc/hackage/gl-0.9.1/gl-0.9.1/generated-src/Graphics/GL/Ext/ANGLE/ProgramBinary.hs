-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ANGLE.ProgramBinary (
  -- * Extension Support
    gl_ANGLE_program_binary

  -- * GL_ANGLE_program_binary
  , pattern GL_PROGRAM_BINARY_ANGLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ANGLE/ANGLE_program_binary.txt GL_ANGLE_program_binary> extension is available.

gl_ANGLE_program_binary :: Bool
gl_ANGLE_program_binary = member "GL_ANGLE_program_binary" extensions
{-# NOINLINE gl_ANGLE_program_binary #-}

pattern GL_PROGRAM_BINARY_ANGLE  :: (Eq a, Num a) => a

pattern GL_PROGRAM_BINARY_ANGLE = 0x93A6