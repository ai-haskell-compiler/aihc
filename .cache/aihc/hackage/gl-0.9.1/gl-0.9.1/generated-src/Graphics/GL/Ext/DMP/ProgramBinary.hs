-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.DMP.ProgramBinary (
  -- * Extension Support
    gl_DMP_program_binary

  -- * GL_DMP_program_binary
  , pattern GL_DMP_PROGRAM_BINARY_DMP
  , pattern GL_SMAPHS30_PROGRAM_BINARY_DMP
  , pattern GL_SMAPHS_PROGRAM_BINARY_DMP
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/DMP/DMP_program_binary.txt GL_DMP_program_binary> extension is available.

gl_DMP_program_binary :: Bool
gl_DMP_program_binary = member "GL_DMP_program_binary" extensions
{-# NOINLINE gl_DMP_program_binary #-}

pattern GL_DMP_PROGRAM_BINARY_DMP  :: (Eq a, Num a) => a

pattern GL_DMP_PROGRAM_BINARY_DMP = 0x9253

pattern GL_SMAPHS30_PROGRAM_BINARY_DMP  :: (Eq a, Num a) => a

pattern GL_SMAPHS30_PROGRAM_BINARY_DMP = 0x9251

pattern GL_SMAPHS_PROGRAM_BINARY_DMP  :: (Eq a, Num a) => a

pattern GL_SMAPHS_PROGRAM_BINARY_DMP = 0x9252