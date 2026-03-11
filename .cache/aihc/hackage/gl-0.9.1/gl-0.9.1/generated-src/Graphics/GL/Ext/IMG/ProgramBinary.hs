-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IMG.ProgramBinary (
  -- * Extension Support
    gl_IMG_program_binary

  -- * GL_IMG_program_binary
  , pattern GL_SGX_PROGRAM_BINARY_IMG
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/IMG/IMG_program_binary.txt GL_IMG_program_binary> extension is available.

gl_IMG_program_binary :: Bool
gl_IMG_program_binary = member "GL_IMG_program_binary" extensions
{-# NOINLINE gl_IMG_program_binary #-}

pattern GL_SGX_PROGRAM_BINARY_IMG  :: (Eq a, Num a) => a

pattern GL_SGX_PROGRAM_BINARY_IMG = 0x9130