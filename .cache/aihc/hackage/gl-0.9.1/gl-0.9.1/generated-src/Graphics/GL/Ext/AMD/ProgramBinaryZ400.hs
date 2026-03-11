-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.ProgramBinaryZ400 (
  -- * Extension Support
    gl_AMD_program_binary_Z400

  -- * GL_AMD_program_binary_Z400
  , pattern GL_Z400_BINARY_AMD
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/AMD/AMD_program_binary_Z400.txt GL_AMD_program_binary_Z400> extension is available.

gl_AMD_program_binary_Z400 :: Bool
gl_AMD_program_binary_Z400 = member "GL_AMD_program_binary_Z400" extensions
{-# NOINLINE gl_AMD_program_binary_Z400 #-}

pattern GL_Z400_BINARY_AMD  :: (Eq a, Num a) => a

pattern GL_Z400_BINARY_AMD = 0x8740