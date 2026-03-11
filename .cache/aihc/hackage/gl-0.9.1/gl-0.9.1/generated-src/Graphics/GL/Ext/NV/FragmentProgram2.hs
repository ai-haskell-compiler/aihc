-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.FragmentProgram2 (
  -- * Extension Support
    gl_NV_fragment_program2

  -- * GL_NV_fragment_program2
  , pattern GL_MAX_PROGRAM_CALL_DEPTH_NV
  , pattern GL_MAX_PROGRAM_EXEC_INSTRUCTIONS_NV
  , pattern GL_MAX_PROGRAM_IF_DEPTH_NV
  , pattern GL_MAX_PROGRAM_LOOP_COUNT_NV
  , pattern GL_MAX_PROGRAM_LOOP_DEPTH_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/fragment_program2.txt GL_NV_fragment_program2> extension is available.

gl_NV_fragment_program2 :: Bool
gl_NV_fragment_program2 = member "GL_NV_fragment_program2" extensions
{-# NOINLINE gl_NV_fragment_program2 #-}

pattern GL_MAX_PROGRAM_IF_DEPTH_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_IF_DEPTH_NV = 0x88F6

pattern GL_MAX_PROGRAM_LOOP_COUNT_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_LOOP_COUNT_NV = 0x88F8

pattern GL_MAX_PROGRAM_LOOP_DEPTH_NV  :: (Eq a, Num a) => a

pattern GL_MAX_PROGRAM_LOOP_DEPTH_NV = 0x88F7