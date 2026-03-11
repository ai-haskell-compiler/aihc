-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.FragmentProgramOption (
  -- * Extension Support
    gl_NV_fragment_program_option
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/fragment_program_option.txt GL_NV_fragment_program_option> extension is available.

gl_NV_fragment_program_option :: Bool
gl_NV_fragment_program_option = member "GL_NV_fragment_program_option" extensions
{-# NOINLINE gl_NV_fragment_program_option #-}