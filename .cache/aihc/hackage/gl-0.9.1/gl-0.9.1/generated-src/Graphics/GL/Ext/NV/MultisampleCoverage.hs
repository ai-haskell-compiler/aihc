-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.MultisampleCoverage (
  -- * Extension Support
    gl_NV_multisample_coverage

  -- * GL_NV_multisample_coverage
  , pattern GL_COLOR_SAMPLES_NV
  , pattern GL_SAMPLES_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/multisample_coverage.txt GL_NV_multisample_coverage> extension is available.

gl_NV_multisample_coverage :: Bool
gl_NV_multisample_coverage = member "GL_NV_multisample_coverage" extensions
{-# NOINLINE gl_NV_multisample_coverage #-}