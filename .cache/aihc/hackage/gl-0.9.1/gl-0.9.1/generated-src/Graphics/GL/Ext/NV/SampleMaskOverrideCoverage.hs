-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.SampleMaskOverrideCoverage (
  -- * Extension Support
    gl_NV_sample_mask_override_coverage
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/sample_mask_override_coverage.txt GL_NV_sample_mask_override_coverage> extension is available.

gl_NV_sample_mask_override_coverage :: Bool
gl_NV_sample_mask_override_coverage = member "GL_NV_sample_mask_override_coverage" extensions
{-# NOINLINE gl_NV_sample_mask_override_coverage #-}