-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.INTEL.ConservativeRasterization (
  -- * Extension Support
    gl_INTEL_conservative_rasterization

  -- * GL_INTEL_conservative_rasterization
  , pattern GL_CONSERVATIVE_RASTERIZATION_INTEL
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/INTEL/conservative_rasterization.txt GL_INTEL_conservative_rasterization> extension is available.

gl_INTEL_conservative_rasterization :: Bool
gl_INTEL_conservative_rasterization = member "GL_INTEL_conservative_rasterization" extensions
{-# NOINLINE gl_INTEL_conservative_rasterization #-}

pattern GL_CONSERVATIVE_RASTERIZATION_INTEL  :: (Eq a, Num a) => a

pattern GL_CONSERVATIVE_RASTERIZATION_INTEL = 0x83FE