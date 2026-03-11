-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ES31Compatibility (
  -- * Extension Support
    gl_ARB_ES3_1_compatibility

  -- * GL_ARB_ES3_1_compatibility
  , glMemoryBarrierByRegion
  , pattern GL_BACK
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/ES3_1_compatibility.txt GL_ARB_ES3_1_compatibility> extension is available.

gl_ARB_ES3_1_compatibility :: Bool
gl_ARB_ES3_1_compatibility = member "GL_ARB_ES3_1_compatibility" extensions
{-# NOINLINE gl_ARB_ES3_1_compatibility #-}