-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.OcclusionQuery2 (
  -- * Extension Support
    gl_ARB_occlusion_query2

  -- * GL_ARB_occlusion_query2
  , pattern GL_ANY_SAMPLES_PASSED
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/occlusion_query2.txt GL_ARB_occlusion_query2> extension is available.

gl_ARB_occlusion_query2 :: Bool
gl_ARB_occlusion_query2 = member "GL_ARB_occlusion_query2" extensions
{-# NOINLINE gl_ARB_occlusion_query2 #-}