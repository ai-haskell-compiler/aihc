-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SUN.SliceAccum (
  -- * Extension Support
    gl_SUN_slice_accum

  -- * GL_SUN_slice_accum
  , pattern GL_SLICE_ACCUM_SUN
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SUN/slice_accum.txt GL_SUN_slice_accum> extension is available.

gl_SUN_slice_accum :: Bool
gl_SUN_slice_accum = member "GL_SUN_slice_accum" extensions
{-# NOINLINE gl_SUN_slice_accum #-}

pattern GL_SLICE_ACCUM_SUN  :: (Eq a, Num a) => a

pattern GL_SLICE_ACCUM_SUN = 0x85CC