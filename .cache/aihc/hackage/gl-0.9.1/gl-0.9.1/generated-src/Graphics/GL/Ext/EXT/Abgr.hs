-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.Abgr (
  -- * Extension Support
    gl_EXT_abgr

  -- * GL_EXT_abgr
  , pattern GL_ABGR_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/abgr.txt GL_EXT_abgr> extension is available.

gl_EXT_abgr :: Bool
gl_EXT_abgr = member "GL_EXT_abgr" extensions
{-# NOINLINE gl_EXT_abgr #-}

pattern GL_ABGR_EXT  :: (Eq a, Num a) => a

pattern GL_ABGR_EXT = 0x8000