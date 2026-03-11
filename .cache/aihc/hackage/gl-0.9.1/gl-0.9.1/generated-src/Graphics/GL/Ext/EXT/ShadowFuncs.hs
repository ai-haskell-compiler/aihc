-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShadowFuncs (
  -- * Extension Support
    gl_EXT_shadow_funcs
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/shadow_funcs.txt GL_EXT_shadow_funcs> extension is available.

gl_EXT_shadow_funcs :: Bool
gl_EXT_shadow_funcs = member "GL_EXT_shadow_funcs" extensions
{-# NOINLINE gl_EXT_shadow_funcs #-}