-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.BlendAlphaMinmax (
  -- * Extension Support
    gl_SGIX_blend_alpha_minmax

  -- * GL_SGIX_blend_alpha_minmax
  , pattern GL_ALPHA_MAX_SGIX
  , pattern GL_ALPHA_MIN_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/blend_alpha_minmax.txt GL_SGIX_blend_alpha_minmax> extension is available.

gl_SGIX_blend_alpha_minmax :: Bool
gl_SGIX_blend_alpha_minmax = member "GL_SGIX_blend_alpha_minmax" extensions
{-# NOINLINE gl_SGIX_blend_alpha_minmax #-}

pattern GL_ALPHA_MAX_SGIX  :: (Eq a, Num a) => a

pattern GL_ALPHA_MAX_SGIX = 0x8321

pattern GL_ALPHA_MIN_SGIX  :: (Eq a, Num a) => a

pattern GL_ALPHA_MIN_SGIX = 0x8320