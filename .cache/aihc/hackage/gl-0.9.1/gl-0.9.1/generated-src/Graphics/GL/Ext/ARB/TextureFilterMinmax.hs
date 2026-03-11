-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureFilterMinmax (
  -- * Extension Support
    gl_ARB_texture_filter_minmax

  -- * GL_ARB_texture_filter_minmax
  , pattern GL_TEXTURE_REDUCTION_MODE_ARB
  , pattern GL_WEIGHTED_AVERAGE_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_filter_minmax.txt GL_ARB_texture_filter_minmax> extension is available.

gl_ARB_texture_filter_minmax :: Bool
gl_ARB_texture_filter_minmax = member "GL_ARB_texture_filter_minmax" extensions
{-# NOINLINE gl_ARB_texture_filter_minmax #-}

pattern GL_TEXTURE_REDUCTION_MODE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_REDUCTION_MODE_ARB = 0x9366

pattern GL_WEIGHTED_AVERAGE_ARB  :: (Eq a, Num a) => a

pattern GL_WEIGHTED_AVERAGE_ARB = 0x9367