-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureBorderClamp (
  -- * Extension Support
    gl_ARB_texture_border_clamp

  -- * GL_ARB_texture_border_clamp
  , pattern GL_CLAMP_TO_BORDER_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_border_clamp.txt GL_ARB_texture_border_clamp> extension is available.

gl_ARB_texture_border_clamp :: Bool
gl_ARB_texture_border_clamp = member "GL_ARB_texture_border_clamp" extensions
{-# NOINLINE gl_ARB_texture_border_clamp #-}

pattern GL_CLAMP_TO_BORDER_ARB  :: (Eq a, Num a) => a

pattern GL_CLAMP_TO_BORDER_ARB = 0x812D