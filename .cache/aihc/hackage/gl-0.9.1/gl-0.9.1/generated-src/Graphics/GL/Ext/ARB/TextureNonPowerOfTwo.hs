-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureNonPowerOfTwo (
  -- * Extension Support
    gl_ARB_texture_non_power_of_two
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_non_power_of_two.txt GL_ARB_texture_non_power_of_two> extension is available.

gl_ARB_texture_non_power_of_two :: Bool
gl_ARB_texture_non_power_of_two = member "GL_ARB_texture_non_power_of_two" extensions
{-# NOINLINE gl_ARB_texture_non_power_of_two #-}