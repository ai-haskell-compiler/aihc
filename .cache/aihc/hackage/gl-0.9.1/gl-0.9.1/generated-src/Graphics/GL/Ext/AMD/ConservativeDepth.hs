-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.ConservativeDepth (
  -- * Extension Support
    gl_AMD_conservative_depth
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/conservative_depth.txt GL_AMD_conservative_depth> extension is available.

gl_AMD_conservative_depth :: Bool
gl_AMD_conservative_depth = member "GL_AMD_conservative_depth" extensions
{-# NOINLINE gl_AMD_conservative_depth #-}