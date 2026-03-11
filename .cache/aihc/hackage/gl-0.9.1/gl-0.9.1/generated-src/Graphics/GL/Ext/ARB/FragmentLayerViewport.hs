-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.FragmentLayerViewport (
  -- * Extension Support
    gl_ARB_fragment_layer_viewport
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/fragment_layer_viewport.txt GL_ARB_fragment_layer_viewport> extension is available.

gl_ARB_fragment_layer_viewport :: Bool
gl_ARB_fragment_layer_viewport = member "GL_ARB_fragment_layer_viewport" extensions
{-# NOINLINE gl_ARB_fragment_layer_viewport #-}