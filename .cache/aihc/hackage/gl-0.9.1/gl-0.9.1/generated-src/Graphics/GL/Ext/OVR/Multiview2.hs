-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OVR.Multiview2 (
  -- * Extension Support
    gl_OVR_multiview2
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OVR/multiview2.txt GL_OVR_multiview2> extension is available.

gl_OVR_multiview2 :: Bool
gl_OVR_multiview2 = member "GL_OVR_multiview2" extensions
{-# NOINLINE gl_OVR_multiview2 #-}