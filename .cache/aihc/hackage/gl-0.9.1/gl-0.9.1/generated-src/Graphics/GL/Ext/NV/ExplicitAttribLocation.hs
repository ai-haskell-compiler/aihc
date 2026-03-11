-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ExplicitAttribLocation (
  -- * Extension Support
    gl_NV_explicit_attrib_location
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_explicit_attrib_location.txt GL_NV_explicit_attrib_location> extension is available.

gl_NV_explicit_attrib_location :: Bool
gl_NV_explicit_attrib_location = member "GL_NV_explicit_attrib_location" extensions
{-# NOINLINE gl_NV_explicit_attrib_location #-}