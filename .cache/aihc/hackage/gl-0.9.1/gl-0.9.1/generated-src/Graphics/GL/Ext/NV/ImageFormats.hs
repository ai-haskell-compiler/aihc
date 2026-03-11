-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ImageFormats (
  -- * Extension Support
    gl_NV_image_formats
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_NV_image_formats extension is available.

gl_NV_image_formats :: Bool
gl_NV_image_formats = member "GL_NV_image_formats" extensions
{-# NOINLINE gl_NV_image_formats #-}