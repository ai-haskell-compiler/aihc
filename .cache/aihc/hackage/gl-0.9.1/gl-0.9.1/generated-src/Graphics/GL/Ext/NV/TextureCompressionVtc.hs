-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TextureCompressionVtc (
  -- * Extension Support
    gl_NV_texture_compression_vtc
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/texture_compression_vtc.txt GL_NV_texture_compression_vtc> extension is available.

gl_NV_texture_compression_vtc :: Bool
gl_NV_texture_compression_vtc = member "GL_NV_texture_compression_vtc" extensions
{-# NOINLINE gl_NV_texture_compression_vtc #-}