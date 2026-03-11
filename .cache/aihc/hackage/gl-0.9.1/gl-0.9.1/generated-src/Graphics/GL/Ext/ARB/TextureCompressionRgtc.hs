-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.TextureCompressionRgtc (
  -- * Extension Support
    gl_ARB_texture_compression_rgtc

  -- * GL_ARB_texture_compression_rgtc
  , pattern GL_COMPRESSED_RED_RGTC1
  , pattern GL_COMPRESSED_RG_RGTC2
  , pattern GL_COMPRESSED_SIGNED_RED_RGTC1
  , pattern GL_COMPRESSED_SIGNED_RG_RGTC2
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/texture_compression_rgtc.txt GL_ARB_texture_compression_rgtc> extension is available.

gl_ARB_texture_compression_rgtc :: Bool
gl_ARB_texture_compression_rgtc = member "GL_ARB_texture_compression_rgtc" extensions
{-# NOINLINE gl_ARB_texture_compression_rgtc #-}