-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.CompressedTexturePixelStorage (
  -- * Extension Support
    gl_ARB_compressed_texture_pixel_storage

  -- * GL_ARB_compressed_texture_pixel_storage
  , pattern GL_PACK_COMPRESSED_BLOCK_DEPTH
  , pattern GL_PACK_COMPRESSED_BLOCK_HEIGHT
  , pattern GL_PACK_COMPRESSED_BLOCK_SIZE
  , pattern GL_PACK_COMPRESSED_BLOCK_WIDTH
  , pattern GL_UNPACK_COMPRESSED_BLOCK_DEPTH
  , pattern GL_UNPACK_COMPRESSED_BLOCK_HEIGHT
  , pattern GL_UNPACK_COMPRESSED_BLOCK_SIZE
  , pattern GL_UNPACK_COMPRESSED_BLOCK_WIDTH
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/compressed_texture_pixel_storage.txt GL_ARB_compressed_texture_pixel_storage> extension is available.

gl_ARB_compressed_texture_pixel_storage :: Bool
gl_ARB_compressed_texture_pixel_storage = member "GL_ARB_compressed_texture_pixel_storage" extensions
{-# NOINLINE gl_ARB_compressed_texture_pixel_storage #-}