-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.CompressedETC1RGB8Texture (
  -- * Extension Support
    gl_OES_compressed_ETC1_RGB8_texture

  -- * GL_OES_compressed_ETC1_RGB8_texture
  , pattern GL_ETC1_RGB8_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_compressed_ETC1_RGB8_texture.txt GL_OES_compressed_ETC1_RGB8_texture> extension is available.

gl_OES_compressed_ETC1_RGB8_texture :: Bool
gl_OES_compressed_ETC1_RGB8_texture = member "GL_OES_compressed_ETC1_RGB8_texture" extensions
{-# NOINLINE gl_OES_compressed_ETC1_RGB8_texture #-}

pattern GL_ETC1_RGB8_OES  :: (Eq a, Num a) => a

pattern GL_ETC1_RGB8_OES = 0x8D64