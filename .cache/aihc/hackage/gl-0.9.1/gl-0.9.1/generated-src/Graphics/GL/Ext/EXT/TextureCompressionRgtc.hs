-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureCompressionRgtc (
  -- * Extension Support
    gl_EXT_texture_compression_rgtc

  -- * GL_EXT_texture_compression_rgtc
  , pattern GL_COMPRESSED_RED_GREEN_RGTC2_EXT
  , pattern GL_COMPRESSED_RED_RGTC1_EXT
  , pattern GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT
  , pattern GL_COMPRESSED_SIGNED_RED_RGTC1_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_compression_rgtc.txt GL_EXT_texture_compression_rgtc> extension is available.

gl_EXT_texture_compression_rgtc :: Bool
gl_EXT_texture_compression_rgtc = member "GL_EXT_texture_compression_rgtc" extensions
{-# NOINLINE gl_EXT_texture_compression_rgtc #-}

pattern GL_COMPRESSED_RED_GREEN_RGTC2_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RED_GREEN_RGTC2_EXT = 0x8DBD

pattern GL_COMPRESSED_RED_RGTC1_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_RED_RGTC1_EXT = 0x8DBB

pattern GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT = 0x8DBE

pattern GL_COMPRESSED_SIGNED_RED_RGTC1_EXT  :: (Eq a, Num a) => a

pattern GL_COMPRESSED_SIGNED_RED_RGTC1_EXT = 0x8DBC