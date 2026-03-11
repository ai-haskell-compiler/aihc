-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PackedDepthStencil (
  -- * Extension Support
    gl_EXT_packed_depth_stencil

  -- * GL_EXT_packed_depth_stencil
  , pattern GL_DEPTH24_STENCIL8_EXT
  , pattern GL_DEPTH_STENCIL_EXT
  , pattern GL_TEXTURE_STENCIL_SIZE_EXT
  , pattern GL_UNSIGNED_INT_24_8_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/packed_depth_stencil.txt GL_EXT_packed_depth_stencil> extension is available.

gl_EXT_packed_depth_stencil :: Bool
gl_EXT_packed_depth_stencil = member "GL_EXT_packed_depth_stencil" extensions
{-# NOINLINE gl_EXT_packed_depth_stencil #-}

pattern GL_DEPTH24_STENCIL8_EXT  :: (Eq a, Num a) => a

pattern GL_DEPTH24_STENCIL8_EXT = 0x88F0

pattern GL_DEPTH_STENCIL_EXT  :: (Eq a, Num a) => a

pattern GL_DEPTH_STENCIL_EXT = 0x84F9

pattern GL_TEXTURE_STENCIL_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_STENCIL_SIZE_EXT = 0x88F1

pattern GL_UNSIGNED_INT_24_8_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_24_8_EXT = 0x84FA