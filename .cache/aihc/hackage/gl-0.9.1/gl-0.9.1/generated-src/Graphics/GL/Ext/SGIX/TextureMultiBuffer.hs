-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.TextureMultiBuffer (
  -- * Extension Support
    gl_SGIX_texture_multi_buffer

  -- * GL_SGIX_texture_multi_buffer
  , pattern GL_TEXTURE_MULTI_BUFFER_HINT_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/texture_multi_buffer.txt GL_SGIX_texture_multi_buffer> extension is available.

gl_SGIX_texture_multi_buffer :: Bool
gl_SGIX_texture_multi_buffer = member "GL_SGIX_texture_multi_buffer" extensions
{-# NOINLINE gl_SGIX_texture_multi_buffer #-}

pattern GL_TEXTURE_MULTI_BUFFER_HINT_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MULTI_BUFFER_HINT_SGIX = 0x812E