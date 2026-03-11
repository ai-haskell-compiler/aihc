-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureBufferObject (
  -- * Extension Support
    gl_EXT_texture_buffer_object

  -- * GL_EXT_texture_buffer_object
  , glTexBufferEXT
  , pattern GL_MAX_TEXTURE_BUFFER_SIZE_EXT
  , pattern GL_TEXTURE_BINDING_BUFFER_EXT
  , pattern GL_TEXTURE_BUFFER_DATA_STORE_BINDING_EXT
  , pattern GL_TEXTURE_BUFFER_EXT
  , pattern GL_TEXTURE_BUFFER_FORMAT_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/texture_buffer_object.txt GL_EXT_texture_buffer_object> extension is available.

gl_EXT_texture_buffer_object :: Bool
gl_EXT_texture_buffer_object = member "GL_EXT_texture_buffer_object" extensions
{-# NOINLINE gl_EXT_texture_buffer_object #-}

pattern GL_TEXTURE_BUFFER_FORMAT_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BUFFER_FORMAT_EXT = 0x8C2E