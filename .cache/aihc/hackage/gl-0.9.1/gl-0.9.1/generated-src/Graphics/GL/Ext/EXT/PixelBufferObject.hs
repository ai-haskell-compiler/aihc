-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PixelBufferObject (
  -- * Extension Support
    gl_EXT_pixel_buffer_object

  -- * GL_EXT_pixel_buffer_object
  , pattern GL_PIXEL_PACK_BUFFER_BINDING_EXT
  , pattern GL_PIXEL_PACK_BUFFER_EXT
  , pattern GL_PIXEL_UNPACK_BUFFER_BINDING_EXT
  , pattern GL_PIXEL_UNPACK_BUFFER_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/pixel_buffer_object.txt GL_EXT_pixel_buffer_object> extension is available.

gl_EXT_pixel_buffer_object :: Bool
gl_EXT_pixel_buffer_object = member "GL_EXT_pixel_buffer_object" extensions
{-# NOINLINE gl_EXT_pixel_buffer_object #-}

pattern GL_PIXEL_PACK_BUFFER_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_PIXEL_PACK_BUFFER_BINDING_EXT = 0x88ED

pattern GL_PIXEL_PACK_BUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_PIXEL_PACK_BUFFER_EXT = 0x88EB

pattern GL_PIXEL_UNPACK_BUFFER_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_PIXEL_UNPACK_BUFFER_BINDING_EXT = 0x88EF

pattern GL_PIXEL_UNPACK_BUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_PIXEL_UNPACK_BUFFER_EXT = 0x88EC