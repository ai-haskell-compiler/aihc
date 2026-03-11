-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ShaderPixelLocalStorage (
  -- * Extension Support
    gl_EXT_shader_pixel_local_storage

  -- * GL_EXT_shader_pixel_local_storage
  , pattern GL_MAX_SHADER_PIXEL_LOCAL_STORAGE_FAST_SIZE_EXT
  , pattern GL_MAX_SHADER_PIXEL_LOCAL_STORAGE_SIZE_EXT
  , pattern GL_SHADER_PIXEL_LOCAL_STORAGE_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_shader_pixel_local_storage.txt GL_EXT_shader_pixel_local_storage> extension is available.

gl_EXT_shader_pixel_local_storage :: Bool
gl_EXT_shader_pixel_local_storage = member "GL_EXT_shader_pixel_local_storage" extensions
{-# NOINLINE gl_EXT_shader_pixel_local_storage #-}

pattern GL_MAX_SHADER_PIXEL_LOCAL_STORAGE_FAST_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_SHADER_PIXEL_LOCAL_STORAGE_FAST_SIZE_EXT = 0x8F63

pattern GL_MAX_SHADER_PIXEL_LOCAL_STORAGE_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_SHADER_PIXEL_LOCAL_STORAGE_SIZE_EXT = 0x8F67

pattern GL_SHADER_PIXEL_LOCAL_STORAGE_EXT  :: (Eq a, Num a) => a

pattern GL_SHADER_PIXEL_LOCAL_STORAGE_EXT = 0x8F64