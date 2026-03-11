-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.AsyncPixel (
  -- * Extension Support
    gl_SGIX_async_pixel

  -- * GL_SGIX_async_pixel
  , pattern GL_ASYNC_DRAW_PIXELS_SGIX
  , pattern GL_ASYNC_READ_PIXELS_SGIX
  , pattern GL_ASYNC_TEX_IMAGE_SGIX
  , pattern GL_MAX_ASYNC_DRAW_PIXELS_SGIX
  , pattern GL_MAX_ASYNC_READ_PIXELS_SGIX
  , pattern GL_MAX_ASYNC_TEX_IMAGE_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/async_pixel.txt GL_SGIX_async_pixel> extension is available.

gl_SGIX_async_pixel :: Bool
gl_SGIX_async_pixel = member "GL_SGIX_async_pixel" extensions
{-# NOINLINE gl_SGIX_async_pixel #-}

pattern GL_ASYNC_DRAW_PIXELS_SGIX  :: (Eq a, Num a) => a

pattern GL_ASYNC_DRAW_PIXELS_SGIX = 0x835D

pattern GL_ASYNC_READ_PIXELS_SGIX  :: (Eq a, Num a) => a

pattern GL_ASYNC_READ_PIXELS_SGIX = 0x835E

pattern GL_ASYNC_TEX_IMAGE_SGIX  :: (Eq a, Num a) => a

pattern GL_ASYNC_TEX_IMAGE_SGIX = 0x835C

pattern GL_MAX_ASYNC_DRAW_PIXELS_SGIX  :: (Eq a, Num a) => a

pattern GL_MAX_ASYNC_DRAW_PIXELS_SGIX = 0x8360

pattern GL_MAX_ASYNC_READ_PIXELS_SGIX  :: (Eq a, Num a) => a

pattern GL_MAX_ASYNC_READ_PIXELS_SGIX = 0x8361

pattern GL_MAX_ASYNC_TEX_IMAGE_SGIX  :: (Eq a, Num a) => a

pattern GL_MAX_ASYNC_TEX_IMAGE_SGIX = 0x835F