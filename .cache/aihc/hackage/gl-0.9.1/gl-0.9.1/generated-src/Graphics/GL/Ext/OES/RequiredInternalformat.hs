-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.RequiredInternalformat (
  -- * Extension Support
    gl_OES_required_internalformat

  -- * GL_OES_required_internalformat
  , pattern GL_ALPHA8_OES
  , pattern GL_DEPTH24_STENCIL8_OES
  , pattern GL_DEPTH_COMPONENT16_OES
  , pattern GL_DEPTH_COMPONENT24_OES
  , pattern GL_DEPTH_COMPONENT32_OES
  , pattern GL_LUMINANCE4_ALPHA4_OES
  , pattern GL_LUMINANCE8_ALPHA8_OES
  , pattern GL_LUMINANCE8_OES
  , pattern GL_RGB10_A2_EXT
  , pattern GL_RGB10_EXT
  , pattern GL_RGB565_OES
  , pattern GL_RGB5_A1_OES
  , pattern GL_RGB8_OES
  , pattern GL_RGBA4_OES
  , pattern GL_RGBA8_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_required_internalformat.txt GL_OES_required_internalformat> extension is available.

gl_OES_required_internalformat :: Bool
gl_OES_required_internalformat = member "GL_OES_required_internalformat" extensions
{-# NOINLINE gl_OES_required_internalformat #-}

pattern GL_ALPHA8_OES  :: (Eq a, Num a) => a

pattern GL_ALPHA8_OES = 0x803C

pattern GL_LUMINANCE4_ALPHA4_OES  :: (Eq a, Num a) => a

pattern GL_LUMINANCE4_ALPHA4_OES = 0x8043

pattern GL_LUMINANCE8_ALPHA8_OES  :: (Eq a, Num a) => a

pattern GL_LUMINANCE8_ALPHA8_OES = 0x8045

pattern GL_LUMINANCE8_OES  :: (Eq a, Num a) => a

pattern GL_LUMINANCE8_OES = 0x8040