-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.PixelFormatFloat (
  -- * Extension Support
    gl_ATI_pixel_format_float

  -- * GL_ATI_pixel_format_float
  , pattern GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI
  , pattern GL_RGBA_FLOAT_MODE_ATI
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/pixel_format_float.txt GL_ATI_pixel_format_float> extension is available.

gl_ATI_pixel_format_float :: Bool
gl_ATI_pixel_format_float = member "GL_ATI_pixel_format_float" extensions
{-# NOINLINE gl_ATI_pixel_format_float #-}

pattern GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI  :: (Eq a, Num a) => a

pattern GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI = 0x8835

pattern GL_RGBA_FLOAT_MODE_ATI  :: (Eq a, Num a) => a

pattern GL_RGBA_FLOAT_MODE_ATI = 0x8820