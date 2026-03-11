-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.UnpackSubimage (
  -- * Extension Support
    gl_EXT_unpack_subimage

  -- * GL_EXT_unpack_subimage
  , pattern GL_UNPACK_ROW_LENGTH_EXT
  , pattern GL_UNPACK_SKIP_PIXELS_EXT
  , pattern GL_UNPACK_SKIP_ROWS_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_unpack_subimage.txt GL_EXT_unpack_subimage> extension is available.

gl_EXT_unpack_subimage :: Bool
gl_EXT_unpack_subimage = member "GL_EXT_unpack_subimage" extensions
{-# NOINLINE gl_EXT_unpack_subimage #-}

pattern GL_UNPACK_ROW_LENGTH_EXT  :: (Eq a, Num a) => a

pattern GL_UNPACK_ROW_LENGTH_EXT = 0x0CF2

pattern GL_UNPACK_SKIP_PIXELS_EXT  :: (Eq a, Num a) => a

pattern GL_UNPACK_SKIP_PIXELS_EXT = 0x0CF4

pattern GL_UNPACK_SKIP_ROWS_EXT  :: (Eq a, Num a) => a

pattern GL_UNPACK_SKIP_ROWS_EXT = 0x0CF3