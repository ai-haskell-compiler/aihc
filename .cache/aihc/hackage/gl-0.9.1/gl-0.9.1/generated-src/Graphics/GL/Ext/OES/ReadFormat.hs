-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.ReadFormat (
  -- * Extension Support
    gl_OES_read_format

  -- * GL_OES_read_format
  , pattern GL_IMPLEMENTATION_COLOR_READ_FORMAT_OES
  , pattern GL_IMPLEMENTATION_COLOR_READ_TYPE_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_read_format.txt GL_OES_read_format> extension is available.

gl_OES_read_format :: Bool
gl_OES_read_format = member "GL_OES_read_format" extensions
{-# NOINLINE gl_OES_read_format #-}

pattern GL_IMPLEMENTATION_COLOR_READ_FORMAT_OES  :: (Eq a, Num a) => a

pattern GL_IMPLEMENTATION_COLOR_READ_FORMAT_OES = 0x8B9B

pattern GL_IMPLEMENTATION_COLOR_READ_TYPE_OES  :: (Eq a, Num a) => a

pattern GL_IMPLEMENTATION_COLOR_READ_TYPE_OES = 0x8B9A