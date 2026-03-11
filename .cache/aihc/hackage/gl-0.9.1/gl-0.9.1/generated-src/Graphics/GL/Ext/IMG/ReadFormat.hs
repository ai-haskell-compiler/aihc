-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IMG.ReadFormat (
  -- * Extension Support
    gl_IMG_read_format

  -- * GL_IMG_read_format
  , pattern GL_BGRA_IMG
  , pattern GL_UNSIGNED_SHORT_4_4_4_4_REV_IMG
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/IMG/IMG_read_format.txt GL_IMG_read_format> extension is available.

gl_IMG_read_format :: Bool
gl_IMG_read_format = member "GL_IMG_read_format" extensions
{-# NOINLINE gl_IMG_read_format #-}

pattern GL_BGRA_IMG  :: (Eq a, Num a) => a

pattern GL_BGRA_IMG = 0x80E1

pattern GL_UNSIGNED_SHORT_4_4_4_4_REV_IMG  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_SHORT_4_4_4_4_REV_IMG = 0x8365