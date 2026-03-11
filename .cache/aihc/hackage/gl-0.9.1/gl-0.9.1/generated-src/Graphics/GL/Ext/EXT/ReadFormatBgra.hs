-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ReadFormatBgra (
  -- * Extension Support
    gl_EXT_read_format_bgra

  -- * GL_EXT_read_format_bgra
  , pattern GL_BGRA_EXT
  , pattern GL_UNSIGNED_SHORT_1_5_5_5_REV_EXT
  , pattern GL_UNSIGNED_SHORT_4_4_4_4_REV_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_read_format_bgra.txt GL_EXT_read_format_bgra> extension is available.

gl_EXT_read_format_bgra :: Bool
gl_EXT_read_format_bgra = member "GL_EXT_read_format_bgra" extensions
{-# NOINLINE gl_EXT_read_format_bgra #-}

pattern GL_UNSIGNED_SHORT_1_5_5_5_REV_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_SHORT_1_5_5_5_REV_EXT = 0x8366

pattern GL_UNSIGNED_SHORT_4_4_4_4_REV_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_SHORT_4_4_4_4_REV_EXT = 0x8365