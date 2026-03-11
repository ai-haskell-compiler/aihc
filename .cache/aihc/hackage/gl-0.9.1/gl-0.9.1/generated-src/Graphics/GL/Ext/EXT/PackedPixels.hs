-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PackedPixels (
  -- * Extension Support
    gl_EXT_packed_pixels

  -- * GL_EXT_packed_pixels
  , pattern GL_UNSIGNED_BYTE_3_3_2_EXT
  , pattern GL_UNSIGNED_INT_10_10_10_2_EXT
  , pattern GL_UNSIGNED_INT_8_8_8_8_EXT
  , pattern GL_UNSIGNED_SHORT_4_4_4_4_EXT
  , pattern GL_UNSIGNED_SHORT_5_5_5_1_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/packed_pixels.txt GL_EXT_packed_pixels> extension is available.

gl_EXT_packed_pixels :: Bool
gl_EXT_packed_pixels = member "GL_EXT_packed_pixels" extensions
{-# NOINLINE gl_EXT_packed_pixels #-}

pattern GL_UNSIGNED_BYTE_3_3_2_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_BYTE_3_3_2_EXT = 0x8032

pattern GL_UNSIGNED_INT_10_10_10_2_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_10_10_10_2_EXT = 0x8036

pattern GL_UNSIGNED_INT_8_8_8_8_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_8_8_8_8_EXT = 0x8035

pattern GL_UNSIGNED_SHORT_4_4_4_4_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_SHORT_4_4_4_4_EXT = 0x8033

pattern GL_UNSIGNED_SHORT_5_5_5_1_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_SHORT_5_5_5_1_EXT = 0x8034