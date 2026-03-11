-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureType2101010REV (
  -- * Extension Support
    gl_EXT_texture_type_2_10_10_10_REV

  -- * GL_EXT_texture_type_2_10_10_10_REV
  , pattern GL_UNSIGNED_INT_2_10_10_10_REV_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_type_2_10_10_10_REV.txt GL_EXT_texture_type_2_10_10_10_REV> extension is available.

gl_EXT_texture_type_2_10_10_10_REV :: Bool
gl_EXT_texture_type_2_10_10_10_REV = member "GL_EXT_texture_type_2_10_10_10_REV" extensions
{-# NOINLINE gl_EXT_texture_type_2_10_10_10_REV #-}

pattern GL_UNSIGNED_INT_2_10_10_10_REV_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_2_10_10_10_REV_EXT = 0x8368