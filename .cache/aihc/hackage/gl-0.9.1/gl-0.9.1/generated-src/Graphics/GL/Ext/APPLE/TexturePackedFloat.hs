-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.TexturePackedFloat (
  -- * Extension Support
    gl_APPLE_texture_packed_float

  -- * GL_APPLE_texture_packed_float
  , pattern GL_R11F_G11F_B10F_APPLE
  , pattern GL_RGB9_E5_APPLE
  , pattern GL_UNSIGNED_INT_10F_11F_11F_REV_APPLE
  , pattern GL_UNSIGNED_INT_5_9_9_9_REV_APPLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/APPLE/APPLE_texture_packed_float.txt GL_APPLE_texture_packed_float> extension is available.

gl_APPLE_texture_packed_float :: Bool
gl_APPLE_texture_packed_float = member "GL_APPLE_texture_packed_float" extensions
{-# NOINLINE gl_APPLE_texture_packed_float #-}

pattern GL_R11F_G11F_B10F_APPLE  :: (Eq a, Num a) => a

pattern GL_R11F_G11F_B10F_APPLE = 0x8C3A

pattern GL_RGB9_E5_APPLE  :: (Eq a, Num a) => a

pattern GL_RGB9_E5_APPLE = 0x8C3D

pattern GL_UNSIGNED_INT_10F_11F_11F_REV_APPLE  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_10F_11F_11F_REV_APPLE = 0x8C3B

pattern GL_UNSIGNED_INT_5_9_9_9_REV_APPLE  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_5_9_9_9_REV_APPLE = 0x8C3E