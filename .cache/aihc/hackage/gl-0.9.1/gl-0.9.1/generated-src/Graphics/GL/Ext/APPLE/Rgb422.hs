-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.Rgb422 (
  -- * Extension Support
    gl_APPLE_rgb_422

  -- * GL_APPLE_rgb_422
  , pattern GL_RGB_422_APPLE
  , pattern GL_RGB_RAW_422_APPLE
  , pattern GL_UNSIGNED_SHORT_8_8_APPLE
  , pattern GL_UNSIGNED_SHORT_8_8_REV_APPLE
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/APPLE/APPLE_rgb_422.txt GL_APPLE_rgb_422> extension is available.

gl_APPLE_rgb_422 :: Bool
gl_APPLE_rgb_422 = member "GL_APPLE_rgb_422" extensions
{-# NOINLINE gl_APPLE_rgb_422 #-}

pattern GL_RGB_422_APPLE  :: (Eq a, Num a) => a

pattern GL_RGB_422_APPLE = 0x8A1F

pattern GL_RGB_RAW_422_APPLE  :: (Eq a, Num a) => a

pattern GL_RGB_RAW_422_APPLE = 0x8A51