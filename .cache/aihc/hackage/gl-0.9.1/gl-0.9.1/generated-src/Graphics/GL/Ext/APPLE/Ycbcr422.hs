-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.Ycbcr422 (
  -- * Extension Support
    gl_APPLE_ycbcr_422

  -- * GL_APPLE_ycbcr_422
  , pattern GL_UNSIGNED_SHORT_8_8_APPLE
  , pattern GL_UNSIGNED_SHORT_8_8_REV_APPLE
  , pattern GL_YCBCR_422_APPLE
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/ycbcr_422.txt GL_APPLE_ycbcr_422> extension is available.

gl_APPLE_ycbcr_422 :: Bool
gl_APPLE_ycbcr_422 = member "GL_APPLE_ycbcr_422" extensions
{-# NOINLINE gl_APPLE_ycbcr_422 #-}

pattern GL_YCBCR_422_APPLE  :: (Eq a, Num a) => a

pattern GL_YCBCR_422_APPLE = 0x85B9