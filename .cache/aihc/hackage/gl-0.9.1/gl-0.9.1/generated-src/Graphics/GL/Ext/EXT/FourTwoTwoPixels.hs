-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.FourTwoTwoPixels (
  -- * Extension Support
    gl_EXT_422_pixels

  -- * GL_EXT_422_pixels
  , pattern GL_422_AVERAGE_EXT
  , pattern GL_422_EXT
  , pattern GL_422_REV_AVERAGE_EXT
  , pattern GL_422_REV_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/422_pixels.txt GL_EXT_422_pixels> extension is available.

gl_EXT_422_pixels :: Bool
gl_EXT_422_pixels = member "GL_EXT_422_pixels" extensions
{-# NOINLINE gl_EXT_422_pixels #-}

pattern GL_422_AVERAGE_EXT  :: (Eq a, Num a) => a

pattern GL_422_AVERAGE_EXT = 0x80CE

pattern GL_422_EXT  :: (Eq a, Num a) => a

pattern GL_422_EXT = 0x80CC

pattern GL_422_REV_AVERAGE_EXT  :: (Eq a, Num a) => a

pattern GL_422_REV_AVERAGE_EXT = 0x80CF

pattern GL_422_REV_EXT  :: (Eq a, Num a) => a

pattern GL_422_REV_EXT = 0x80CD