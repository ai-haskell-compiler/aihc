-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.HalfFloatPixel (
  -- * Extension Support
    gl_ARB_half_float_pixel

  -- * GL_ARB_half_float_pixel
  , pattern GL_HALF_FLOAT_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/half_float_pixel.txt GL_ARB_half_float_pixel> extension is available.

gl_ARB_half_float_pixel :: Bool
gl_ARB_half_float_pixel = member "GL_ARB_half_float_pixel" extensions
{-# NOINLINE gl_ARB_half_float_pixel #-}

pattern GL_HALF_FLOAT_ARB  :: (Eq a, Num a) => a

pattern GL_HALF_FLOAT_ARB = 0x140B