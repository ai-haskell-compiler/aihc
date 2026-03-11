-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SUN.ConvolutionBorderModes (
  -- * Extension Support
    gl_SUN_convolution_border_modes

  -- * GL_SUN_convolution_border_modes
  , pattern GL_WRAP_BORDER_SUN
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SUN/convolution_border_modes.txt GL_SUN_convolution_border_modes> extension is available.

gl_SUN_convolution_border_modes :: Bool
gl_SUN_convolution_border_modes = member "GL_SUN_convolution_border_modes" extensions
{-# NOINLINE gl_SUN_convolution_border_modes #-}

pattern GL_WRAP_BORDER_SUN  :: (Eq a, Num a) => a

pattern GL_WRAP_BORDER_SUN = 0x81D4