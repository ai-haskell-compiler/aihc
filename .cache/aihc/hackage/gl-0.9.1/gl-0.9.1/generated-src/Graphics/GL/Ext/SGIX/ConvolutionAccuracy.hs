-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.ConvolutionAccuracy (
  -- * Extension Support
    gl_SGIX_convolution_accuracy

  -- * GL_SGIX_convolution_accuracy
  , pattern GL_CONVOLUTION_HINT_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/convolution_accuracy.txt GL_SGIX_convolution_accuracy> extension is available.

gl_SGIX_convolution_accuracy :: Bool
gl_SGIX_convolution_accuracy = member "GL_SGIX_convolution_accuracy" extensions
{-# NOINLINE gl_SGIX_convolution_accuracy #-}

pattern GL_CONVOLUTION_HINT_SGIX  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_HINT_SGIX = 0x8316