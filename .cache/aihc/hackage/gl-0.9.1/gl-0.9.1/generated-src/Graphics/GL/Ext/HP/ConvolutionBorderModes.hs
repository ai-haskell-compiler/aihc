-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.HP.ConvolutionBorderModes (
  -- * Extension Support
    gl_HP_convolution_border_modes

  -- * GL_HP_convolution_border_modes
  , pattern GL_CONSTANT_BORDER_HP
  , pattern GL_CONVOLUTION_BORDER_COLOR_HP
  , pattern GL_IGNORE_BORDER_HP
  , pattern GL_REPLICATE_BORDER_HP
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/HP/convolution_border_modes.txt GL_HP_convolution_border_modes> extension is available.

gl_HP_convolution_border_modes :: Bool
gl_HP_convolution_border_modes = member "GL_HP_convolution_border_modes" extensions
{-# NOINLINE gl_HP_convolution_border_modes #-}

pattern GL_CONSTANT_BORDER_HP  :: (Eq a, Num a) => a

pattern GL_CONSTANT_BORDER_HP = 0x8151

pattern GL_CONVOLUTION_BORDER_COLOR_HP  :: (Eq a, Num a) => a

pattern GL_CONVOLUTION_BORDER_COLOR_HP = 0x8154

pattern GL_IGNORE_BORDER_HP  :: (Eq a, Num a) => a

pattern GL_IGNORE_BORDER_HP = 0x8150

pattern GL_REPLICATE_BORDER_HP  :: (Eq a, Num a) => a

pattern GL_REPLICATE_BORDER_HP = 0x8153