-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGI.ColorMatrix (
  -- * Extension Support
    gl_SGI_color_matrix

  -- * GL_SGI_color_matrix
  , pattern GL_COLOR_MATRIX_SGI
  , pattern GL_COLOR_MATRIX_STACK_DEPTH_SGI
  , pattern GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI
  , pattern GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI
  , pattern GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI
  , pattern GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI
  , pattern GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI
  , pattern GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI
  , pattern GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI
  , pattern GL_POST_COLOR_MATRIX_RED_BIAS_SGI
  , pattern GL_POST_COLOR_MATRIX_RED_SCALE_SGI
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGI/color_matrix.txt GL_SGI_color_matrix> extension is available.

gl_SGI_color_matrix :: Bool
gl_SGI_color_matrix = member "GL_SGI_color_matrix" extensions
{-# NOINLINE gl_SGI_color_matrix #-}

pattern GL_COLOR_MATRIX_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_MATRIX_SGI = 0x80B1

pattern GL_COLOR_MATRIX_STACK_DEPTH_SGI  :: (Eq a, Num a) => a

pattern GL_COLOR_MATRIX_STACK_DEPTH_SGI = 0x80B2

pattern GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI  :: (Eq a, Num a) => a

pattern GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI = 0x80B3

pattern GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI = 0x80BB

pattern GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI = 0x80B7

pattern GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI = 0x80BA

pattern GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI = 0x80B6

pattern GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI = 0x80B9

pattern GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI = 0x80B5

pattern GL_POST_COLOR_MATRIX_RED_BIAS_SGI  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_RED_BIAS_SGI = 0x80B8

pattern GL_POST_COLOR_MATRIX_RED_SCALE_SGI  :: (Eq a, Num a) => a

pattern GL_POST_COLOR_MATRIX_RED_SCALE_SGI = 0x80B4