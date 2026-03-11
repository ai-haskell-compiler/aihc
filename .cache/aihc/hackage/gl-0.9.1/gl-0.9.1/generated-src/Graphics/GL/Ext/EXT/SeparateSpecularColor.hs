-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.SeparateSpecularColor (
  -- * Extension Support
    gl_EXT_separate_specular_color

  -- * GL_EXT_separate_specular_color
  , pattern GL_LIGHT_MODEL_COLOR_CONTROL_EXT
  , pattern GL_SEPARATE_SPECULAR_COLOR_EXT
  , pattern GL_SINGLE_COLOR_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/separate_specular_color.txt GL_EXT_separate_specular_color> extension is available.

gl_EXT_separate_specular_color :: Bool
gl_EXT_separate_specular_color = member "GL_EXT_separate_specular_color" extensions
{-# NOINLINE gl_EXT_separate_specular_color #-}

pattern GL_LIGHT_MODEL_COLOR_CONTROL_EXT  :: (Eq a, Num a) => a

pattern GL_LIGHT_MODEL_COLOR_CONTROL_EXT = 0x81F8

pattern GL_SEPARATE_SPECULAR_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_SEPARATE_SPECULAR_COLOR_EXT = 0x81FA

pattern GL_SINGLE_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_SINGLE_COLOR_EXT = 0x81F9