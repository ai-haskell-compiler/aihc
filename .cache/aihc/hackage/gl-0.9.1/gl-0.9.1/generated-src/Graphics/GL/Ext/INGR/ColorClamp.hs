-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.INGR.ColorClamp (
  -- * Extension Support
    gl_INGR_color_clamp

  -- * GL_INGR_color_clamp
  , pattern GL_ALPHA_MAX_CLAMP_INGR
  , pattern GL_ALPHA_MIN_CLAMP_INGR
  , pattern GL_BLUE_MAX_CLAMP_INGR
  , pattern GL_BLUE_MIN_CLAMP_INGR
  , pattern GL_GREEN_MAX_CLAMP_INGR
  , pattern GL_GREEN_MIN_CLAMP_INGR
  , pattern GL_RED_MAX_CLAMP_INGR
  , pattern GL_RED_MIN_CLAMP_INGR
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/INGR/color_clamp.txt GL_INGR_color_clamp> extension is available.

gl_INGR_color_clamp :: Bool
gl_INGR_color_clamp = member "GL_INGR_color_clamp" extensions
{-# NOINLINE gl_INGR_color_clamp #-}

pattern GL_ALPHA_MAX_CLAMP_INGR  :: (Eq a, Num a) => a

pattern GL_ALPHA_MAX_CLAMP_INGR = 0x8567

pattern GL_ALPHA_MIN_CLAMP_INGR  :: (Eq a, Num a) => a

pattern GL_ALPHA_MIN_CLAMP_INGR = 0x8563

pattern GL_BLUE_MAX_CLAMP_INGR  :: (Eq a, Num a) => a

pattern GL_BLUE_MAX_CLAMP_INGR = 0x8566

pattern GL_BLUE_MIN_CLAMP_INGR  :: (Eq a, Num a) => a

pattern GL_BLUE_MIN_CLAMP_INGR = 0x8562

pattern GL_GREEN_MAX_CLAMP_INGR  :: (Eq a, Num a) => a

pattern GL_GREEN_MAX_CLAMP_INGR = 0x8565

pattern GL_GREEN_MIN_CLAMP_INGR  :: (Eq a, Num a) => a

pattern GL_GREEN_MIN_CLAMP_INGR = 0x8561

pattern GL_RED_MAX_CLAMP_INGR  :: (Eq a, Num a) => a

pattern GL_RED_MAX_CLAMP_INGR = 0x8564

pattern GL_RED_MIN_CLAMP_INGR  :: (Eq a, Num a) => a

pattern GL_RED_MIN_CLAMP_INGR = 0x8560