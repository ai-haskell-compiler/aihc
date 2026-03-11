-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.REND.ScreenCoordinates (
  -- * Extension Support
    gl_REND_screen_coordinates

  -- * GL_REND_screen_coordinates
  , pattern GL_INVERTED_SCREEN_W_REND
  , pattern GL_SCREEN_COORDINATES_REND
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/REND/screen_coordinates.txt GL_REND_screen_coordinates> extension is available.

gl_REND_screen_coordinates :: Bool
gl_REND_screen_coordinates = member "GL_REND_screen_coordinates" extensions
{-# NOINLINE gl_REND_screen_coordinates #-}

pattern GL_INVERTED_SCREEN_W_REND  :: (Eq a, Num a) => a

pattern GL_INVERTED_SCREEN_W_REND = 0x8491

pattern GL_SCREEN_COORDINATES_REND  :: (Eq a, Num a) => a

pattern GL_SCREEN_COORDINATES_REND = 0x8490