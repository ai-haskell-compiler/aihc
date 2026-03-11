-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.PointSprite (
  -- * Extension Support
    gl_ARB_point_sprite

  -- * GL_ARB_point_sprite
  , pattern GL_COORD_REPLACE_ARB
  , pattern GL_POINT_SPRITE_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/point_sprite.txt GL_ARB_point_sprite> extension is available.

gl_ARB_point_sprite :: Bool
gl_ARB_point_sprite = member "GL_ARB_point_sprite" extensions
{-# NOINLINE gl_ARB_point_sprite #-}

pattern GL_COORD_REPLACE_ARB  :: (Eq a, Num a) => a

pattern GL_COORD_REPLACE_ARB = 0x8862

pattern GL_POINT_SPRITE_ARB  :: (Eq a, Num a) => a

pattern GL_POINT_SPRITE_ARB = 0x8861