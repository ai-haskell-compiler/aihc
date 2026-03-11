-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.PointSprite (
  -- * Extension Support
    gl_OES_point_sprite

  -- * GL_OES_point_sprite
  , pattern GL_COORD_REPLACE_OES
  , pattern GL_POINT_SPRITE_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_point_sprite.txt GL_OES_point_sprite> extension is available.

gl_OES_point_sprite :: Bool
gl_OES_point_sprite = member "GL_OES_point_sprite" extensions
{-# NOINLINE gl_OES_point_sprite #-}

pattern GL_COORD_REPLACE_OES  :: (Eq a, Num a) => a

pattern GL_COORD_REPLACE_OES = 0x8862

pattern GL_POINT_SPRITE_OES  :: (Eq a, Num a) => a

pattern GL_POINT_SPRITE_OES = 0x8861