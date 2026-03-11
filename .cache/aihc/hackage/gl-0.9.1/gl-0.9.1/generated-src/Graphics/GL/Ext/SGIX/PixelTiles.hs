-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.PixelTiles (
  -- * Extension Support
    gl_SGIX_pixel_tiles

  -- * GL_SGIX_pixel_tiles
  , pattern GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX
  , pattern GL_PIXEL_TILE_CACHE_INCREMENT_SGIX
  , pattern GL_PIXEL_TILE_CACHE_SIZE_SGIX
  , pattern GL_PIXEL_TILE_GRID_DEPTH_SGIX
  , pattern GL_PIXEL_TILE_GRID_HEIGHT_SGIX
  , pattern GL_PIXEL_TILE_GRID_WIDTH_SGIX
  , pattern GL_PIXEL_TILE_HEIGHT_SGIX
  , pattern GL_PIXEL_TILE_WIDTH_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_SGIX_pixel_tiles extension is available.

gl_SGIX_pixel_tiles :: Bool
gl_SGIX_pixel_tiles = member "GL_SGIX_pixel_tiles" extensions
{-# NOINLINE gl_SGIX_pixel_tiles #-}

pattern GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX = 0x813E

pattern GL_PIXEL_TILE_CACHE_INCREMENT_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_TILE_CACHE_INCREMENT_SGIX = 0x813F

pattern GL_PIXEL_TILE_CACHE_SIZE_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_TILE_CACHE_SIZE_SGIX = 0x8145

pattern GL_PIXEL_TILE_GRID_DEPTH_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_TILE_GRID_DEPTH_SGIX = 0x8144

pattern GL_PIXEL_TILE_GRID_HEIGHT_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_TILE_GRID_HEIGHT_SGIX = 0x8143

pattern GL_PIXEL_TILE_GRID_WIDTH_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_TILE_GRID_WIDTH_SGIX = 0x8142

pattern GL_PIXEL_TILE_HEIGHT_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_TILE_HEIGHT_SGIX = 0x8141

pattern GL_PIXEL_TILE_WIDTH_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_TILE_WIDTH_SGIX = 0x8140