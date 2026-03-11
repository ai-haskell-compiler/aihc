{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Types where

import Foreign.Storable
import Data.Int

#include <cairo.h>

data CairoTextExtentsT = CairoTextExtentsT {
	cairoTextExtentsTXBearing :: #{type double},
	cairoTextExtentsTYBearing :: #{type double},
	cairoTextExtentsTWidth :: #{type double},
	cairoTextExtentsTHeight :: #{type double},
	cairoTextExtentsTXAdvance :: #{type double},
	cairoTextExtentsTYAdvance :: #{type double} } deriving Show

instance Storable CairoTextExtentsT where
	sizeOf _ = #{size cairo_text_extents_t}
	alignment _ = #{alignment cairo_text_extents_t}
	peek p = CairoTextExtentsT
		<$> #{peek cairo_text_extents_t, x_bearing} p
		<*> #{peek cairo_text_extents_t, y_bearing} p
		<*> #{peek cairo_text_extents_t, width} p
		<*> #{peek cairo_text_extents_t, height} p
		<*> #{peek cairo_text_extents_t, x_advance} p
		<*> #{peek cairo_text_extents_t, y_advance} p
	poke p (CairoTextExtentsT xb yb w h xa ya) = do
		#{poke cairo_text_extents_t, x_bearing} p xb
		#{poke cairo_text_extents_t, y_bearing} p yb
		#{poke cairo_text_extents_t, width} p w
		#{poke cairo_text_extents_t, height} p h
		#{poke cairo_text_extents_t, x_advance} p xa
		#{poke cairo_text_extents_t, y_advance} p ya
