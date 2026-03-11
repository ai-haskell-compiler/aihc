{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoImage (
	-- * Class Image and ImageMut
	Image(..), ImageMut(..),
	-- * Type CairoImage and CairoImageMut
	CairoImage, CairoImageMut, cairoImageFreeze, cairoImageThaw,
	-- * Image Format
	-- ** ARGB 32
	PixelArgb32,
	pattern PixelArgb32Premultiplied, pixelArgb32Premultiplied,
	pattern PixelArgb32Straight,
	pattern CairoImageArgb32, Argb32,
	pattern CairoImageMutArgb32, Argb32Mut,
	-- ** RGB 24
	PixelRgb24, pattern PixelRgb24,
	pattern CairoImageRgb24, Rgb24,
	pattern CairoImageMutRgb24, Rgb24Mut,
	-- ** A 8
	PixelA8, pattern PixelA8,
	pattern CairoImageA8, A8,
	pattern CairoImageMutA8, A8Mut,
	-- ** A 1
	PixelA1, pattern PixelA1, Bit(..), bit,
	pattern CairoImageA1, A1,
	pattern CairoImageMutA1, A1Mut,
	-- ** RGB 16 565
	PixelRgb16_565, pattern PixelRgb16_565,
	pattern CairoImageRgb16_565, Rgb16_565,
	pattern CairoImageMutRgb16_565, Rgb16_565Mut,
	-- ** RGB 30
	PixelRgb30, pattern PixelRgb30,
	pattern CairoImageRgb30, Rgb30,
	pattern CairoImageMutRgb30, Rgb30Mut ) where

import Data.CairoImage.Internal (
	Image(..), ImageMut(..),
	CairoImage, CairoImageMut, cairoImageFreeze, cairoImageThaw,
	PixelArgb32,
	pattern PixelArgb32Premultiplied, pixelArgb32Premultiplied,
	pattern PixelArgb32Straight,
	pattern CairoImageArgb32, Argb32,
	pattern CairoImageMutArgb32, Argb32Mut,
	PixelRgb24, pattern PixelRgb24,
	pattern CairoImageRgb24, Rgb24, pattern CairoImageMutRgb24, Rgb24Mut,
	PixelA8, pattern PixelA8,
	pattern CairoImageA8, A8, pattern CairoImageMutA8, A8Mut,
	PixelA1, pattern PixelA1, Bit(..), bit,
	pattern CairoImageA1, A1, pattern CairoImageMutA1, A1Mut,
	PixelRgb16_565, pattern PixelRgb16_565,
	pattern CairoImageRgb16_565, Rgb16_565,
	pattern CairoImageMutRgb16_565, Rgb16_565Mut,
	PixelRgb30, pattern PixelRgb30,
	pattern CairoImageRgb30, Rgb30, pattern CairoImageMutRgb30, Rgb30Mut )
