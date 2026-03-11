{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, PatternSynonyms,
	ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.CairoImage.Parts (
	-- * Tool
	gen, new, ptr, stride, with,
	-- * Cairo Format
	CairoFormatT(..),
	pattern CairoFormatArgb32, pattern CairoFormatRgb24,
	pattern CairoFormatA8, pattern CairoFormatA1,
	pattern CairoFormatRgb16_565, pattern CairoFormatRgb30 ) where

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal (mallocBytes, free)
import Foreign.Storable (Storable, sizeOf, alignment, poke)
import Foreign.C.Types (CInt(..))
import Foreign.C.Enum(enum)
import Control.Monad.Primitive (
	PrimMonad(..), PrimBase, unsafeIOToPrim, unsafePrimToIO )
import Data.Foldable (for_)
import Data.Int (Int32)

#include <cairo.h>

---------------------------------------------------------------------------

-- * CAIRO FORMAT
-- * TOOL

---------------------------------------------------------------------------
-- CAIRO FORMAT
---------------------------------------------------------------------------

enum "CairoFormatT" ''#{type cairo_format_t} [''Show, ''Read, ''Eq] [
	("CairoFormatArgb32", #{const CAIRO_FORMAT_ARGB32}),
	("CairoFormatRgb24", #{const CAIRO_FORMAT_RGB24}),
	("CairoFormatA8", #{const CAIRO_FORMAT_A8}),
	("CairoFormatA1", #{const CAIRO_FORMAT_A1}),
	("CairoFormatRgb16_565", #{const CAIRO_FORMAT_RGB16_565}),
	("CairoFormatRgb30", #{const CAIRO_FORMAT_RGB30}) ]

---------------------------------------------------------------------------
-- TOOL
---------------------------------------------------------------------------

gen :: (PrimBase m, Storable a) =>
	CInt -> CInt -> CInt -> (CInt -> CInt -> m a) -> m (ForeignPtr a)
gen w h s f = unsafeIOToPrim $ mallocBytes (fromIntegral $ s * h) >>= \d -> do
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x ->
		unsafePrimToIO (f x y) >>= \p ->
			maybe (pure ()) (`poke` p) $ ptr w h s d x y
	newForeignPtr d $ free d

new :: PrimMonad m => CInt -> CInt -> m (ForeignPtr a)
new s h = unsafeIOToPrim
	$ mallocBytes (fromIntegral $ s * h) >>= \d -> newForeignPtr d $ free d

ptr :: forall a . Storable a =>
	CInt -> CInt -> CInt -> Ptr a -> CInt -> CInt -> Maybe (Ptr a)
ptr (fromIntegral -> w) (fromIntegral -> h) (fromIntegral -> s) p
	(fromIntegral -> x) (fromIntegral -> y)
	| 0 <= x && x < w && 0 <= y && y < h = Just $ p `plusPtr` (y * s + b)
	| otherwise = Nothing
	where
	b = x * ((sizeOf @a undefined - 1) `div` al + 1) * al
	al = alignment @a undefined

stride :: PrimMonad m => CairoFormatT -> CInt -> m CInt
stride = (unsafeIOToPrim .) . c_cairo_format_stride_for_width

foreign import ccall "cairo_format_stride_for_width"
	c_cairo_format_stride_for_width :: CairoFormatT -> CInt -> IO CInt

with :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
with = withForeignPtr
