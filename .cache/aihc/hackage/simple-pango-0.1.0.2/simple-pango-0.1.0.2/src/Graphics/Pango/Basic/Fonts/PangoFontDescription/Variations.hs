{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations (
	-- * SET AND GET AXIS
	pangoFontDescriptionSetAxis, pangoFontDescriptionGetAxis,
	-- * SET AND GET VARIATIONS
	Variations,
	pangoFontDescriptionSetVariationsMap,
	pangoFontDescriptionGetVariationsMap,
	-- * DEFAULT AXES
	Weight(..), Width(..), Slant(..), Italic(..), OpticalSize(..)
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Control.Monad.Primitive

import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

import qualified Data.ByteString as BS

import System.IO.Unsafe

import Data.Font.VariationAxis

pangoFontDescriptionSetAxis :: forall a m .
	(FontDescriptionAxis a, PrimMonad m) =>
	PangoFontDescriptionPrim (PrimState m) -> a -> m ()
pangoFontDescriptionSetAxis fd a = do
	as <- pangoFontDescriptionGetVariationsMap fd
	pangoFontDescriptionSetVariationsMap fd $ variationsSetAxis a as

pangoFontDescriptionGetAxis ::
	forall a . FontDescriptionAxis a => PangoFontDescription -> Maybe a
pangoFontDescriptionGetAxis fd = unsafePerformIO do
	pangoFontDescriptionThaw fd >>= \case
		fd' -> do
			as <- pangoFontDescriptionGetVariationsMap fd'
			pure $ variationsGetAxis as

pangoFontDescriptionSetVariationsMap :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> Variations -> m ()
pangoFontDescriptionSetVariationsMap (PangoFontDescriptionPrim ffd) v = unsafeIOToPrim
	$ withForeignPtr ffd \pfd -> BS.useAsCString (showVariations v) \cv ->
		c_pango_font_description_set_variations pfd cv

foreign import ccall "pango_font_description_set_variations"
	c_pango_font_description_set_variations ::
	Ptr PangoFontDescription -> CString -> IO ()

pangoFontDescriptionGetVariationsMap :: PrimMonad m =>
	PangoFontDescriptionPrim (PrimState m) -> m Variations
pangoFontDescriptionGetVariationsMap (PangoFontDescriptionPrim ffd) = unsafeIOToPrim
	$ withForeignPtr ffd \pfd -> readVariations <$>
		(myPackCString =<< c_pango_font_description_get_variations pfd)

myPackCString :: CString -> IO BS.ByteString
myPackCString cs | cs == nullPtr = pure "" | otherwise = BS.packCString cs

foreign import ccall "pango_font_description_get_variations"
	c_pango_font_description_get_variations ::
	Ptr PangoFontDescription -> IO CString
