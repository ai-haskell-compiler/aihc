{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.TagsAndLinks where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Control.Monad.Primitive
import Data.CairoContext

import Graphics.Cairo.Exception

#include <cairo.h>

cairoTagLinkInternal :: PrimMonad m =>
	CairoT r (PrimState m) -> Name -> m a -> m a
cairoTagLinkInternal cr d = cairoTagLinkInternalDestPage cr $ Left d

cairoTagLinkInternalDestPage :: PrimMonad m => CairoT r (PrimState m) ->
	Either Name (Int, Maybe (Double, Double)) -> m a -> m a
cairoTagLinkInternalDestPage cr@(CairoT fcr) d m = do
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> do
		tl <- newCString #{const_str CAIRO_TAG_LINK}
		internalAttributes d $ c_cairo_tag_begin pcr tl
	m <* unsafeIOToPrim do
		withForeignPtr fcr \pcr -> c_cairo_tag_end pcr
			=<< newCString #{const_str CAIRO_TAG_LINK}
		raiseIfError cr

internalAttributes :: Either Name (Int, Maybe (Double, Double)) -> (CString -> IO a) -> IO a
internalAttributes = \case
	Left n -> withCString $ "dest='" ++ escape n ++ "'"
	Right (p, Nothing) -> withCString $ "page=" ++ show p
	Right (p, Just (x, y)) -> withCString $ "page=" ++ show p ++ " pos=[" ++ show x ++ " " ++ show y ++ "]"

cairoTagLinkUri :: PrimMonad m => CairoT r (PrimState m) -> Uri -> m a -> m a
cairoTagLinkUri cr@(CairoT fcr) u m = do
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> do
		tl <- newCString #{const_str CAIRO_TAG_LINK}
		c_cairo_tag_begin pcr tl =<< newCString ("uri='" ++ escape u ++ "'")
	m <* unsafeIOToPrim do
		withForeignPtr fcr \pcr -> c_cairo_tag_end pcr
			=<< newCString #{const_str CAIRO_TAG_LINK}
		raiseIfError cr

type Uri = String

cairoTagDestination :: PrimMonad m => CairoT r (PrimState m) -> Name -> m a -> m a
cairoTagDestination cr@(CairoT fcr) n m = do
	unsafeIOToPrim $ withForeignPtr fcr \pcr -> do
		td <- newCString #{const_str CAIRO_TAG_DEST}
		c_cairo_tag_begin pcr td =<< newCString ("name='" ++ escape n ++ "'")
	m <* unsafeIOToPrim do
		withForeignPtr fcr \pcr -> c_cairo_tag_end pcr
			=<< newCString #{const_str CAIRO_TAG_DEST}
		raiseIfError cr

type Name = String

foreign import ccall "cairo_tag_begin" c_cairo_tag_begin ::
	Ptr (CairoT r s) -> CString -> CString -> IO ()

foreign import ccall "cairo_tag_end" c_cairo_tag_end ::
	Ptr (CairoT r s) -> CString -> IO ()

escape :: String -> String
escape = \case
	"" -> ""
	'\'' : cs -> "\\'" ++ escape cs
	'\\' : cs -> "\\\\" ++ escape cs
	c : cs -> c : escape cs
