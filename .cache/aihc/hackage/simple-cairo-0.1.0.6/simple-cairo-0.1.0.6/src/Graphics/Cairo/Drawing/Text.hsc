{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Text (
	cairoSelectFontFace, cairoSetFontSize, cairoShowText, cairoTextExtents
	) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Control.Monad.Primitive
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Graphics.Cairo.Types
import Graphics.Cairo.Values

import Data.CairoContext

#include <cairo.h>

foreign import ccall "cairo_select_font_face" c_cairo_select_font_face ::
	Ptr (CairoT r s) -> CString ->
	#{type cairo_font_slant_t} -> #{type cairo_font_weight_t} -> IO ()

cairoSelectFontFace :: PrimMonad m =>
	CairoT r (PrimState m) -> T.Text -> CairoFontSlantT -> CairoFontWeightT -> m ()
cairoSelectFontFace cr ff (CairoFontSlantT sl) (CairoFontWeightT wt) =
	withCairoT cr \pcr -> encode ff \cs ->
		c_cairo_select_font_face pcr cs sl wt

foreign import ccall "cairo_set_font_size" c_cairo_set_font_size ::
	Ptr (CairoT r s) -> #{type double} -> IO ()

cairoSetFontSize :: PrimMonad m => CairoT r (PrimState m) -> #{type double} -> m ()
cairoSetFontSize cr fs = withCairoT cr \pcr -> c_cairo_set_font_size pcr fs

foreign import ccall "cairo_text_extents" c_cairo_text_extents ::
	Ptr (CairoT r s) -> CString -> Ptr CairoTextExtentsT -> IO ()

cairoTextExtents :: PrimMonad m => CairoT r (PrimState m) -> T.Text -> m CairoTextExtentsT
cairoTextExtents cr t = withCairoT cr \pcr ->
	encode t \cs -> alloca \p -> c_cairo_text_extents pcr cs p *> peek p

encode :: T.Text -> (CString -> IO a) -> IO a
encode t = BS.useAsCString $ T.encodeUtf8 t

foreign import ccall "cairo_show_text" c_cairo_show_text ::
	Ptr (CairoT r s) -> CString -> IO ()

cairoShowText :: PrimMonad m => CairoT r (PrimState m) -> T.Text -> m ()
cairoShowText cr t = withCairoT cr \pcr -> encode t \cs -> c_cairo_show_text pcr cs
