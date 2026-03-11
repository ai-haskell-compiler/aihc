{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Rendering.Cairo (
	-- * CREATE
	pangoCairoCreateContext,
	pangoCairoUpdateContext,
	pangoCairoCreateLayout,
	pangoCairoUpdateLayout,

	-- * SHOW
	pangoCairoShowLayout,
	pangoCairoShowErrorUnderline,
	pangoCairoShowGlyphItem,
	pangoCairoShowLayoutLine,

	-- * PATH
	pangoCairoLayoutLinePath,
	pangoCairoLayoutPath,
	pangoCairoErrorUnderlinePath,

	-- * RESOLUTION
	pangoCairoContextGetResolution,
	pangoCairoContextSetResolution

	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.C
import Foreign.C.String.Misc
import Control.Monad.Primitive
import Data.Maybe
import Data.CairoContext

import Graphics.Pango.Basic.GlyphStorage.Internal
import Graphics.Pango.Basic.LayoutObjects.PangoLayout.Internal
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine.Internal
import Graphics.Pango.LowLevel.Contexts.Internal

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

pangoCairoCreateContext :: PrimMonad m => CairoT r (PrimState m) -> m PangoContext
pangoCairoCreateContext (CairoT fcr) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr ->
		mkPangoContext =<< c_pango_cairo_create_context pcr

foreign import ccall "pango_cairo_create_context"
	c_pango_cairo_create_context :: Ptr (CairoT r s) -> IO (Ptr PangoContext)

pangoCairoUpdateContext ::
	PrimMonad m => CairoT r (PrimState m) -> PangoContext -> m ()
pangoCairoUpdateContext (CairoT fcr) (PangoContext fc) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> withForeignPtr fc \pc ->
		c_pango_cairo_update_context pcr pc

foreign import ccall "pango_cairo_update_context"
	c_pango_cairo_update_context ::
	Ptr (CairoT r s) -> Ptr PangoContext -> IO ()

pangoCairoCreateLayout ::
	PrimMonad m => CairoT r (PrimState m) -> m (PangoLayoutPrim (PrimState m))
pangoCairoCreateLayout (CairoT fcr) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr ->
		mkPangoLayoutPrim =<< c_pango_cairo_create_layout pcr

foreign import ccall "pango_cairo_create_layout" c_pango_cairo_create_layout ::
	Ptr (CairoT r s) -> IO (Ptr PangoLayout)

pangoCairoUpdateLayout :: PrimMonad m =>
	CairoT r (PrimState m) -> PangoLayoutPrim (PrimState m) -> m ()
pangoCairoUpdateLayout (CairoT fcr) (PangoLayoutPrim fl) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> withForeignPtr fl \pl ->
		c_pango_cairo_update_layout pcr pl

foreign import ccall "pango_cairo_update_layout" c_pango_cairo_update_layout ::
	Ptr (CairoT r s) -> Ptr PangoLayout -> IO ()

pangoCairoShowGlyphItem :: CairoTIO r -> T.Text -> PangoGlyphItem -> IO ()
pangoCairoShowGlyphItem (CairoT fcr) t (PangoGlyphItem fgi) =
	withForeignPtr fcr \cr -> T.withCStringLen t \(ct, n) -> do
		cs' <- copyCString ct n
		addForeignPtrFinalizer fcr $ free cs'
		withForeignPtr fgi $ c_pango_cairo_show_glyph_item cr cs'

foreign import ccall "pango_cairo_show_glyph_item"
	c_pango_cairo_show_glyph_item ::
	Ptr (CairoT r s) -> CString -> Ptr PangoGlyphItem -> IO ()

pangoCairoShowLayoutLine :: CairoTIO r -> PangoLayoutLine -> IO ()
pangoCairoShowLayoutLine (CairoT fcr) (PangoLayoutLine fll) =
	withForeignPtr fcr \cr ->
		withForeignPtr fll $ c_pango_cairo_show_layout_line cr

foreign import ccall "pango_cairo_show_layout_line"
	c_pango_cairo_show_layout_line ::
	Ptr (CairoT r s) -> Ptr PangoLayoutLine -> IO ()

pangoCairoShowLayout :: CairoTIO r -> PangoLayout -> IO ()
pangoCairoShowLayout (CairoT fcr) (PangoLayout_ fl) = withForeignPtr fcr \cr ->
	withForeignPtr fl $ c_pango_cairo_show_layout cr

foreign import ccall "pango_cairo_show_layout" c_pango_cairo_show_layout ::
	Ptr (CairoT r s) -> Ptr PangoLayout -> IO ()

pangoCairoShowErrorUnderline ::
	CairoTIO r -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
pangoCairoShowErrorUnderline (CairoT fcr) x y w h =
	withForeignPtr fcr \cr -> c_pango_cairo_show_error_underline cr x y w h

foreign import ccall "pango_cairo_show_error_underline"
	c_pango_cairo_show_error_underline ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

pangoCairoLayoutLinePath :: CairoTIO r -> PangoLayoutLine -> IO ()
pangoCairoLayoutLinePath (CairoT fcr) (PangoLayoutLine fll) =
	withForeignPtr fcr \cr ->
		withForeignPtr fll $ c_pango_cairo_layout_line_path cr

foreign import ccall "pango_cairo_layout_line_path"
	c_pango_cairo_layout_line_path ::
	Ptr (CairoT r s) -> Ptr PangoLayoutLine -> IO ()

pangoCairoLayoutPath :: CairoTIO r -> PangoLayout -> IO ()
pangoCairoLayoutPath (CairoT fcr) (PangoLayout_ fl) = withForeignPtr fcr \cr ->
	withForeignPtr fl $ c_pango_cairo_layout_path cr

foreign import ccall "pango_cairo_layout_path" c_pango_cairo_layout_path ::
	Ptr (CairoT r s) -> Ptr PangoLayout -> IO ()

pangoCairoErrorUnderlinePath ::
	CairoTIO r -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
pangoCairoErrorUnderlinePath (CairoT fcr) x y w h = withForeignPtr fcr \cr ->
	c_pango_cairo_error_underline_path cr x y w h

foreign import ccall "pango_cairo_error_underline_path"
	c_pango_cairo_error_underline_path ::
	Ptr (CairoT r s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

pangoCairoContextGetResolution :: PangoContext -> IO (Maybe CDouble)
pangoCairoContextGetResolution (PangoContext fctx) = do
	r <- withForeignPtr fctx c_pango_cairo_context_get_resolution
	pure if r < 0 then Nothing else Just r

foreign import ccall "pango_cairo_context_get_resolution"
	c_pango_cairo_context_get_resolution ::
	Ptr PangoContext -> IO CDouble

pangoCairoContextSetResolution :: PangoContext -> Maybe CDouble -> IO ()
pangoCairoContextSetResolution (PangoContext fctx) mr =
	withForeignPtr fctx \pctx -> c_pango_cairo_context_set_resolution pctx
		$ fromMaybe (- 1) mr

foreign import ccall "pango_cairo_context_set_resolution"
	c_pango_cairo_context_set_resolution ::
	Ptr PangoContext -> CDouble -> IO ()
