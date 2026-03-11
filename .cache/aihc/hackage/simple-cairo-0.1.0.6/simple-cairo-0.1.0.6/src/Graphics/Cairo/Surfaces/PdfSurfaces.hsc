{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.PdfSurfaces where

import GHC.Foreign hiding (peekCString)
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String hiding (withCString)
import Control.Monad.Primitive
import Control.Concurrent.STM
import Data.Bits
import Data.Word
import System.IO

import Graphics.Cairo.Exception
import Graphics.Cairo.Surfaces.CairoWriteFuncT
import Graphics.Cairo.Surfaces.CairoSurfaceT.Internal
import Graphics.Cairo.Surfaces.CairoSurfaceTypeT
import Graphics.Cairo.Drawing.TagsAndLinks

import Graphics.Cairo.Surfaces.PdfSurfaces.Template

import qualified Data.ByteString as BS

#include <cairo.h>
#include <cairo-pdf.h>

data CairoSurfacePdfT s ps = CairoSurfacePdfT (ForeignPtr (CairoSurfaceT s ps)) (STM (TChan ()))

pattern CairoSurfaceTPdf :: CairoSurfacePdfT s ps -> CairoSurfaceT s ps
pattern CairoSurfaceTPdf sr <- (cairoSurfaceTPdf -> Just sr) where
	CairoSurfaceTPdf = toCairoSurfaceT

cairoSurfaceTPdf :: CairoSurfaceT s ps -> Maybe (CairoSurfacePdfT s ps)
cairoSurfaceTPdf sr@(CairoSurfaceT fsr) = case cairoSurfaceGetType sr of
	CairoSurfaceTypePdf -> Just $ CairoSurfacePdfT fsr newTChan
	_ -> Nothing

instance IsCairoSurfaceT CairoSurfacePdfT where
	toCairoSurfaceT (CairoSurfacePdfT fsr _) = CairoSurfaceT fsr
	cairoSurfaceTFinishChecker (CairoSurfacePdfT _ ck) = ck

cairoPdfSurfaceWith :: FilePath -> CDouble -> CDouble ->
	(forall s . CairoSurfacePdfT s RealWorld -> IO a) -> IO a
cairoPdfSurfaceWith fp w h f = do
	sr@(CairoSurfacePdfT fsr mck) <- cairoPdfSurfaceCreateNoGC fp w h
	ck <- atomically mck
	f sr <* withForeignPtr fsr
		(\psr -> do
			c_cairo_surface_finish psr
			c_cairo_surface_destroy psr
			putStrLn "PDF Surface destroyed"
			atomically (writeTChan ck ())
			putStrLn "writeTChan ck () finished")

cairoPdfSurfaceCreate :: FilePath -> CDouble -> CDouble -> IO (CairoSurfacePdfT s RealWorld)
cairoPdfSurfaceCreate fp w h = do
	ck <- atomically newTChan
	(`CairoSurfacePdfT` pure ck) <$> withCString utf8 fp \cstr -> do
		p <- c_cairo_pdf_surface_create cstr w h
		newForeignPtr p (c_cairo_surface_destroy p) <* raiseIfErrorPtrSurface p

cairoPdfSurfaceCreateNoGC :: FilePath -> CDouble -> CDouble -> IO (CairoSurfacePdfT s RealWorld)
cairoPdfSurfaceCreateNoGC fp w h = do
	ck <- atomically newTChan
	(`CairoSurfacePdfT` pure ck) <$> withCString utf8 fp \cstr -> do
		p <- c_cairo_pdf_surface_create cstr w h
		newForeignPtr p (pure ()) <* raiseIfErrorPtrSurface p

foreign import ccall "cairo_pdf_surface_create" c_cairo_pdf_surface_create ::
	CString -> CDouble -> CDouble -> IO (Ptr (CairoSurfaceT s ps))

unsafeCairoSurfaceFinish :: CairoSurfaceT s RealWorld -> IO ()
unsafeCairoSurfaceFinish (CairoSurfaceT fsr) =
	withForeignPtr fsr c_cairo_surface_finish

cairoPdfSurfaceWithForStream :: PrimBase m =>
	(Ptr a -> BS.ByteString -> m WriteResult) -> Ptr a -> CDouble -> CDouble ->
	(forall s . CairoSurfacePdfT s (PrimState m) -> m a) -> m a
cairoPdfSurfaceWithForStream wf cl w h f = do
	sr@(CairoSurfacePdfT fsr mck) <- cairoPdfSurfaceCreateForStreamNoGC wf cl w h
	ck <- unsafeIOToPrim $ atomically mck
	f sr <* unsafeIOToPrim (withForeignPtr fsr $
		\psr -> c_cairo_surface_finish psr >> atomically (writeTChan ck ()) >> c_cairo_surface_destroy psr)

cairoPdfSurfaceCreateForStream :: PrimBase m =>
	(Ptr a -> BS.ByteString -> m WriteResult) -> Ptr a -> CDouble -> CDouble ->
	m (CairoSurfacePdfT s (PrimState m))
cairoPdfSurfaceCreateForStream wf cl w h = do
	ck <- unsafeIOToPrim $ atomically newTChan
	(`CairoSurfacePdfT` pure ck) <$> unsafeIOToPrim do
		p <- (wrapCairoWriteFuncTByteString wf >>= \pwf ->
			c_cairo_pdf_surface_create_for_stream pwf cl w h)
		newForeignPtr p (c_cairo_surface_destroy p) <* raiseIfErrorPtrSurface p

cairoPdfSurfaceCreateForStreamNoGC :: PrimBase m =>
	(Ptr a -> BS.ByteString -> m WriteResult) -> Ptr a -> CDouble -> CDouble ->
	m (CairoSurfacePdfT s (PrimState m))
cairoPdfSurfaceCreateForStreamNoGC wf cl w h = do
	ck <- unsafeIOToPrim $ atomically newTChan
	(`CairoSurfacePdfT` pure ck) <$> unsafeIOToPrim do
		p <- (wrapCairoWriteFuncTByteString wf >>= \pwf ->
			c_cairo_pdf_surface_create_for_stream pwf cl w h)
		newForeignPtr p (pure ()) <* raiseIfErrorPtrSurface p

foreign import ccall "cairo_pdf_surface_create_for_stream"
	c_cairo_pdf_surface_create_for_stream ::
	FunPtr (Ptr a -> CString -> CInt -> IO #{type cairo_status_t}) ->
	Ptr a -> CDouble -> CDouble -> IO (Ptr (CairoSurfaceT s ps))

newtype CairoPdfOutlineT = CairoPdfOutlineT CInt deriving Show

pattern CairoPdfOutlineRoot :: CairoPdfOutlineT
pattern CairoPdfOutlineRoot <- CairoPdfOutlineT #{const CAIRO_PDF_OUTLINE_ROOT} where
	CairoPdfOutlineRoot = CairoPdfOutlineT #{const CAIRO_PDF_OUTLINE_ROOT}

mkOFlag "CairoPdfOutlineFlagOpen" #{const CAIRO_PDF_OUTLINE_FLAG_OPEN}
mkOFlag "CairoPdfOutlineFlagBold" #{const CAIRO_PDF_OUTLINE_FLAG_BOLD}
mkOFlag "CairoPdfOutlineFlagItalic" #{const CAIRO_PDF_OUTLINE_FLAG_ITALIC}

join :: CairoPdfOutlineFlagsT -> CairoPdfOutlineFlagsT -> CairoPdfOutlineFlagsT
join (CairoPdfOutlineFlagsT f1) (CairoPdfOutlineFlagsT f2) = CairoPdfOutlineFlagsT $ f1 .|. f2

cairoPdfSurfaceAddOutline :: PrimMonad m =>
	CairoSurfacePdfT s (PrimState m) -> CairoPdfOutlineT -> Name ->
	Either Name (Int, Maybe (Double, Double)) -> [CairoPdfOutlineFlagsT] -> m CairoPdfOutlineT
cairoPdfSurfaceAddOutline (CairoSurfacePdfT fsr _) (CairoPdfOutlineT pid) nm d fs = unsafeIOToPrim
	$ CairoPdfOutlineT <$> withForeignPtr fsr \psr -> withCString utf8 nm \cnm -> internalAttributes d \cd ->
		c_cairo_pdf_surface_add_outline psr pid cnm cd f
	where
	CairoPdfOutlineFlagsT f = foldr join (CairoPdfOutlineFlagsT 0) fs

foreign import ccall "cairo_pdf_surface_add_outline" c_cairo_pdf_surface_add_outline ::
	Ptr (CairoSurfaceT s ps) -> CInt -> CString -> CString -> #{type cairo_pdf_outline_flags_t} -> IO CInt

cairoPdfSurfaceSetSize :: PrimMonad m =>
	CairoSurfacePdfT s (PrimState m) -> CDouble -> CDouble -> m ()
cairoPdfSurfaceSetSize (CairoSurfacePdfT fsr _) w h =
	unsafeIOToPrim $ withForeignPtr fsr \psr -> c_cairo_pdf_surface_set_size psr w h

foreign import ccall "cairo_pdf_surface_set_size" c_cairo_pdf_surface_set_size ::
	Ptr (CairoSurfaceT s ps) -> CDouble -> CDouble -> IO ()

mkMeta "CairoPdfMetadataTitle" #{const CAIRO_PDF_METADATA_TITLE}
mkMeta "CairoPdfMetadataAuthor" #{const CAIRO_PDF_METADATA_AUTHOR}
mkMeta "CairoPdfMetadataSubject" #{const CAIRO_PDF_METADATA_SUBJECT}
mkMeta "CairoPdfMetadataKeywords" #{const CAIRO_PDF_METADATA_KEYWORDS}
mkMeta "CairoPdfMetadataCreator" #{const CAIRO_PDF_METADATA_CREATOR}
mkMeta "CairoPdfMetadataCreateDate" #{const CAIRO_PDF_METADATA_CREATE_DATE}
mkMeta "CairoPdfMetadataModDate" #{const CAIRO_PDF_METADATA_MOD_DATE}

cairoPdfSurfaceSetMetadata :: PrimMonad m =>
	CairoSurfacePdfT s (PrimState m) -> CairoPdfMetadataT -> String -> m ()
cairoPdfSurfaceSetMetadata (CairoSurfacePdfT fsr _) (CairoPdfMetadataT md) v =
	unsafeIOToPrim $ withForeignPtr fsr \psr -> withCString utf8 v \cv ->
		c_cairo_pdf_surface_set_metadata psr md cv

foreign import ccall "cairo_pdf_surface_set_metadata" c_cairo_pdf_surface_set_metadata ::
	Ptr (CairoSurfaceT s ps) -> #{type cairo_pdf_metadata_t} -> CString -> IO ()

cairoPdfSurfaceSetPageLabel :: PrimMonad m =>
	CairoSurfacePdfT s (PrimState m) -> String -> m ()
cairoPdfSurfaceSetPageLabel (CairoSurfacePdfT fsr _) pl =
	unsafeIOToPrim $ withForeignPtr fsr \psr -> withCString utf8 pl \cpl ->
		c_cairo_pdf_surface_set_page_label psr cpl

foreign import ccall "cairo_pdf_surface_set_page_label" c_cairo_pdf_surface_set_page_label ::
	Ptr (CairoSurfaceT s ps) -> CString -> IO ()

mkVersion "CairoPdfVersion1_4" #{const CAIRO_PDF_VERSION_1_4}
mkVersion "CairoPdfVersion1_5" #{const CAIRO_PDF_VERSION_1_5}

cairoPdfSurfaceRestrictToVersion :: PrimMonad m =>
	CairoSurfacePdfT s (PrimState m) -> CairoPdfVersionT -> m ()
cairoPdfSurfaceRestrictToVersion (CairoSurfacePdfT fsr _) (CairoPdfVersionT v) =
	unsafeIOToPrim $ withForeignPtr fsr \psr ->
		c_cairo_pdf_surface_restrict_to_version psr v

foreign import ccall "cairo_pdf_surface_restrict_to_version" c_cairo_pdf_surface_restrict_to_version ::
	Ptr (CairoSurfaceT s ps) -> #{type cairo_pdf_version_t} -> IO ()

cairoPdfGetVersions :: IO [CairoPdfVersionT]
cairoPdfGetVersions = (CairoPdfVersionT <$>) <$> alloca \ppv -> alloca \pn -> do
	c_cairo_pdf_get_versions ppv pn
	n <- peek pn
	peekArray (fromIntegral n) =<< peek ppv

foreign import ccall "cairo_pdf_get_versions" c_cairo_pdf_get_versions ::
	Ptr (Ptr #{type cairo_pdf_version_t}) -> Ptr CInt -> IO ()

cairoPdfVersionToString :: CairoPdfVersionT -> IO String
cairoPdfVersionToString (CairoPdfVersionT v) = peekCString =<< c_cairo_pdf_version_to_string v

foreign import ccall "cairo_pdf_version_to_string" c_cairo_pdf_version_to_string ::
	#{type cairo_pdf_version_t} -> IO CString
