{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Surfaces.CairoWriteFuncT where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.Primitive
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

import qualified Data.ByteString as BS

#include <cairo.h>

data WriteResult = WriteFailure | WriteSuccess deriving Show

writeResultToCairoStatusT :: WriteResult -> #{type cairo_status_t}
writeResultToCairoStatusT = \case
	WriteFailure -> #{const CAIRO_STATUS_WRITE_ERROR}
	WriteSuccess -> #{const CAIRO_STATUS_SUCCESS}

foreign import ccall "wrapper" c_wrap_cairo_write_func_t ::
	(Ptr a -> CString -> CInt -> IO #{type cairo_status_t}) ->
	IO (FunPtr (Ptr a -> CString -> CInt -> IO #{type cairo_status_t}))

wrapCairoWriteFuncTText :: PrimBase m => (Ptr a -> T.Text -> m WriteResult) ->
	IO (FunPtr (Ptr a -> CString -> CInt -> IO #{type cairo_status_t}))
wrapCairoWriteFuncTText wf = c_wrap_cairo_write_func_t $ convertCairoWriteFuncTText wf

wrapCairoWriteFuncTByteString :: PrimBase m => (Ptr a -> BS.ByteString -> m WriteResult) ->
	IO (FunPtr (Ptr a -> CString -> CInt -> IO #{type cairo_status_t}))
wrapCairoWriteFuncTByteString = c_wrap_cairo_write_func_t . convertCairoWriteFuncTByteString

convertCairoWriteFuncT :: (Ptr a -> String -> IO #{type cairo_status_t}) ->
	Ptr a -> CString -> CInt -> IO #{type cairo_status_t}
convertCairoWriteFuncT wf p cs ln = peekCStringLen (cs, fromIntegral ln) >>= \s -> wf p s

convertCairoWriteFuncTText :: PrimBase m => (Ptr a -> T.Text -> m WriteResult) ->
	Ptr a -> CString -> CInt -> IO #{type cairo_status_t}
convertCairoWriteFuncTText wf p cs ln = writeResultToCairoStatusT
	<$> (T.peekCStringLen (cs, fromIntegral ln) >>= \t -> unsafePrimToIO $ wf p t)

convertCairoWriteFuncTByteString :: PrimBase m => (Ptr a -> BS.ByteString -> m WriteResult) ->
	Ptr a -> CString -> CInt -> IO #{type cairo_status_t}
convertCairoWriteFuncTByteString wf p cs ln = writeResultToCairoStatusT
	<$> (BS.packCStringLen (cs, fromIntegral ln) >>= \bs -> unsafePrimToIO $ wf p bs)
