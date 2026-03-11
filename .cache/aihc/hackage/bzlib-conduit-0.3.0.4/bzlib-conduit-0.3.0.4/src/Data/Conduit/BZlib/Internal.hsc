{-# LANGUAGE ForeignFunctionInterface #-}

#include <bzlib.h>
#include <bindings.dsl.h>

module Data.Conduit.BZlib.Internal where

#strict_import

#num BZ_RUN
#num BZ_FLUSH
#num BZ_FINISH

#num BZ_OK
#num BZ_RUN_OK
#num BZ_FLUSH_OK
#num BZ_FINISH_OK
#num BZ_STREAM_END
#num BZ_SEQUENCE_ERROR
#num BZ_PARAM_ERROR
#num BZ_MEM_ERROR
#num BZ_DATA_ERROR
#num BZ_DATA_ERROR_MAGIC
#num BZ_IO_ERROR
#num BZ_UNEXPECTED_EOF
#num BZ_OUTBUFF_FULL
#num BZ_CONFIG_ERROR

#starttype bz_stream
#field next_in,        Ptr CChar
#field avail_in,       CUInt
#field total_in_lo32,  CUInt
#field total_in_hi32,  CUInt
#field next_out,       Ptr CChar
#field avail_out,      CUInt
#field total_out_lo32, CUInt
#field total_out_hi32, CUInt
#field state,          Ptr ()
#field bzalloc,        Ptr ()
#field bzfree,         Ptr ()
#field opaque,         Ptr ()
#stoptype

#ccall BZ2_bzCompressInit, Ptr <bz_stream> -> CInt -> CInt -> CInt -> IO CInt
#ccall BZ2_bzCompress, Ptr <bz_stream> -> CInt -> IO CInt
#ccall BZ2_bzCompressEnd, Ptr <bz_stream> -> IO CInt

#ccall BZ2_bzDecompressInit, Ptr <bz_stream> -> CInt -> CInt -> IO CInt
#ccall BZ2_bzDecompress, Ptr <bz_stream> -> IO CInt
#ccall BZ2_bzDecompressEnd, Ptr <bz_stream> -> IO CInt
