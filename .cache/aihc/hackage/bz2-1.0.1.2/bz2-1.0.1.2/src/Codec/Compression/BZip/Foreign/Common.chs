{-# LANGUAGE DeriveDataTypeable #-}

module Codec.Compression.BZip.Foreign.Common ( -- * Types
                                               BZAction (..)
                                             , BZError (..)
                                             , BzStream
                                             -- * Helper
                                             , bzWrap
                                             -- * Contributed functions
                                             , bZ2BzlibVersion
                                             ) where

import Control.Applicative
import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt)

#include <bzlib.h>

{# enum define BZAction { BZ_RUN as BzRun
                        , BZ_FLUSH as BzFlush
                        , BZ_FINISH as BzFinish
                        }
  #}

{# enum define BZError { BZ_OK as BzOk
                       , BZ_RUN_OK as BzRunOk
                       , BZ_FLUSH_OK as BzFlushOk
                       , BZ_FINISH_OK as BzFinishOk
                       , BZ_STREAM_END as BzStreamEnd
                       , BZ_SEQUENCE_ERROR as BzSequenceError
                       , BZ_PARAM_ERROR as BzParamError
                       , BZ_MEM_ERROR as BzMemError
                       , BZ_DATA_ERROR as BzDataError
                       , BZ_DATA_ERROR_MAGIC as BzDataErrorMagic
                       , BZ_IO_ERROR as BzIoError
                       , BZ_UNEXPECTED_EOF as BzUnexpectedEof
                       , BZ_OUTBUFF_FULL as BzOutbuffFull
                       , BZ_CONFIG_ERROR as BzConfigError
                       } deriving (Eq, Show, Typeable)
  #}

instance Exception BZError where

-- | Abstract type
data BzStream

-- Contributed functions
{# fun pure BZ2_bzlibVersion as ^ { } -> `String' #}

bzWrap :: CInt -> IO BZError
bzWrap err =
    let err' = toEnum (fromIntegral err) in
    case err' of
        BzOk        -> pure err'
        BzRunOk     -> pure err'
        BzFlushOk   -> pure err'
        BzFinishOk  -> pure err'
        BzStreamEnd -> pure err'
        x           -> throw x
