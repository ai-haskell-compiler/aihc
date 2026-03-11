module Codec.Compression.BZip.Common ( bzStreamInit ) where

import Codec.Compression.BZip.Foreign.Common (BzStream)
import Control.Applicative
import Foreign.Ptr (nullFunPtr, nullPtr, Ptr)
import Foreign.Marshal (mallocBytes)

#include <bzlib.h>

bzStreamInit :: IO (Ptr BzStream)
bzStreamInit = do
    p <- mallocBytes {# sizeof bz_stream #}
    {# set bz_stream.bzalloc #} p nullFunPtr
    {# set bz_stream.bzfree #} p nullFunPtr
    {# set bz_stream.opaque #} p nullPtr
    pure p
