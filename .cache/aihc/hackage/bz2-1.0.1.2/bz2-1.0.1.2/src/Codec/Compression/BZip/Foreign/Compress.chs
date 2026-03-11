module Codec.Compression.BZip.Foreign.Compress ( bZ2BzCompressInit
                                               , bZ2BzCompress
                                               , bZ2BzCompressEnd
                                               , BzStreamPtr
                                               ) where

{# import Codec.Compression.BZip.Foreign.Common #}

import Foreign.C.Types (CInt)

#include <bzlib.h>

{#pointer *bz_stream as BzStreamPtr foreign finalizer BZ2_bzCompressEnd as ^ -> BzStream #}

{# fun BZ2_bzCompressInit as ^ { `BzStreamPtr', `CInt', `CInt', `CInt' } -> `()' bzWrap*- #}
{# fun BZ2_bzCompress as ^ { `BzStreamPtr', `BZAction' } -> `BZError' bzWrap* #}
