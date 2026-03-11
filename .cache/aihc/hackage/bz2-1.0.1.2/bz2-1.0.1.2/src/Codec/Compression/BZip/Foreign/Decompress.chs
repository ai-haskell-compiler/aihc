module Codec.Compression.BZip.Foreign.Decompress ( bZ2BzDecompressInit
                                                 , bZ2BzDecompress
                                                 , bZ2BzDecompressEnd
                                                 , BzStreamPtr
                                                 ) where

{# import Codec.Compression.BZip.Foreign.Common #}

import Foreign.C.Types (CInt)

#include <bzlib.h>

{#pointer *bz_stream as BzStreamPtr foreign finalizer BZ2_bzDecompressEnd as ^ -> BzStream #}

{# fun BZ2_bzDecompressInit as ^ { `BzStreamPtr', `CInt', `Bool' } -> `()' bzWrap*- #}
{# fun BZ2_bzDecompress as ^ { `BzStreamPtr' } -> `BZError' bzWrap* #}
