#include "hs-bzlib.h"

void _hs_bzlib_bzCompressEnd(bz_stream * strm) {
  BZ2_bzCompressEnd(strm);
}

void _hs_bzlib_bzDecompressEnd(bz_stream * strm) {
  BZ2_bzDecompressEnd(strm);
}
