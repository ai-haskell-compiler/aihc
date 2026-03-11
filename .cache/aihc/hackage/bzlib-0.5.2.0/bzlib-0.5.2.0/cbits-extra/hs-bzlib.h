#ifndef HS_BZLIB_EXTRAS
#define HS_BZLIB_EXTRAS

#include "bzlib.h"

void _hs_bzlib_bzCompressEnd(bz_stream * strm);
void _hs_bzlib_bzDecompressEnd(bz_stream * strm);
#endif
