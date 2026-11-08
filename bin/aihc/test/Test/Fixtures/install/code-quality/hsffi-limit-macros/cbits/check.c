#include "HsFFI.h"
#if HS_CHAR_MAX != 0x10FFFF || HS_BOOL_TRUE != 1
#error HsFFI.h must define the Haskell 2010 limit symbols
#endif
HsInt int_bounds_differ(void) { return HS_INT_MIN < 0 && HS_INT_MAX > 0 && HS_WORD8_MAX == 255; }
HsDouble double_epsilon(void) { return HS_DOUBLE_EPSILON + HS_FLOAT_MAX; }
