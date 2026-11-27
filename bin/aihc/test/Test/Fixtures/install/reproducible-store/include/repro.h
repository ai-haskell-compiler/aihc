/* The header of the configure script is in the configure directory, which
   the store holds. Thus the capi wrapper reads a header from the store. */
#include "ReproConfig.h"

static inline int repro_twice(int value) { return value * 2; }
