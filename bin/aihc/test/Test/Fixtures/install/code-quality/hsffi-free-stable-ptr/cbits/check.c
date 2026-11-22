#include "HsFFI.h"
void release_value(HsStablePtr value) { hs_free_stable_ptr(value); }
