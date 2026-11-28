#include <stdint.h>

/* The Haskell module defines these two functions. */
int64_t hs_twice(int64_t value);
int64_t hs_bump(int64_t value);

int64_t c_call_exports(int64_t value) {
  return hs_bump(hs_twice(value));
}
