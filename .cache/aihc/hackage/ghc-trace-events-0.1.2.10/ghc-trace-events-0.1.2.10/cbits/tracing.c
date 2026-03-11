#include <stdbool.h>
#include "Rts.h"

bool userTracingEnabled() {
  return RtsFlags.TraceFlags.user;
}
