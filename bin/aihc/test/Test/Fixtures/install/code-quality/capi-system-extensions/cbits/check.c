#include "HsFFI.h"
#if !defined(_GNU_SOURCE)
#error HsFFI.h must enable the system extensions as GHC's ghcautoconf.h does
#endif
#include <stdlib.h>
char *terminal_name(int fd) { return ptsname(fd); }
