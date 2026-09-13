#ifndef GHCAUTOCONF_H
#define GHCAUTOCONF_H

/* GHC writes this header from the results of its configure script. aihc does
   not run a configure script. C code of a package reads the word size and the
   byte order from this header. These come from ghcplatform.h, which gets them
   from the target of the C compiler. Feature macros stay undefined. GHC also
   keeps a feature macro undefined if the host does not have that feature. */
#include "ghcplatform.h"

#endif
