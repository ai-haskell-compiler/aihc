#ifndef GHCAUTOCONF_H
#define GHCAUTOCONF_H

#if 0
  The CPP pass over a Haskell source reads this file, and that pass keeps
  comments.  Thus the notes here are in a skipped group and not in a C
  comment.

  GHC writes this header from the results of its configure script.  aihc runs
  no configure script, so feature macros stay undefined.  GHC also keeps a
  feature macro undefined if the host does not have that feature.

  Code reads the word size and the byte order from this header.  Both come
  from ghcplatform.h.  A C compile finds the header of the runtime.  The CPP
  pass finds the header that the compiler synthesizes, which describes the
  Haskell word.
#endif

#include "ghcplatform.h"

#endif
