module Demo where

#include "HsFFI.h"

#if defined(darwin_HOST_OS) || defined(linux_HOST_OS)
#if !defined(HAVE_DLFCN_H)
#error HsFFI.h must expose HAVE_DLFCN_H for this target
#endif
#include <dlfcn.h>
#else
#if defined(HAVE_DLFCN_H)
#error This target has no dynamic linker header
#endif
#endif

import GHC.Types (Int)

flag :: Int
#if defined(HAVE_DLFCN_H)
flag = #const RTLD_NOW
#else
flag = 0
#endif
