module Demo where

#include "HsFFI.h"

#if !defined(HAVE_FCNTL_H)
#error HsFFI.h must expose HAVE_FCNTL_H, as GHC does
#endif
#include <fcntl.h>

import GHC.Types (Int)

flag :: Int
flag = #const O_RDONLY
