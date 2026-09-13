module Demo where

#include <ghcautoconf.h>
#include "MachDeps.h"

#if WORD_SIZE_IN_BITS != 64 || SIZEOF_HSWORD != 8
#error compiler configuration changed the Haskell word representation
#endif

-- ghcautoconf.h is one shipped file, which the C compiler reads as well. It
-- gives the word size through ghcplatform.h.
#if SIZEOF_VOID_P != 8
#error ghcautoconf.h did not give the word size
#endif

data Token = Token
