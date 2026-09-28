module Demo (Answer (..)) where

#include "demo.h"
#if DEMO_ANSWER != 42
#error hsc2hs did not read demo.h
#endif

-- The constant hsc2hs reads out of demo.h with the C compiler.
data Answer = Answer
