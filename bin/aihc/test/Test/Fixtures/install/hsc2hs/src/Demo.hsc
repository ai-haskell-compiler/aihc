module Demo (answer) where

#include "demo.h"

-- The constant hsc2hs reads out of demo.h with the C compiler.
answer = #{const DEMO_ANSWER}
