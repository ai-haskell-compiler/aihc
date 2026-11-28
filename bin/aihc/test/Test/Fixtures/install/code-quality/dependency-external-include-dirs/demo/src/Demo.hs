module Demo (answer) where

#include "sysdep_answer.h"

import Sysdep (Unit (..))

answer :: Unit
#if SYSDEP_ANSWER == 42
answer = Unit
#endif
