{- ORACLE_TEST
id: hashable-xxh3-cpp-include
category: corpus
expected: pass
reason: from hashable/src/Data/Hashable/XXH3.hs; covered by parser-test CPP preprocessing
-}
module X where

#include "MachDeps.h"
