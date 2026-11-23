{-# LANGUAGE CPP #-}
module Cpp (cppFile, headerFile, headerLocation) where

import GHC.Stack (HasCallStack, callStack, prettyCallStack)

cppFile :: String
cppFile = __FILE__

location :: HasCallStack => String
location = prettyCallStack callStack

#include "cpp.h"
