{-# LANGUAGE CPP #-}
module Dependency (dependencyFile, dependencyLocation) where

import GHC.Stack (HasCallStack, callStack, prettyCallStack)

locate :: HasCallStack => String
locate = prettyCallStack callStack

#include "dependency.h"
