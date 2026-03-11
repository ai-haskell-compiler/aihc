{-# LANGUAGE CPP, FlexibleContexts #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#define HasCallStack_ HasCallStack =>
#else
#define HasCallStack_
#endif

module Example where

import           Data.CallStack

test :: CallStack
test = foo

foo :: HasCallStack_ CallStack
foo = bar

bar :: HasCallStack_ CallStack
bar = callStack
