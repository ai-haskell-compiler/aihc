module Demo where

#include "DemoConfig.h"

#if !defined(DEMO_CONFIGURED)
#error the configure script did not write DemoConfig.h
#endif

#if !defined(DEMO_HOOKED_HS)
#error the cpp-options from demo.buildinfo are missing
#endif


configured value = value
