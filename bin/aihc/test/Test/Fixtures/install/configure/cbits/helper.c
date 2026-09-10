#include "DemoConfig.h"

#if !DEMO_CONFIGURED
#error the configure script did not write DemoConfig.h
#endif

#if !DEMO_HOOKED
#error the cc-options from demo.buildinfo are missing
#endif

int aihc_configured(void) { return DEMO_CONFIGURED; }
