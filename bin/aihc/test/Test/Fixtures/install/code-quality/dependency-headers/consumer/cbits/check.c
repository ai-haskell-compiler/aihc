#include "Provider.h"
#include "ProviderConfig.h"
#if PROVIDER_STATIC != 17 || PROVIDER_CONFIGURED != 23
#error C dependency header values are incorrect
#endif
int dependency_header_value(void) { return PROVIDER_CONFIGURED; }
