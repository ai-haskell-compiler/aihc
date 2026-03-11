#include <stdio.h>

/* cf. FFI cookbook */
#if __GLASGOW_HASKELL__ < 800
#define hsc_alignment(t) \
    printf("(%lu)", (unsigned long)offsetof(struct {char x__; t (y__); }, y__));
#endif
