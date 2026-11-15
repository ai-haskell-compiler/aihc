#include "Rts.h"

/* GHC's Rts.h includes stdio.h, stdlib.h, assert.h, and math.h, so this
   file calls their functions with no include of its own, as the cbits of
   bytebuild do. */
HsInt demo_paste_double(char *buffer, double value) {
    assert(buffer != NULL);
    return (HsInt)sprintf(buffer, "%.14g", sqrt(fabs(value)));
}
