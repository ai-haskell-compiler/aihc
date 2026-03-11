#include "Rts.h"
#include <stdint.h>
#include <string.h>

HsInt bytebuild_paste_double(char *s0, HsInt off, double n) {
    char* start = s0 + off;
    memset(start,0,32);
    sprintf(s0 + off,"%.14g", n);
    size_t r = strlen(start);
    return (HsInt)r;
}
