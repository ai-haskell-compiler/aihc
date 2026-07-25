#ifndef AIHC_WASM_STRING_H
#define AIHC_WASM_STRING_H

#include <stddef.h>

void *memcpy(void *destination, const void *source, size_t length);
void *memmove(void *destination, const void *source, size_t length);
void *memset(void *destination, int byte, size_t length);

#endif
