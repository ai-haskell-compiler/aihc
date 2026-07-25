#ifndef AIHC_WASM_STDLIB_H
#define AIHC_WASM_STDLIB_H

#include <stddef.h>

void *calloc(size_t count, size_t size);
void free(void *pointer);
void *malloc(size_t size);
void *realloc(void *pointer, size_t size);
_Noreturn void abort(void);

#endif
