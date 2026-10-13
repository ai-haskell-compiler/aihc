#ifndef AIHC_TRIAL_LIBC_H
#define AIHC_TRIAL_LIBC_H

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

/* These C implementations also execute in the native control program. */
void *trial_malloc(size_t size);
void trial_free(void *pointer);
void *trial_calloc(size_t count, size_t size);
void *trial_realloc(void *pointer, size_t size);
void trial_qsort(void *base, size_t count, size_t size,
                 int (*compare)(const void *, const void *));
void *trial_memcpy(void *destination, const void *source, size_t size);
void *trial_memmove(void *destination, const void *source, size_t size);
void *trial_memset(void *destination, int byte, size_t size);
int trial_memcmp(const void *left, const void *right, size_t size);
size_t trial_strlen(const char *text);

#ifndef AIHC_TRIAL_LIBC_IMPLEMENTATION
#define malloc trial_malloc
#define free trial_free
#define calloc trial_calloc
#define realloc trial_realloc
#define qsort trial_qsort
#undef memcpy
#define memcpy trial_memcpy
#undef memmove
#define memmove trial_memmove
#undef memset
#define memset trial_memset
#define memcmp trial_memcmp
#define strlen trial_strlen
#endif

#endif
