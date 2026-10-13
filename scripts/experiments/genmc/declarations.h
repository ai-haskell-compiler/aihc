#include <stddef.h>

/* GenMC omits these declarations. These declarations supply no implementation.
 */
void *calloc(size_t count, size_t size);
void *realloc(void *pointer, size_t size);
void qsort(void *base, size_t count, size_t size,
           int (*compare)(const void *, const void *));
