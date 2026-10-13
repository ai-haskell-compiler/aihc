#define AIHC_TRIAL_LIBC_IMPLEMENTATION
#include "libc.h"
#include <stdint.h>

/* The header preserves allocation sizes and the alignment of malloc. */
typedef union {
  max_align_t alignment;
  size_t size;
} Allocation;

void *trial_malloc(size_t size) {
  if (size > SIZE_MAX - sizeof(Allocation)) {
    return NULL;
  }
  Allocation *allocation = malloc(sizeof(Allocation) + size);
  if (allocation == NULL) {
    return NULL;
  }
  allocation->size = size;
  return allocation + 1;
}

void trial_free(void *pointer) {
  if (pointer != NULL) {
    free((Allocation *)pointer - 1);
  }
}

void *trial_calloc(size_t count, size_t size) {
  if (size != 0 && count > SIZE_MAX / size) {
    return NULL;
  }
  size_t bytes = count * size;
  void *result = trial_malloc(bytes);
  if (result != NULL) {
    trial_memset(result, 0, bytes);
  }
  return result;
}

void *trial_realloc(void *pointer, size_t size) {
  if (pointer == NULL) {
    return trial_malloc(size);
  }
  if (size == 0) {
    trial_free(pointer);
    return NULL;
  }
  void *result = trial_malloc(size);
  if (result == NULL) {
    return NULL;
  }
  size_t previous = ((Allocation *)pointer - 1)->size;
  trial_memcpy(result, pointer, previous < size ? previous : size);
  trial_free(pointer);
  return result;
}

void *trial_memcpy(void *destination, const void *source, size_t size) {
  unsigned char *out = destination;
  const unsigned char *in = source;
  for (size_t index = 0; index < size; ++index) {
    out[index] = in[index];
  }
  return destination;
}

void *trial_memmove(void *destination, const void *source, size_t size) {
  unsigned char *out = destination;
  const unsigned char *in = source;
  if ((uintptr_t)out < (uintptr_t)in) {
    return trial_memcpy(destination, source, size);
  }
  while (size != 0) {
    --size;
    out[size] = in[size];
  }
  return destination;
}

void *trial_memset(void *destination, int byte, size_t size) {
  unsigned char *out = destination;
  for (size_t index = 0; index < size; ++index) {
    out[index] = (unsigned char)byte;
  }
  return destination;
}

int trial_memcmp(const void *left, const void *right, size_t size) {
  const unsigned char *first = left;
  const unsigned char *second = right;
  for (size_t index = 0; index < size; ++index) {
    if (first[index] != second[index]) {
      return first[index] < second[index] ? -1 : 1;
    }
  }
  return 0;
}

size_t trial_strlen(const char *text) {
  size_t size = 0;
  while (text[size] != '\0') {
    ++size;
  }
  return size;
}

/* Insertion sort needs no temporary allocation and preserves each record. */
void trial_qsort(void *base, size_t count, size_t size,
                 int (*compare)(const void *, const void *)) {
  unsigned char *bytes = base;
  for (size_t index = 1; index < count; ++index) {
    size_t position = index;
    while (position != 0 && compare(bytes + (position - 1) * size,
                                    bytes + position * size) > 0) {
      for (size_t byte = 0; byte < size; ++byte) {
        unsigned char saved = bytes[(position - 1) * size + byte];
        bytes[(position - 1) * size + byte] = bytes[position * size + byte];
        bytes[position * size + byte] = saved;
      }
      --position;
    }
  }
}
