#include <stddef.h>
#include <stdint.h>

extern unsigned char aihc_heap_base __asm__("__heap_base");

static uintptr_t aihc_heap_next;

static uintptr_t aihc_align(uintptr_t value, uintptr_t alignment) {
  return (value + alignment - 1U) & ~(alignment - 1U);
}

void *malloc(size_t size) {
  if (aihc_heap_next == 0) {
    aihc_heap_next = (uintptr_t)&aihc_heap_base;
  }
  size_t requested = size == 0 ? 1 : size;
  uintptr_t payload = aihc_align(aihc_heap_next + sizeof(size_t), 16);
  uintptr_t header = payload - sizeof(size_t);
  uintptr_t end = payload + requested;
  if (payload < aihc_heap_next || end < payload) {
    return NULL;
  }
  size_t memory_size = __builtin_wasm_memory_size(0) * 65536U;
  if (end > memory_size) {
    size_t pages = (end - memory_size + 65535U) / 65536U;
    if (__builtin_wasm_memory_grow(0, pages) == (size_t)-1) {
      return NULL;
    }
  }
  *(size_t *)header = requested;
  aihc_heap_next = end;
  return (void *)payload;
}

void free(void *pointer) { (void)pointer; }

_Noreturn void abort(void) { __builtin_trap(); }

void *memset(void *destination, int byte, size_t length) {
  unsigned char *output = destination;
  for (size_t index = 0; index < length; ++index) {
    output[index] = (unsigned char)byte;
  }
  return destination;
}

void *memcpy(void *destination, const void *source, size_t length) {
  unsigned char *output = destination;
  const unsigned char *input = source;
  for (size_t index = 0; index < length; ++index) {
    output[index] = input[index];
  }
  return destination;
}

void *memmove(void *destination, const void *source, size_t length) {
  unsigned char *output = destination;
  const unsigned char *input = source;
  if (output < input) {
    return memcpy(destination, source, length);
  }
  for (size_t index = length; index != 0; --index) {
    output[index - 1] = input[index - 1];
  }
  return destination;
}

void *calloc(size_t count, size_t size) {
  if (size != 0 && count > SIZE_MAX / size) {
    return NULL;
  }
  size_t bytes = count * size;
  void *result = malloc(bytes == 0 ? 1 : bytes);
  if (result != NULL) {
    memset(result, 0, bytes);
  }
  return result;
}

void *realloc(void *pointer, size_t size) {
  if (pointer == NULL) {
    return malloc(size);
  }
  void *result = malloc(size);
  if (result == NULL) {
    return NULL;
  }
  size_t old_size = *(size_t *)((uintptr_t)pointer - sizeof(size_t));
  memcpy(result, pointer, old_size < size ? old_size : size);
  return result;
}
