#include "libc.h"
#include <assert.h>
#include <stdint.h>

static int compare(const void *left, const void *right) {
  unsigned char first = *(const unsigned char *)left;
  unsigned char second = *(const unsigned char *)right;
  return (first > second) - (first < second);
}

int main(void) {
  unsigned char *bytes = calloc(3, 1);
  assert(bytes != NULL);
  for (size_t index = 0; index < 3; ++index) {
    assert(bytes[index] == 0);
    bytes[index] = (unsigned char)(index + 7);
  }
  unsigned char *grown = realloc(bytes, 5);
  assert(grown != NULL);
  for (size_t index = 0; index < 3; ++index) {
    assert(grown[index] == index + 7);
  }
  unsigned char *shrunk = realloc(grown, 1);
  assert(shrunk != NULL && shrunk[0] == 7);
  assert(realloc(shrunk, SIZE_MAX) == NULL);
  assert(shrunk[0] == 7);
  free(shrunk);
  assert(calloc(SIZE_MAX, 2) == NULL);
  free(NULL);
  unsigned char values[] = {4, 1, 4, 2};
  qsort(values, 4, 1, compare);
  assert(values[0] == 1 && values[1] == 2);
  assert(values[2] == 4 && values[3] == 4);
  char text[] = "abcdef";
  memmove(text + 1, text, 5);
  assert(memcmp(text, "aabcde", 6) == 0);
  memmove(text, text + 1, 5);
  assert(memcmp(text, "abcdee", 6) == 0);
  assert(strlen(text) == 6);
  return 0;
}
