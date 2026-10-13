#include "libc.h"
#include <assert.h>
#include <stdint.h>

int main(void) {
  uint64_t *word = malloc(sizeof(*word));
  assert(word != NULL);
  *word = 7;
  uint64_t *copy = realloc(word, 2 * sizeof(*word));
  assert(copy != NULL && *copy == 7);
  free(copy);
  return 0;
}
