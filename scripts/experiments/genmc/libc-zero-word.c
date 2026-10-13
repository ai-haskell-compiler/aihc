#include "libc.h"
#include <assert.h>
#include <stdint.h>

int main(void) {
  uint64_t *word = calloc(1, sizeof(*word));
  assert(word != NULL);
  assert(*word == 0);
  free(word);
  return 0;
}
