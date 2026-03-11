#include <stdlib.h>
#include "libbf.h"

static
void *libBF_hs_realloc(void *clo, void *ptr, size_t size) {
  return realloc(ptr,size);
}

void bf_context_init_hs(bf_context_t *s) {
  bf_context_init(s, libBF_hs_realloc, NULL);
}

void bf_delete_hs(bf_t *s) {
  bf_delete(s);
}

