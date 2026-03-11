#ifndef LAME_HASKELL_HELPERS_H
#define LAME_HASKELL_HELPERS_H

#define LAME_HASKELL_INT    0
#define LAME_HASKELL_FLOAT  1
#define LAME_HASKELL_DOUBLE 2

#include <lame/lame.h>
#include <stdint.h>
#include <stdlib.h>

int id3tag_set_textinfo_utf16_wrapped(lame_global_flags *, const char *, const uint16_t *, int);
int id3tag_set_comment_utf16_wrapped(lame_global_flags *, const uint16_t *, int);
int lame_encoding_helper(lame_global_flags *, uint64_t, uint64_t, uint16_t, uint16_t, const char *, const char *);

#endif /* LAME_HASKELL_HELPERS_H */
