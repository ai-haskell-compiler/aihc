#include <stdlib.h>

extern unsigned int keylookup[];
extern char *toASCIILut[];

int
comparator (const void *key, const void *member)
{
  unsigned int keyU = (*((unsigned int *) key));
  unsigned int memberU = (*((unsigned int *) member));

  if (keyU < memberU) {
      return -1;
  } else if (keyU > memberU) {
      return 1;
  }
  return 0;
}

char *
findString (unsigned int index)
{
  unsigned int *out_index =
    bsearch (&index, &keylookup, 8922, sizeof (unsigned int), &comparator);

  if (out_index == NULL) {
    // not found
    return "";
  } else {
    return toASCIILut[out_index - keylookup];
  }
}
