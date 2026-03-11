#include <stdio.h>

#include "rdtsc.h"

int
main (int argc, char * argv[])
{
  unsigned long long t1, t2;
  int x;

  t1 = rdtsc();
  t2 = rdtsc();
  printf("Cost of rdtsc (function call): %lld\n", t2 - t1, x);

  return 0;
}
