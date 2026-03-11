/* #include "rdtsc.h" */

#include "cycle.h"

unsigned long long 
rdtsc(void)
{
  return getticks();
}

