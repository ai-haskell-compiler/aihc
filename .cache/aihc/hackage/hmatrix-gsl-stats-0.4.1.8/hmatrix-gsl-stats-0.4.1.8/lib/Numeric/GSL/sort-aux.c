#include <gsl/gsl_vector.h>
#include <gsl/gsl_sort.h>

int sort(int vs, const double* v, int rs, double* r)
{
  if (vs != rs) return 2000; // BAD_SIZE
  int i;
  for (i = 0; i < vs; i++) r[i] = v[i];
  gsl_sort(r,1,rs);
}
