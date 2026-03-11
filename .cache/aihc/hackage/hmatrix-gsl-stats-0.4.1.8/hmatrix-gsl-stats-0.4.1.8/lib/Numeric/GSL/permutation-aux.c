#include <gsl/gsl_permutation.h>
#include <gsl/gsl_permute.h>
     
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

int random_permute(int s, gsl_permutation* p)
{
  const gsl_rng_type * T;
  gsl_rng * r;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  r = gsl_rng_alloc(T);
  gsl_rng_set(r,s);

  gsl_ran_shuffle(r, p->data, p->size, 1);

  gsl_rng_free(r);

  return 0;
}

int permute(const gsl_permutation* p, int vs, const double* v, int rs, double* r)
{
  int i;
  for (i = 0; i < vs; i++)
    r[i] = v[i];
  return gsl_permute(p->data,r,1,vs);
}

int permute_inverse(const gsl_permutation* p, int vs, const double* v, int rs, double* r)
{
  int i;
  for (i = 0; i < vs; i++)
    r[i] = v[i];
  return gsl_permute_inverse(p->data,r,1,vs);
}

int perm_fwrite(const char* filename, const gsl_permutation* p)
{
  int err;
  FILE* f = fopen(filename,"w");
  if (!f) return 2003; // BAD_FILE
  err = gsl_permutation_fwrite(f,p);
  fclose(f);
  return err;
}

int perm_fread(const char* filename, gsl_permutation* p)
{
  int err;
  FILE* f = fopen(filename,"r");
  if (!f) return 2003; // BAD_FILE
  err = gsl_permutation_fread(f,p);
  fclose(f);
  return err;
}

int perm_fprintf(const char* filename, const char* fmt, const gsl_permutation* p)
{
  int err;
  FILE* f = fopen(filename,"w");
  if (!f) return 2003; // BAD_FILE
  err = gsl_permutation_fprintf(f,p,fmt);
  fclose(f);
  return err;
}

int perm_fscanf(const char* filename, gsl_permutation* p)
{
  int err;
  FILE* f = fopen(filename,"r");
  if (!f) return 2003; //BAD_FILE
  err = gsl_permutation_fscanf(f,p);
  fclose(f);
  return err;
}


