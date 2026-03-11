#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>

#include <gsl/gsl_histogram.h>
#include <gsl/gsl_histogram2d.h>

#include <stdio.h>

int from_vectors(gsl_histogram * H, int rs, const double* r, int bs, const double* b)
{
  if (rs != bs + 1) return 2000; // BAD_SIZE
  int i;
  for (i = 0; i < rs - 1; i++)
    gsl_histogram_accumulate(H,(r[i]+r[i+1])/2,b[i]);
  return 0;
}

int to_vectors(gsl_histogram * H, int rs, double* r, int bs, double* b)
{
  int i;
  for (i = 0; i < rs; i++)
    r[i] = H->range[i];
  for (i = 0; i < bs; i++)
    b[i] = H->bin[i];
  return 0;
}

int increment_vector(gsl_histogram* H, int vs, const double* v)
{
  int i;
  for (i = 0; i < vs; i++)
    gsl_histogram_increment(H,v[i]);
  return 0;
}

int accumulate_vector(gsl_histogram* H, int vs, const double* v, int ws, const double* w)
{
  if (vs != ws) return 2000; // BAD_SIZE
  int i;
  for (i = 0; i < vs; i++)
    gsl_histogram_accumulate(H,v[i],w[i]);
  return 0;
}

int hist_fwrite(const char* filename, const gsl_histogram* h)
{
  int err;
  FILE* f = fopen(filename,"w");
  if (!f) return 2003; // BAD_FILE
  err = gsl_histogram_fwrite(f,h);
  fclose(f);
  return err;
}

int hist_fread(const char* filename, gsl_histogram* h)
{
  int err;
  FILE* f = fopen(filename,"r");
  if (!f) return 2003; // BAD_FILE
  err = gsl_histogram_fread(f,h);
  fclose(f);
  return err;
}

int hist_fprintf(const char* filename, const char* rfmt, const char* bfmt, const gsl_histogram* h)
{
  int err;
  FILE* f = fopen(filename,"w");
  if (!f) return 2003; // BAD_FILE
  err = gsl_histogram_fprintf(f,h,rfmt,bfmt);
  fclose(f);
  return err;
}

int hist_fscanf(const char* filename, gsl_histogram* h)
{
  int err;
  FILE* f = fopen(filename,"r");
  if (!f) return 2003; //BAD_FILE
  err = gsl_histogram_fscanf(f,h);
  fclose(f);
  return err;
}

//////////////////////

int from_matrix(gsl_histogram2d * H, int rxs, const double* rx, int rys, const double* ry, int bx, int by, const double* b)
{
  if (rxs != bx + 1) return 2000; // BAD_SIZE
  if (rys != by + 1) return 2000; // BAD_SIZE

  int i, j;
  for (i = 0; i < rxs - 1; i++)
    for (j = 0; j <rys - 1; j++)
      gsl_histogram2d_accumulate(H,(rx[i]+rx[i+1])/2,(ry[j]+ry[j+1])/2,b[i*by+j]);
  return 0;
}

int to_matrix(gsl_histogram2d * H, int rxs, double* rx, int rys, double* ry, int bx, int by, double* b)
{
  int bz = (rxs-1)*(rys-1); 
  if (bx*by != bz) return 2000; // BAD_SIZE
  int i,j;
  for (i = 0; i < rxs; i++)
    rx[i] = H->xrange[i];
  for (i = 0; i < rys; i++)
    ry[i] = H->yrange[i];
  for (i = 0; i < bz; i++)
    b[i] = H->bin[i];
  return 0;
}

int increment_matrix(gsl_histogram2d* H, int xs, const double* x, int ys, const double* y)
{
  if (xs != ys) return 2000; // BAD_SIZE
  int i;
  for (i = 0; i < xs; i++)
    gsl_histogram2d_increment(H,x[i],y[i]);
  return 0;
}

int accumulate_matrix(gsl_histogram2d* H, int xs, const double* x, int ys, const double* y, int ws, const double* w)
{
  if (xs != ys) return 2000; // BAD_SIZE
  if (xs != ws) return 2000; // BAD_SIZE
  int i;
  for (i = 0; i < xs; i++)
    gsl_histogram2d_accumulate(H,x[i],y[i],w[i]);
  return 0;
}

int hist2d_count(gsl_histogram2d* H, int xs, const double* x, int ys, const double* y, int rs, double* res)
{
  if (xs != ys) return 2000; // BAD_SIZE
  if (xs != rs) return 2000; // BAD_SIZE
  int i;
  size_t r,c;
  int err;

  for (i = 0; i < xs; i++) {
    err = gsl_histogram2d_find(H,x[i],y[i],&r,&c);
    if (err != 0) return 0;
    else { res[i] = gsl_histogram2d_get(H,r,c); }
  }

  return 0;
}
      
int hist2d_count_pair(gsl_histogram2d* H, int xs, const double* x, int rs, double* res)
{
  if (xs != rs) return 2000; // BAD_SIZE
  int i;
  size_t r,c;
  int err;

  for (i = 0; i < xs; i++) {
    err = gsl_histogram2d_find(H,x[i*2],x[i*2+1],&r,&c);
    if (err != 0) return 0;
    else { res[i] = gsl_histogram2d_get(H,r,c); }
  }

  return 0;
}
      
int hist2d_fwrite(const char* filename, const gsl_histogram2d* h)
{
  int err;
  FILE* f = fopen(filename,"w");
  if (!f) return 2003; // BAD_FILE
  err = gsl_histogram2d_fwrite(f,h);
  fclose(f);
  return err;
}

int hist2d_fread(const char* filename, gsl_histogram2d* h)
{
  int err;
  FILE* f = fopen(filename,"r");
  if (!f) return 2003; // BAD_FILE
  err = gsl_histogram2d_fread(f,h);
  fclose(f);
  return err;
}

int hist2d_fprintf(const char* filename, const char* rfmt, const char* bfmt, const gsl_histogram2d* h)
{
  int err;
  FILE* f = fopen(filename,"w");
  if (!f) return 2003; // BAD_FILE
  err = gsl_histogram2d_fprintf(f,h,rfmt,bfmt);
  fclose(f);
  return err;
}

int hist2d_fscanf(const char* filename, gsl_histogram2d* h)
{
  int err;
  FILE* f = fopen(filename,"r");
  if (!f) return 2003; //BAD_FILE
  err = gsl_histogram2d_fscanf(f,h);
  fclose(f);
  return err;
}

///////////////////////////////////////////////////////////////

int unzip_double_pair(int vs, const double* v, int as, double* a, int bs, double* b)
{
  if (vs != as || vs != bs) return 2000; // BAD_SIZE

  int i;
  for (i = 0; i < vs; i++) {
    a[i] = v[i*2];
    b[i] = v[i*2+1];
  }

  return 0;
}
