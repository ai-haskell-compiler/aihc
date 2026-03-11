#include <gsl/gsl_complex.h>

#include <gsl/gsl_math.h>
#include <gsl/gsl_fft_real.h>
#include <gsl/gsl_fft_halfcomplex.h>
#include <gsl/gsl_fft_complex.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_poly.h>
#include <gsl/gsl_statistics_double.h>
#include <gsl/gsl_statistics_float.h>

#include <stdio.h>

int vector_float_convolve(int cs, const float* c, int as, const float* a, int rs, float* r)
{
  int h = cs / 2;
  int li, ri;
  int i,j;

  for (i = 0; i < cs; i++) {
    li = i - h;
    ri = i + h;
    r[i] = 0;
    for (j = (li >= 0 ? li : 0); j < (ri < as ? ri : (as - 1)); j++) {
      r[i] += a[j]*c[j+h+1];
    }
  }
  return 0;
}

int vector_double_convolve(int cs, const double* c, int as, const double* a, int rs, double* r)
{
  int h = cs / 2;
  int li, ri;
  int i,j;

  for (i = 0; i < cs; i++) {
    li = i - h;
    ri = i + h;
    r[i] = 0;
    for (j = (li >= 0 ? li : 0); j < (ri < as ? ri : (as - 1)); j++) {
      r[i] += a[j]*c[j+h+1];
    }
  }
  return 0;
}

int vector_complex_convolve(int cs, const gsl_complex* c, int as, const gsl_complex* a, int rs, gsl_complex* r)
{
  int h = cs / 2;
  int li, ri;
  int i,j;

  for (i = 0; i < cs; i++) {
    li = i - h;
    ri = i + h;
    r[i].dat[0] = 0;
    r[i].dat[1] = 0;
    for (j = (li >= 0 ? li : 0); j < (ri < as ? ri : (as - 1)); j++) {
      r[i].dat[0] += a[j].dat[0]*c[j+h+1].dat[0]-a[j].dat[1]*c[j+h+1].dat[1];
      r[i].dat[1] += a[j].dat[0]*c[j+h+1].dat[1]+a[j].dat[1]*c[j+h+1].dat[0];
    }
  }
  return 0;
}

int filter_double(int ls, const double* l, int ks, const double* k, int vs, const double* v, int rs, double* r)
{
  if (ls > vs || ks > vs) return 2000; // BAD_SIZE

  int i,j;

  double L = l[0];
  double K = k[0];
  
  int N = ls - 1;
  int M = ks - 1;

  for (i = 0; i < vs; i++) {
    r[i] = 0;
    for (j = 0; j < N; j++) {
      if (i - j > 0) r[i] -= (l[j+1])*v[i-j];
    }
    for (j = 0; j < M; j++) {
      if (i - j > 0) r[i] += (k[j+1])*r[i-j];
    }
  }
  return 0;
}

int filter_float(int ls, const float* l, int ks, const float* k, int vs, const float* v, int rs, float* r)
{
  if (ls > vs || ks > vs) return 2000; // BAD_SIZE

  int i,j;

  float L = l[0];
  float K = k[0];
  
  int N = ls - 1;
  int M = ks - 1;

  for (i = 0; i < vs; i++) {
    r[i] = 0;
    for (j = 0; j < N; j++) {
      if (i - j > 0) r[i] -= (l[j+1])*v[i-j];
    }
    for (j = 0; j < M; j++) {
      if (i - j > 0) r[i] += (k[j+1])*r[i-j];
    }
  }
  return 0;
}

int hilbert(int rs, gsl_complex* r)
{
  int s = rs;

  gsl_fft_complex_wavetable * wavetable = gsl_fft_complex_wavetable_alloc (s);
  gsl_fft_complex_workspace * workspace = gsl_fft_complex_workspace_alloc (s);

  // forward fourier transform
  gsl_fft_complex_forward ((double*)r, 1, s, wavetable, workspace);
  // zero negative coefficients and double positive

  int i;
  int m = s/2;
  for (i = 1; i < s; i++) {
    if (i <= m) {
      r[i].dat[0] *= 2;
      r[i].dat[1] *= 2;
    }
    else if (s % 2 == 0 && i == m+1) {
    }
    else {
      r[i].dat[0] = 0;
      r[i].dat[1] = 0;
    }
  }

  // inverse fourier transform
  gsl_fft_complex_inverse ((double*)r, 1, s, wavetable, workspace);

  gsl_fft_complex_wavetable_free (wavetable);
  gsl_fft_complex_workspace_free (workspace);

  return 0;
}

int pwelch(int w, int vs, const gsl_complex* v, int rs, double* r)
{
  if (w > vs) return 2000; // BAD_SIZE

  int i,j;

  int fs = w;

  int num_windows = vs / fs; // ignore end

  double s[fs];
  for (i = 0; i < fs; i++) s[i] = 0;

  gsl_fft_complex_wavetable * wavetable = gsl_fft_complex_wavetable_alloc (fs);
  gsl_fft_complex_workspace * workspace = gsl_fft_complex_workspace_alloc (fs);

  gsl_complex* f = malloc(sizeof(gsl_complex)*fs);
  gsl_vector_view F = gsl_vector_view_array((double*)f, 2*fs);

  gsl_vector_view X;

  for (i = 0; i < num_windows; i++) {
    X = gsl_vector_view_array((double*)(&v[i*fs]), 2*fs); // v is gsl_complex*
    gsl_blas_dcopy(&X.vector,&F.vector);
    gsl_fft_complex_forward ((double*)f, 1, fs, wavetable, workspace);
    for (j = 0; j < fs; j++) s[j] += f[j].dat[0]*f[j].dat[0] + f[j].dat[1]*f[j].dat[1];
  }
  for (j = 0; j < rs; j++) {
    if (j == 0) r[j] = s[j];
    else if (j == (rs-1)) r[j] = s[j];
    else r[j] = s[j] + s[fs-j+1];
    
    r[j] /= num_windows;
    r[j] = sqrt(r[j]);
  }
  gsl_fft_complex_wavetable_free (wavetable);
  gsl_fft_complex_workspace_free (workspace);

  free(f);

  return 0;
}

int hamming_double(int rs, double* r)
{
  int i;

  for (i = 0; i < rs; i++) r[i] = 0.54 - 0.46 * cos(2*M_PI*i/rs);

  return 0;
}

int hamming_float(int rs, float* r)
{
  int i;

  for (i = 0; i < rs; i++) r[i] = 0.54 - 0.46 * cos(2*M_PI*i/rs);

  return 0;
}

int real_poly_complex_eval(int cs, const double* c, int zs, const gsl_complex* z, int rs, gsl_complex* r)
{
  int i;
  
  for (i = 0; i < zs; i++)
    r[i] = gsl_poly_complex_eval(c,cs,z[i]);

  return 0;
} 

int complex_power_double(int cs, const gsl_complex* c, int rs, double* r)
{
  if (rs != cs) return 2000; // BAD_SIZE

  int i;

  for (i = 0; i < cs; i++)
    r[i] = c[i].dat[0]*c[i].dat[0] + c[i].dat[1]*c[i].dat[1];

  return 0;
}

int complex_power_float(int cs, const gsl_complex* c, int rs, float* r)
{
  if (rs != cs) return 2000; // BAD_SIZE

  int i;

  for (i = 0; i < cs; i++)
    r[i] = c[i].dat[0]*c[i].dat[0] + c[i].dat[1]*c[i].dat[1];

  return 0;
}

int downsample_double(int n, int xs, const double* x, int rs, double* r)
{
  if (rs != xs/n) return 2000; // BAD_SIZE
  
  int i;

  for (i = 0; i < rs; i++)
    r[i] = x[i*n];

  return 0;
}

int downsample_float(int n, int xs, const float* x, int rs, float* r)
{
  if (rs != xs/n) return 2000; // BAD_SIZE
  
  int i;

  for (i = 0; i < rs; i++)
    r[i] = x[i*n];

  return 0;
}

int vector_diff_double(int xs, const double* x, int rs, double* r)
{
  if (rs != xs - 1) return 2000; // BAD_SIZE

  int i;

  for (i = 0; i < rs; i++)
    r[i] = x[i+1] - x[i];

  return 0;
}

int vector_diff_float(int xs, const float* x, int rs, float* r)
{
  if (rs != xs - 1) return 2000; // BAD_SIZE

  int i;

  for (i = 0; i < rs; i++)
    r[i] = x[i+1] - x[i];

  return 0;
}

int unwrap_double(int xs, const double* x, int rs, double* r)
{
  if (rs != xs) return 2000; // BAD_SIZE

  int i;

  r[0] = x[0];

  double c = 0;

  int tmp;

  for (i = 1; i < rs; i++) {
    tmp = x[i-1] - x[i];
    if (tmp > M_PI) {
      r[i] = 2*M_PI;
    }
    else if (tmp < (-M_PI)) {
      r[i] = -2*M_PI;
    }
    else {
      r[i] = 0;
    }
    c += r[i];
    r[i] = c + x[i];
  }
  
  return 0;
}

int unwrap_float(int xs, const float* x, int rs, float* r)
{
  if (rs != xs) return 2000; // BAD_SIZE

  int i;

  r[0] = x[0];

  double c = 0;

  int tmp;

  for (i = 1; i < rs; i++) {
    tmp = x[i-1] - x[i];
    if (tmp > M_PI) {
      r[i] = 2*M_PI;
    }
    else if (tmp < (-M_PI)) {
      r[i] = -2*M_PI;
    }
    else {
      r[i] = 0;
    }
    c += r[i];
    r[i] = c + x[i];
  }

  return 0;
}

int cross_covariance_double(int max_lag,
			    double* sx,
			    double* sy,
			    int xs, const double* x,
			    int ys, const double* y,
			    int rs, double* r)
{
  if (xs != ys)        return 2000; // BAD_SIZE
  if (rs != 2*max_lag) return 2000; // BAD_SIZE

  double mx = gsl_stats_mean(x,1,xs);
  double my = gsl_stats_mean(y,1,ys);

  *sx = gsl_stats_sd(x,1,xs);
  *sy = gsl_stats_sd(y,1,ys);

  int delay;
  int i, j;

  double sxy;

  for (delay=-max_lag; delay < max_lag; delay++) {
    sxy = 0;
    for (i=0;i<xs;i++) {
      j = i+delay;
      if (j < 0 || j >= xs) sxy += (x[i]-mx)*(-my);
      else sxy += (x[i]-mx)*(y[j]-my);
      /* or should it be:
      if (j < 0 || j >= xs) continue;
      else sxy += (x[i]-mx)*(y[j]-my);
      */
    }
    r[delay+max_lag] = sxy/xs;
  }

  return 0;
}

int cross_covariance_float(int max_lag,
			    float* sx,
			    float* sy,
			    int xs, const float* x,
			    int ys, const float* y,
			    int rs, double* r)
{
  if (xs != ys)        return 2000; // BAD_SIZE
  if (rs != 2*max_lag) return 2000; // BAD_SIZE

  float mx = gsl_stats_float_mean(x,1,xs);
  float my = gsl_stats_float_mean(y,1,ys);

  *sx = gsl_stats_float_sd(x,1,xs);
  *sy = gsl_stats_float_sd(y,1,ys);

  int delay;
  int i, j;

  float sxy;

  for (delay=-max_lag; delay < max_lag; delay++) {
    sxy = 0;
    for (i=0;i<xs;i++) {
      j = i+delay;
      if (j < 0 || j >= xs) sxy += (x[i]-mx)*(-my);
      else sxy += (x[i]-mx)*(y[j]-my);
      /* or should it be:
      if (j < 0 || j >= xs) continue;
      else sxy += (x[i]-mx)*(y[j]-my);
      */
    }
    r[delay+max_lag] = sxy/xs;
  }

  return 0;
}

int cum_sum_double(int xs, const double* x, int rs, double* r)
{
  int i;

  r[0] = x[0];

  for (i=1;i<xs;i++) {
    r[i] = r[i-1]+x[i];
  }

  return 0;
}

int cum_sum_float(int xs, const float* x, int rs, float* r)
{
  int i;

  r[0] = x[0];

  for (i=1;i<xs;i++) {
    r[i] = r[i-1]+x[i];
  }

  return 0;
}
