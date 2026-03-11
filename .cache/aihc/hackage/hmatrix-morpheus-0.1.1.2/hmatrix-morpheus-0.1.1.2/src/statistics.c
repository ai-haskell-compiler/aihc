#include "statistics.h"
#include "matrix_reduce.h"

#include <math.h>
#include <stdio.h>
#include <assert.h>

double morpheus_mean(int n, const double *x) {
  double m = 0;
  for (int i = 0; i < n; ++i) {
    m += x[i];
  }
  return m/n;
}

double morpheus_stddev_m(double mean, int n, const double *x) {
  if (n < 2) return 0;

  double s = 0;
  for (int i = 0; i < n; ++i) {
    double v = x[i] - mean;
    s += v*v;
  }
  return sqrt(s/(n-1));
}

double morpheus_stddev(int n, const double *x) {
  if (n < 2) return 0;

  double m = morpheus_mean(n, x);
  return morpheus_stddev_m(m, n, x);
}

void morpheus_column_mean(morpheus_layout_e layout,
                          int nrows, int ncols,
                          const double *x,
                          double *means) {
  morpheus_column_sum(layout, nrows, ncols, x, means);
  for (int i = 0; i < ncols; ++i) {
    means[i] /= nrows;
  }
}

void morpheus_row_mean(morpheus_layout_e layout,
                       int nrows, int ncols,
                       const double *x,
                       double *means) {
  morpheus_row_sum(layout, nrows, ncols, x, means);
  for (int i = 0; i < nrows; ++i) {
    means[i] /= ncols;
  }
}

void morpheus_column_stddev_m(morpheus_layout_e layout,
                              int nrows, int ncols,
                              const double *means,
                              const double *x,
                              double *s) {
  if (layout == morpheus_row_major) {
    for (int j = 0; j < ncols; ++j) {
      s[j] = 0;
    }
    for (int i = 0; i < nrows; ++i) {
      const double *row = x + i*ncols;
      for (int j = 0; j < ncols; ++j) {
        double v = row[j] - means[j];
        s[j] += v*v;
      }
    }
  } else if (layout == morpheus_col_major) {
    for (int j = 0; j < ncols; ++j) {
      const double *column = &x[j*nrows];
      s[j] = 0;
      for (int i = 0; i < nrows; ++i) {
        double v = column[i] - means[j];
        s[j] += v*v;
      }
    }
  } else {
    perror("unkwnown matrix layout");
    assert(0);
  }
  for (int j = 0; j < ncols; ++j) {
    s[j] = sqrt(s[j]/(nrows-1));
  }
}

void morpheus_row_stddev_m(morpheus_layout_e layout,
                           int nrows, int ncols,
                           const double *means,
                           const double *x,
                           double *s) {
  if (layout == morpheus_row_major) {
    for (int i = 0; i < nrows; ++i) {
      const double *row = x + i*ncols;
      s[i] = 0;
      for (int j = 0; j < ncols; ++j) {
        double v = row[j] - means[i];
        s[i] += v*v;
      }
    }
  } else if (layout == morpheus_col_major) {
    for (int i = 0; i < nrows; ++i) {
      s[i] = 0;
    }
    for (int j = 0; j < ncols; ++j) {
      const double *column = x + j*nrows;
      for (int i = 0; i < nrows; ++i) {
        double v = column[i] - means[i];
        s[i] += v*v;
      }
    }
  } else {
    perror("unkwnown matrix layout");
    assert(0);
  }
  for (int i = 0; i < nrows; ++i) {
    s[i] = sqrt(s[i]/(ncols-1));
  }
}
