#include "matrix_reduce.h"

#include <math.h>
#include <stdio.h>
#include <assert.h>

void morpheus_column_predicate(double (*pred)(double, double),
                               morpheus_layout_e layout,
                               int nrows,
                               int ncols,
                               const double *x,
                               double *y) {
  if (layout == morpheus_row_major) {
    /* first row is initial values */
    morpheus_dcopy(ncols, x, y);
    for (int i = 1; i < nrows; ++i) {
      const double *row = x + i*ncols;
      for (int j = 0; j < ncols; ++j) {
        y[j] = pred(y[j], row[j]);
      }
    }
  } else if (layout == morpheus_col_major) {
    for (int j = 0; j < ncols; ++j) {
      const double *column = &x[j*nrows];
      y[j] = column[0];
      for (int i = 1; i < nrows; ++i) {
        y[j] = pred(y[j], column[i]);
      }
    }
  } else {
    perror("unkwnown matrix layout");
    assert(0);
  }
}

void morpheus_row_predicate(double (*pred)(double, double),
                            morpheus_layout_e layout,
                            int nrows, int ncols,
                            const double *x,
                            double *y) {
  if (layout == morpheus_row_major) {
    for (int i = 0; i < nrows; ++i) {
      const double *row = x + i*ncols;
      y[i] = row[0];
      for (int j = 1; j < ncols; ++j) {
        y[i] = pred(y[i], row[j]);
      }
    }
  } else if (layout == morpheus_col_major) {
    /* first column is initial values */
    morpheus_dcopy(nrows, x, y);
    for (int j = 1; j < ncols; ++j) {
      const double *column = x + j*nrows;
      for (int i = 0; i < nrows; ++i) {
        y[i] = pred(y[i], column[i]);
      }
    }
  } else {
    perror("unkwnown matrix layout");
    assert(0);
  }
}

void morpheus_column_max_index(morpheus_layout_e layout,
                               int nrows, int ncols,
                               const double *x,
                               double *y,
                               int *is) {
  if (layout == morpheus_row_major) {
    for (int j = 0; j < ncols; ++j) {
      is[j] = 0;
      y[j] = x[j];
    }
    for (int i = 1; i < nrows; ++i) {
      const double *row = x + i*ncols;
      for (int j = 0; j < ncols; ++j) {
        if (y[j] < row[j]) {
          y[j] = row[j];
          is[j] = i;
        }
      }
    }
  } else if (layout == morpheus_col_major) {
    for (int j = 0; j < ncols; ++j) {
      const double *column = &x[j*nrows];
      y[j] = column[0];
      is[j] = 0;
      for (int i = 1; i < nrows; ++i) {
        if (y[j] < column[i]) {
          y[j] = column[i];
          is[j] = i;
        }
      }
    }
  } else {
    perror("unkwnown matrix layout");
    assert(0);
  }
}


void morpheus_column_min_index(morpheus_layout_e layout,
                               int nrows, int ncols,
                               const double *x,
                               double *y,
                               int *is) {
  if (layout == morpheus_row_major) {
    for (int j = 0; j < ncols; ++j) {
      is[j] = 0;
      y[j] = x[j];
    }
    for (int i = 1; i < nrows; ++i) {
      const double *row = x + i*ncols;
      for (int j = 0; j < ncols; ++j) {
        if (y[j] > row[j]) {
          y[j] = row[j];
          is[j] = i;
        }
      }
    }
  } else if (layout == morpheus_col_major) {
    for (int j = 0; j < ncols; ++j) {
      const double *column = &x[j*nrows];
      y[j] = column[0];
      is[j] = 0;
      for (int i = 1; i < nrows; ++i) {
        if (y[j] > column[i]) {
          y[j] = column[i];
          is[j] = i;
        }
      }
    }
  } else {
    perror("unkwnown matrix layout");
    assert(0);
  }
}

void morpheus_row_max_index(morpheus_layout_e layout,
                            int nrows, int ncols,
                            const double *x,
                            double *y,
                            int *is) {
  if (layout == morpheus_row_major) {
    for (int i = 0; i < nrows; ++i) {
      const double *row = x + i*ncols;
      y[i] = row[0];
      is[i] = 0;
      for (int j = 1; j < ncols; ++j) {
        if (y[i] < row[j]) {
          y[i] = row[j];
          is[i] = j;
        }
      }
    }
  } else if (layout == morpheus_col_major) {
    for (int i = 0; i < nrows; ++i) {
      is[i] = 0;
      y[i] = x[i];
    }
    for (int j = 1; j < ncols; ++j) {
      const double *column = x + j*nrows;
      for (int i = 0; i < nrows; ++i) {
        if (y[i] < column[i]) {
          y[i] = column[i];
          is[i] = j;
        }
      }
    }
  } else {
    perror("unkwnown matrix layout");
    assert(0);
  }
}

void morpheus_row_min_index(morpheus_layout_e layout,
                            int nrows, int ncols,
                            const double *x,
                            double *y,
                            int *is) {
  if (layout == morpheus_row_major) {
    for (int i = 0; i < nrows; ++i) {
      const double *row = x + i*ncols;
      y[i] = row[0];
      is[i] = 0;
      for (int j = 1; j < ncols; ++j) {
        if (y[i] > row[j]) {
          y[i] = row[j];
          is[i] = j;
        }
      }
    }
  } else if (layout == morpheus_col_major) {
    for (int i = 0; i < nrows; ++i) {
      is[i] = 0;
      y[i] = x[i];
    }
    for (int j = 1; j < ncols; ++j) {
      const double *column = x + j*nrows;
      for (int i = 0; i < nrows; ++i) {
        if (y[i] > column[i]) {
          y[i] = column[i];
          is[i] = j;
        }
      }
    }
  } else {
    perror("unkwnown matrix layout");
    assert(0);
  }
}

void morpheus_column_sum(morpheus_layout_e layout,
                         int nrows, int ncols,
                         const double *x,
                         double *y) {
  if (layout == morpheus_row_major) {
    for (int j = 0; j < ncols; ++j) {
      y[j] = 0;
    }
    for (int i = 0; i < nrows; ++i) {
      const double *row = x + i*ncols;
      for (int j = 0; j < ncols; ++j) {
        y[j] += row[j];
      }
    }
  } else if (layout == morpheus_col_major) {
    for (int j = 0; j < ncols; ++j) {
      const double *column = &x[j*nrows];
      y[j] = 0;
      for (int i = 0; i < nrows; ++i) {
        y[j] += column[i];
      }
    }
  } else {
    perror("unkwnown matrix layout");
    assert(0);
  }
}

void morpheus_row_sum(morpheus_layout_e layout,
                      int nrows, int ncols,
                      const double *x,
                      double *y) {
  if (layout == morpheus_row_major) {
    for (int i = 0; i < nrows; ++i) {
      const double *row = x + i*ncols;
      y[i] = 0;
      for (int j = 0; j < ncols; ++j) {
        y[i] += row[j];
      }
    }
  } else if (layout == morpheus_col_major) {
    for (int i = 0; i < nrows; ++i) {
      y[i] = 0;
    }
    for (int j = 0; j < ncols; ++j) {
      const double *column = x + j*nrows;
      for (int i = 0; i < nrows; ++i) {
        y[i] += column[i];
      }
    }
  } else {
    perror("unkwnown matrix layout");
    assert(0);
  }
}
