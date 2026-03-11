#include "simple_blas.h"

#ifdef ACCELERATE
#include <Accelerate/Accelerate.h>
#else
#include <cblas.h>
#include "lapack.h"
#endif

int to_cblas_transpose(const morpheus_transpose_e trans) {
  switch (trans) {
  case morpheus_no_trans:
    return CblasNoTrans;
  case morpheus_trans:
    return CblasTrans;
  }
  return 0;
}

int to_cblas_layout(const morpheus_layout_e layout) {
  switch (layout) {
  case morpheus_row_major:
    return CblasRowMajor;
  case morpheus_col_major:
    return CblasColMajor;
  }
  return 0;
}

void morpheus_dscal(const int n, const double alpha, double *x) {
  const int inc = 1;
  cblas_dscal(n, alpha, x, inc);
}

void morpheus_dcopy(const int n, const double *x, double *y) {
  const int inc = 1;
  cblas_dcopy(n, x, inc, y, inc);
}

void morpheus_dcopy_scalar(const int n, const double alpha, double *x) {
  cblas_dcopy(n, &alpha, 0, x, 1);
}

void morpheus_dgemv(const morpheus_layout_e layout,
                    const morpheus_transpose_e trans,
                    const int nrows, const int ncols,
                    const double alpha, const double *A, const double *x,
                    const double beta, double *y) {
  const int inc = 1;
  const int lda = layout == morpheus_row_major ? ncols : nrows;
  cblas_dgemv(to_cblas_layout(layout), to_cblas_transpose(trans), nrows, ncols,
              alpha, A, lda, x, inc, beta, y, inc);
}

void morpheus_daxpy(const int n, const double alpha,
                    const double *x, double *y) {
  const int inc = 1;
  cblas_daxpy(n, alpha, x, inc, y, inc);
}

double morpheus_dnrm2(const int n, const double *x) {
  const int inc = 1;
  return cblas_dnrm2(n, x, inc);
}

double morpheus_ddot(const int n, const double *x, const double *y) {
  const int inc = 1;
  return cblas_ddot(n, x, inc, y, inc);
}

int morpheus_inverse(int n, double *x, int *pivot, double *workspace) {
  workspace = workspace+n;
  int lwork = n*n;

  int rc;
  dgetrf_(&n, &n, x, &n, pivot, &rc);
  if (rc != 0) {
    return rc;
  }
  dgetri_(&n, x, &n, pivot, workspace, &lwork, &rc);
  return rc;
}

void morpheus_dger(const morpheus_layout_e layout,
                   const int nrows,
                   const int ncols,
                   const double alpha,
                   const double *x,
                   const double *y,
                   double *a) {
  const int inc = 1;
  const int lda = layout == morpheus_row_major ? ncols : nrows;
  cblas_dger(to_cblas_layout(layout), nrows, ncols,
                             alpha, x, inc, y, inc, a, lda);
}

void morpheus_dgemm(const morpheus_layout_e layout,
                    const morpheus_transpose_e trans_a,
                    const morpheus_transpose_e trans_b,
                    const int m,
                    const int n,
                    const int k,
                    const double alpha,
                    const double *a,
                    const double *b,
                    const double beta,
                    double *c) {
  int lda, ldb, ldc;
  if (layout == morpheus_row_major) {
    lda = trans_a == morpheus_no_trans ? k : m;
    ldb = trans_b == morpheus_no_trans ? n : k;
    ldc = n;
  } else {
    lda = trans_a == morpheus_no_trans ? m : k;
    ldb = trans_b == morpheus_no_trans ? k : n;
    ldc = m;
  }
  cblas_dgemm(to_cblas_layout(layout),
              to_cblas_transpose(trans_a), to_cblas_transpose(trans_b),
              m, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
}

void morpheus_identity(const int n, double *a) {
  const int m = n*n;
  const double zero = 0;
  const double one = 1;
  cblas_dcopy(m, &zero, 0, a, 1);
  cblas_dcopy(n, &one, 0, a, n+1);
}
