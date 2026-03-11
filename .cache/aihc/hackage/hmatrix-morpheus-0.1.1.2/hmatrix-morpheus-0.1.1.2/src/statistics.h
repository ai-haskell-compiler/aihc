/*! \file statistics.h
  \brief Statistics functions
*/

#ifndef MORHEPUS_STATISTICS_H
#define MORHEPUS_STATISTICS_H

#include "simple_blas.h"

/*! \defgroup statistics statistics
  \brief Statistics functions
  \{
*/

/*! Calculates mean (average). */
double morpheus_mean(int n, const double *x);

/*! Calculates sample standard deviation using given mean value. */
double morpheus_stddev_m(double mean, int n, const double *x);

/*! Calculates sample standard deviation.*/
double morpheus_stddev(int n, const double *x);

/*! Calculates mean (average) for every column. */
void morpheus_column_mean(morpheus_layout_e layout,
                          int nrows, int ncols,
                          const double *x,       /*!< matrix of nrows x ncols */
                          double *means          /*!< output vector of mean values of size ncols */
                          );

/*! Calculates mean (average) for every row. */
void morpheus_row_mean(morpheus_layout_e layout,
                       int nrows, int ncols,
                       const double *x,          /*!< matrix of nrows x ncols */
                       double *means             /*!< output vector of mean values of size nrows */
                       );

/*! Calculates sample standard deviation using given mean value for every column. */
void morpheus_column_stddev_m(morpheus_layout_e layout,
                              int nrows, int ncols,
                              const double *means,  /*!< vector of mean values of size ncols */
                              const double *x,      /*!< matrix of nrows x ncols */
                              double *s             /*!< output vector of stddev values of size ncols */
                              );

/*! Calculates sample standard deviation using given mean value for every row. */
void morpheus_row_stddev_m(morpheus_layout_e layout,
                           int nrows, int ncols,
                           const double *means,     /*!< vector of mean values of size nrows */
                           const double *x,         /*!< matrix of nrows x nrows */
                           double *s                /*!< output vector of stddev values of size nrows */
                           );
/*! \} */

#endif  /* MORHEPUS_STATISTICS_H */
