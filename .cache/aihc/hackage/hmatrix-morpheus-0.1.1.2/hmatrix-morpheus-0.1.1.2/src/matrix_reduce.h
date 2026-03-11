/*! \file matrix_reduce.h
  \brief Matrix reduce by column/row operations.
*/

#ifndef MORPHEPUS_MATRIX_REDUCE_H
#define MORPHEPUS_MATRIX_REDUCE_H

#include "simple_blas.h"

/*! \defgroup matrix_reduce matrix_reduce
  \brief Matrix reduce by column/row operations.
  \{
*/

/*! Scan every column of the matrix x.
  Pred takes and accumulator and next value of the column, returns new accumulator.
  Returns accumulator values for every column.
 */
void morpheus_column_predicate(double (*pred)(double, double),
                               morpheus_layout_e layout,
                               int nrows,
                               int ncols,
                               const double *x,
                               double *y                  /*!< vector of size ncols */);

/*! Scan every row of the matrix x.
 Pred takes and accumulator and next value of the row, returns new accumulator.
 Returns accumulator values for every row.
*/
void morpheus_row_predicate(double (*pred)(double, double),
                            morpheus_layout_e layout,
                            int nrows, int ncols,
                            const double *x,
                            double *y             /*!< vector of size ncols */);


/*! Finds maximum values and their indices of every column of the matrix x */
void morpheus_column_max_index(morpheus_layout_e layout,
                               int nrows, int ncols,
                               const double *x,
                               double *y,     /*!< vector of maximum values of size ncols */
                               int *is        /*!< vector of indices of size ncols */);

/*! Finds minimum values and their indices of every column of the matrix x */
void morpheus_column_min_index(morpheus_layout_e layout,
                               int nrows, int ncols,
                               const double *x,
                               double *y,     /*!< vector of minimum values of size ncols */
                               int *is        /*!< vector of indices of size ncols */);

/*! Finds maximum values and their indices of every row of the matrix x */
void morpheus_row_max_index(morpheus_layout_e layout,
                            int nrows, int ncols,
                            const double *x,
                            double *y,        /*!< vector of maximum values of size nrows */
                            int *is           /*!< vector of indices of size nrows */);

/*! Finds minimum values and their indices of every row of the matrix x */
void morpheus_row_min_index(morpheus_layout_e layout,
                            int nrows, int ncols,
                            const double *x,
                            double *y,        /*!< vector of minimum values of size nrows */
                            int *is           /*!< vector of indices of size nrows */);

/*! Calculates sums of elements of every column of the matrix x */
void morpheus_column_sum(morpheus_layout_e layout,
                         int nrows, int ncols,
                         const double *x,
                         double *y           /*!< vector of sums of size ncols */);

/*! Calculates sums of elements of every row of the matrix x */
void morpheus_row_sum(morpheus_layout_e layout,
                      int nrows, int ncols,
                      const double *x,
                      double *y              /*!< vector of sums of size nrows */);
/*! \} */

#endif /* MORPHEPUS_MATRIX_REDUCE_H */
