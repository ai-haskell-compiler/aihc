/*! \file lapack.h
  \brief LAPACK functions declarations.
*/

#ifndef MORPHEUS_LAPACK_H
#define MORPHEUS_LAPACK_H

/*! \defgroup lapack lapack
  \brief LAPACK functions declarations.
  \{
*/

/*! LU decomoposition of a general matrix */
extern void dgetrf_(int* M, int *N, double* A, int* lda, int* IPIV, int* INFO);

/*! generate inverse of a matrix given its LU decomposition */
extern void dgetri_(int* N, double* A, int* lda, int* IPIV, double* WORK, int* lwork, int* INFO);

/*! \} */

#endif  /* MORPHEUS_LAPACK_H */
