/*! \file types.h
  \brief Common types definitions
*/

#ifndef MORHEPUS_TYPES_H
#define MORHEPUS_TYPES_H

#include "simple_blas.h"
#include "regularization.h"

/*! \defgroup types types
  \brief Common types definitions
  \{
*/

/*! \brief Learing Data structure */
typedef struct {
  const double *x;            /*!< (num_examples x num_features) matrix */
  const double *y;            /*!< (num_examples) vector */
  int num_features;           /*!< number of features including bias dimension */
  int num_examples;           /*!< number of examples */
  morpheus_layout_e layout;   /*!< layout of matrix x */
} morpheus_data_t;


/*! Type definition of pointer to cost function. */
typedef double (*morpheus_cost_f)(const morpheus_reg_t *reg,
                                  const morpheus_data_t *data,
                                  const double *theta,
                                  double *tmp_buffer);


/*! Type definition of pointer to gradient function. */
typedef void (*morpheus_gradient_f)(const morpheus_reg_t *reg,
                                    const morpheus_data_t *data,
                                    const double *theta,
                                    double *grad,
                                    double *tmp_buffer);


typedef double (*morpheus_f)(const double *x, void *params);

typedef void (*morpheus_df)(const double *x, void *params, double *grad);


typedef struct {
  morpheus_data_t data;
  morpheus_reg_t reg;
  double *memory_buffer;
} morpheus_params_t;

/*! \} */

#endif /* MORHEPUS_TYPES_H */
