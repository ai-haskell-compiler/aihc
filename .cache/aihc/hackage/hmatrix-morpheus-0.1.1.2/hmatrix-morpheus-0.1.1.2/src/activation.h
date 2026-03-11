/*! \file activation.h
  \brief Activation functions
*/

#ifndef MORHEPUS_ACTIVATION_H
#define MORHEPUS_ACTIVATION_H


/*! \defgroup activation activation
  \brief Activation functions
  \{
*/

/*! Calculates sigmoid. */
void morpheus_sigmoid(int n, const double *x, double *y);

/*! Calculates derivatives of sigmoid. */
void morpheus_sigmoid_gradient(int n, const double *x, double *y);

/*! Calculates ReLu. */
void morpheus_relu(int n, const double *x, double *y);

/*! Calculates derivatives of ReLu. */
void morpheus_relu_gradient(int n, const double *x, double *y);

/*! Calculates tanh. */
void morpheus_tanh(int n, const double *x, double *y);

/*! Calculates derivatives of tanh. */
void morpheus_tanh_gradient(int n, const double *x, double *y);
/*! \} */

#endif  /* MORHEPUS_ACTIVATION_H */
