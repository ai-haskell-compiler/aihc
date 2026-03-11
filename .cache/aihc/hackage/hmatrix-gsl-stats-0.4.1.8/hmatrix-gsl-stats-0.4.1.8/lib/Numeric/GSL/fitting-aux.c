#include <gsl/gsl_fit.h>
#include <gsl/gsl_multifit.h>

#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

int linear(double* c0, double* c1, double* chi_sq,
	   double* cov00, double* cov01, double* cov11,
	   int xs, const double* x, int ys, const double* y)
{
  if (xs != ys) return 2000; //BAD_SIZE
  
  return gsl_fit_linear(x,1,y,1,xs,c0,c1,cov00,cov01,cov11,chi_sq);
}

int linear_weighted(double* c0, double* c1, double* chi_sq,
		    double* cov00, double* cov01, double* cov11,
		    int xs, const double* x, 
		    int ws, const double * w, 
		    int ys, const double* y)
{
  if (xs != ys || xs != ws) return 2000; //BAD_SIZE
  
  return gsl_fit_wlinear(x,1,w,1,y,1,xs,c0,c1,cov00,cov01,cov11,chi_sq);
}


int linear_estimate(double x, double c0, double c1, 
		    double cov00, double cov01, double cov11,
		    double* y, double* e)
{
  return gsl_fit_linear_est(x,c0,c1,cov00,cov01,cov11,y,e);
}

int multifit(double* chi_sq,
	     int xrs, int xcs, const double* x,
	     int ys, const double* y,
	     int cs, double* c,
	     int covrs, int covcs, double* cov)
{
  gsl_multifit_linear_workspace* wsp = gsl_multifit_linear_alloc(xrs,xcs);

  gsl_matrix_const_view X = gsl_matrix_const_view_array(x,xrs,xcs);
  gsl_vector_const_view Y = gsl_vector_const_view_array(y,ys);
  gsl_vector_view C = gsl_vector_view_array(c,cs);
  gsl_matrix_view V = gsl_matrix_view_array(cov,covrs,covcs);

  int err = gsl_multifit_linear(&X.matrix,&Y.vector,&C.vector,&V.matrix,chi_sq,wsp);

  gsl_multifit_linear_free(wsp);

  return err;
}

int multifit_weighted(double* chi_sq,
		      int xrs, int xcs, const double* x,
		      int ws, const double* w,
		      int ys, const double* y,
		      int cs, double* c,
		      int covrs, int covcs, double* cov)
{
  gsl_multifit_linear_workspace* wsp = gsl_multifit_linear_alloc(xrs,xcs);

  gsl_matrix_const_view X = gsl_matrix_const_view_array(x,xrs,xcs);
  gsl_vector_const_view W = gsl_vector_const_view_array(w,ws);
  gsl_vector_const_view Y = gsl_vector_const_view_array(y,ys);
  gsl_vector_view C = gsl_vector_view_array(c,cs);
  gsl_matrix_view V = gsl_matrix_view_array(cov,covrs,covcs);

  int err = gsl_multifit_wlinear(&X.matrix,&W.vector,&Y.vector,&C.vector,&V.matrix,chi_sq,wsp);

  gsl_multifit_linear_free(wsp);

  return err;
}

int multifit_estimate(double* y, double* e,
		      int xs, const double* x,
		      int cs, double* c,
		      int covrs, int covcs, double* cov)
{
  gsl_vector_const_view X = gsl_vector_const_view_array(x,xs);
  gsl_vector_const_view C = gsl_vector_const_view_array(c,cs);
  gsl_matrix_const_view V = gsl_matrix_const_view_array(cov,covrs,covcs);

  return gsl_multifit_linear_est(&X.vector,&C.vector,&V.matrix,y,e);
}
