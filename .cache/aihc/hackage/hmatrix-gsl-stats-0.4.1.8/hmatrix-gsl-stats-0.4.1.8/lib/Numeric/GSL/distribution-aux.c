#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>

gsl_rng* new_rng()
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);

  return rng;
}

int random0(int s, int type, double* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  switch(type) {
  case 0: { (*r) = gsl_ran_landau(rng); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

int random0_s(gsl_rng* rng, int type, double* r)
{
  switch(type) {
  case 0: { (*r) = gsl_ran_landau(rng); break; }
  }
  return 0;
}

int random0_v(int s, int type, int rs, double* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  int i;

  switch(type) {
  case 0: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_landau(rng); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

double random0_pdf(int type, double x)
{
  switch (type) {
  case 0: return gsl_ran_landau_pdf(x);
  }
}

double random0_dist(int df, int type, double x)
{
  switch(df) {
  case 0: return random0_pdf(type,x);
  }
}

//////////////////////////////////////////////////////////////////////

int random1(int s, int type, double par, double* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  switch(type) {
  case 0: { (*r) = gsl_ran_gaussian(rng,par); break; }
  case 1: { (*r) = gsl_ran_exponential(rng,par); break; }
  case 2: { (*r) = gsl_ran_laplace(rng,par); break; }
  case 3: { (*r) = gsl_ran_cauchy(rng,par); break; }
  case 4: { (*r) = gsl_ran_rayleigh(rng,par); break; }
  case 5: { (*r) = gsl_ran_chisq(rng,par); break; }
  case 6: { (*r) = gsl_ran_tdist(rng,par); break; }
  case 7: { (*r) = gsl_ran_logistic(rng,par); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

int random1_s(gsl_rng* rng, int type, double par, double* r)
{
  switch(type) {
  case 0: { (*r) = gsl_ran_gaussian(rng,par); break; }
  case 1: { (*r) = gsl_ran_exponential(rng,par); break; }
  case 2: { (*r) = gsl_ran_laplace(rng,par); break; }
  case 3: { (*r) = gsl_ran_cauchy(rng,par); break; }
  case 4: { (*r) = gsl_ran_rayleigh(rng,par); break; }
  case 5: { (*r) = gsl_ran_chisq(rng,par); break; }
  case 6: { (*r) = gsl_ran_tdist(rng,par); break; }
  case 7: { (*r) = gsl_ran_logistic(rng,par); break; }
  }
  return 0;
}

int random1_v(int s, int type, double par, int rs, double* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  int i;

  switch(type) {
  case 0: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_gaussian(rng,par); break; }
  case 1: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_exponential(rng,par); break; }
  case 2: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_laplace(rng,par); break; }
  case 3: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_cauchy(rng,par); break; }
  case 4: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_rayleigh(rng,par); break; }
  case 5: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_chisq(rng,par); break; }
  case 6: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_tdist(rng,par); break; }
  case 7: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_logistic(rng,par); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

double random1_pdf(int type, double x, double par)
{
  switch (type) {
  case 0: return gsl_ran_gaussian_pdf(x,par);
  case 1: return gsl_ran_exponential_pdf(x,par);
  case 2: return gsl_ran_laplace_pdf(x,par);
  case 3: return gsl_ran_cauchy_pdf(x,par);
  case 4: return gsl_ran_rayleigh_pdf(x,par);
  case 5: return gsl_ran_chisq_pdf(x,par);
  case 6: return gsl_ran_tdist_pdf(x,par);
  case 7: return gsl_ran_logistic_pdf(x,par);
  }
}

double random1_cdf_lower(int type, double x, double par)
{
  switch (type) {
  case 0: return gsl_cdf_gaussian_P(x,par);
  case 1: return gsl_cdf_exponential_P(x,par);
  case 2: return gsl_cdf_laplace_P(x,par);
  case 3: return gsl_cdf_cauchy_P(x,par);
  case 4: return gsl_cdf_rayleigh_P(x,par);
  case 5: return gsl_cdf_chisq_P(x,par);
  case 6: return gsl_cdf_tdist_P(x,par);
  case 7: return gsl_cdf_logistic_P(x,par);
  }
}

double random1_cdf_upper(int type, double x, double par)
{
  switch (type) {
  case 0: return gsl_cdf_gaussian_Q(x,par);
  case 1: return gsl_cdf_exponential_Q(x,par);
  case 2: return gsl_cdf_laplace_Q(x,par);
  case 3: return gsl_cdf_cauchy_Q(x,par);
  case 4: return gsl_cdf_rayleigh_Q(x,par);
  case 5: return gsl_cdf_chisq_Q(x,par);
  case 6: return gsl_cdf_tdist_Q(x,par);
  case 7: return gsl_cdf_logistic_Q(x,par);
  }
}

double random1_cdf_lower_inv(int type, double X, double par)
{
  switch (type) {
  case 0: return gsl_cdf_gaussian_Pinv(X,par);
  case 1: return gsl_cdf_exponential_Pinv(X,par);
  case 2: return gsl_cdf_laplace_Pinv(X,par);
  case 3: return gsl_cdf_cauchy_Pinv(X,par);
  case 4: return gsl_cdf_rayleigh_Pinv(X,par);
  case 5: return gsl_cdf_chisq_Pinv(X,par);
  case 6: return gsl_cdf_tdist_Pinv(X,par);
  case 7: return gsl_cdf_logistic_Pinv(X,par);
  }
}

double random1_cdf_upper_inv(int type, double X, double par)
{
  switch (type) {
  case 0: return gsl_cdf_gaussian_Qinv(X,par);
  case 1: return gsl_cdf_exponential_Qinv(X,par);
  case 2: return gsl_cdf_laplace_Qinv(X,par);
  case 3: return gsl_cdf_cauchy_Qinv(X,par);
  case 4: return gsl_cdf_rayleigh_Qinv(X,par);
  case 5: return gsl_cdf_chisq_Qinv(X,par);
  case 6: return gsl_cdf_tdist_Qinv(X,par);
  case 7: return gsl_cdf_logistic_Qinv(X,par);
  }
}

double random1_dist(int df, int type, double x, double par)
{
  switch(df) {
  case 0: return random1_pdf(type,x,par);
  case 1: return random1_cdf_lower(type,x,par);
  case 2: return random1_cdf_upper(type,x,par);
  case 3: return random1_cdf_lower_inv(type,x,par);
  case 4: return random1_cdf_upper_inv(type,x,par);
  }
}

//////////////////////////////////////////////////////////////////////

int random2(int s, int type, double par1, double par2, double* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  switch(type) {
  case 0: { (*r) = gsl_ran_gaussian_tail(rng,par1,par2); break; }
  case 1: { (*r) = gsl_ran_exppow(rng,par1,par2); break; }
  case 2: { (*r) = gsl_ran_rayleigh_tail(rng,par1,par2); break; }
  case 3: { (*r) = gsl_ran_levy(rng,par1,par2); break; }
  case 4: { (*r) = gsl_ran_gamma(rng,par1,par2); break; }
  case 5: { (*r) = gsl_ran_flat(rng,par1,par2); break; }
  case 6: { (*r) = gsl_ran_lognormal(rng,par1,par2); break; }
  case 7: { (*r) = gsl_ran_fdist(rng,par1,par2); break; }
  case 8: { (*r) = gsl_ran_beta(rng,par1,par2); break; }
  case 9: { (*r) = gsl_ran_pareto(rng,par1,par2); break; }
  case 10: { (*r) = gsl_ran_weibull(rng,par1,par2); break; }
  case 11: { (*r) = gsl_ran_gumbel1(rng,par1,par2); break; }
  case 12: { (*r) = gsl_ran_gumbel2(rng,par1,par2); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

int random2_s(gsl_rng* rng, int type, double par1, double par2, double* r)
{
  switch(type) {
  case 0: { (*r) = gsl_ran_gaussian_tail(rng,par1,par2); break; }
  case 1: { (*r) = gsl_ran_exppow(rng,par1,par2); break; }
  case 2: { (*r) = gsl_ran_rayleigh_tail(rng,par1,par2); break; }
  case 3: { (*r) = gsl_ran_levy(rng,par1,par2); break; }
  case 4: { (*r) = gsl_ran_gamma(rng,par1,par2); break; }
  case 5: { (*r) = gsl_ran_flat(rng,par1,par2); break; }
  case 6: { (*r) = gsl_ran_lognormal(rng,par1,par2); break; }
  case 7: { (*r) = gsl_ran_fdist(rng,par1,par2); break; }
  case 8: { (*r) = gsl_ran_beta(rng,par1,par2); break; }
  case 9: { (*r) = gsl_ran_pareto(rng,par1,par2); break; }
  case 10: { (*r) = gsl_ran_weibull(rng,par1,par2); break; }
  case 11: { (*r) = gsl_ran_gumbel1(rng,par1,par2); break; }
  case 12: { (*r) = gsl_ran_gumbel2(rng,par1,par2); break; }
  }
  return 0;
}

int random2_v(int s, int type, double par1, double par2, int rs, double* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  int i;

  switch(type) {
  case 0: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_gaussian_tail(rng,par1,par2); break; }
  case 1: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_exppow(rng,par1,par2); break; }
  case 2: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_rayleigh_tail(rng,par1,par2); break; }
  case 3: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_levy(rng,par1,par2); break; }
  case 4: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_gamma(rng,par1,par2); break; }
  case 5: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_flat(rng,par1,par2); break; }
  case 6: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_lognormal(rng,par1,par2); break; }
  case 7: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_fdist(rng,par1,par2); break; }
  case 8: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_beta(rng,par1,par2); break; }
  case 9: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_pareto(rng,par1,par2); break; }
  case 10: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_weibull(rng,par1,par2); break; }
  case 11: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_gumbel1(rng,par1,par2); break; }
  case 12: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_gumbel2(rng,par1,par2); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

double random2_pdf(int type, double x, double par1, double par2)
{
  switch (type) {
  case 0: return gsl_ran_gaussian_tail_pdf(x,par1,par2);
  case 1: return gsl_ran_exppow_pdf(x,par1,par2);
  case 2: return gsl_ran_rayleigh_tail_pdf(x,par1,par2);
  case 4: return gsl_ran_gamma_pdf(x,par1,par2);
  case 5: return gsl_ran_flat_pdf(x,par1,par2);
  case 6: return gsl_ran_lognormal_pdf(x,par1,par2);
  case 7: return gsl_ran_fdist_pdf(x,par1,par2);
  case 8: return gsl_ran_beta_pdf(x,par1,par2);
  case 9: return gsl_ran_pareto_pdf(x,par1,par2);
  case 10: return gsl_ran_weibull_pdf(x,par1,par2);
  case 11: return gsl_ran_gumbel1_pdf(x,par1,par2);
  case 12: return gsl_ran_gumbel2_pdf(x,par1,par2);
  }
}

double random2_cdf_lower(int type, double x, double par1, double par2)
{
  switch (type) {
  case 1: return gsl_cdf_exppow_P(x,par1,par2);
  case 4: return gsl_cdf_gamma_P(x,par1,par2);
  case 5: return gsl_cdf_flat_P(x,par1,par2);
  case 6: return gsl_cdf_lognormal_P(x,par1,par2);
  case 7: return gsl_cdf_fdist_P(x,par1,par2);
  case 8: return gsl_cdf_beta_P(x,par1,par2);
  case 9: return gsl_cdf_pareto_P(x,par1,par2);
  case 10: return gsl_cdf_weibull_P(x,par1,par2);
  case 11: return gsl_cdf_gumbel1_P(x,par1,par2);
  case 12: return gsl_cdf_gumbel2_P(x,par1,par2);
  }
}

double random2_cdf_upper(int type, double x, double par1, double par2)
{
  switch (type) {
  case 1: return gsl_cdf_exppow_Q(x,par1,par2);
  case 4: return gsl_cdf_gamma_Q(x,par1,par2);
  case 5: return gsl_cdf_flat_Q(x,par1,par2);
  case 6: return gsl_cdf_lognormal_Q(x,par1,par2);
  case 7: return gsl_cdf_fdist_Q(x,par1,par2);
  case 8: return gsl_cdf_beta_Q(x,par1,par2);
  case 9: return gsl_cdf_pareto_Q(x,par1,par2);
  case 10: return gsl_cdf_weibull_Q(x,par1,par2);
  case 11: return gsl_cdf_gumbel1_Q(x,par1,par2);
  case 12: return gsl_cdf_gumbel2_Q(x,par1,par2);
  }
}

double random2_cdf_lower_inv(int type, double X, double par1, double par2)
{
  switch (type) {
  case 4: return gsl_cdf_gamma_Pinv(X,par1,par2);
  case 5: return gsl_cdf_flat_Pinv(X,par1,par2);
  case 6: return gsl_cdf_lognormal_Pinv(X,par1,par2);
  case 7: return gsl_cdf_fdist_Pinv(X,par1,par2);
  case 8: return gsl_cdf_beta_Pinv(X,par1,par2);
  case 9: return gsl_cdf_pareto_Pinv(X,par1,par2);
  case 10: return gsl_cdf_weibull_Pinv(X,par1,par2);
  case 11: return gsl_cdf_gumbel1_Pinv(X,par1,par2);
  case 12: return gsl_cdf_gumbel2_Pinv(X,par1,par2);
  }
}

double random2_cdf_upper_inv(int type, double X, double par1, double par2)
{
  switch (type) {
  case 4: return gsl_cdf_gamma_Qinv(X,par1,par2);
  case 5: return gsl_cdf_flat_Qinv(X,par1,par2);
  case 6: return gsl_cdf_lognormal_Qinv(X,par1,par2);
  case 7: return gsl_cdf_fdist_Qinv(X,par1,par2);
  case 8: return gsl_cdf_beta_Qinv(X,par1,par2);
  case 9: return gsl_cdf_pareto_Qinv(X,par1,par2);
  case 10: return gsl_cdf_weibull_Qinv(X,par1,par2);
  case 11: return gsl_cdf_gumbel1_Qinv(X,par1,par2);
  case 12: return gsl_cdf_gumbel2_Qinv(X,par1,par2);
  }
}

double random2_dist(int df, int type, double x, double par1, double par2)
{
  switch(df) {
  case 0: return random2_pdf(type,x,par1,par2);
  case 1: return random2_cdf_lower(type,x,par1,par2);
  case 2: return random2_cdf_upper(type,x,par1,par2);
  case 3: return random2_cdf_lower_inv(type,x,par1,par2);
  case 4: return random2_cdf_upper_inv(type,x,par1,par2);
  }
}

//////////////////////////////////////////////////////////////////////

int random3(int s, int type, double par1, double par2, double par3, double* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  switch(type) {
  case 0: { (*r) = gsl_ran_levy_skew(rng,par1,par2,par3); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

int random3_s(gsl_rng* rng, int type, double par1, double par2, double par3, double* r)
{
  switch(type) {
  case 0: { (*r) = gsl_ran_levy_skew(rng,par1,par2,par3); break; }
  }
  return 0;
}

int random3_v(int s, int type, double par1, double par2, double par3, int rs, double* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  int i;

  switch(type) {
  case 0: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_levy_skew(rng,par1,par2,par3); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

double random3_pdf(int type, double x, double par1, double par2, double par3)
{
  switch (type) {
  default : return 0;
  }
}

double random3_cdf_lower(int type, double x, double par1, double par2, double par3)
{
  switch (type) {
  default : return 0;
  }
}

double random3_cdf_upper(int type, double x, double par1, double par2, double par3)
{
  switch (type) {
  default : return 0;
  }
}

double random3_cdf_lower_inv(int type, double X, double par1, double par2, double par3)
{
  switch (type) {
  default : return 0;
  }
}

double random3_cdf_upper_inv(int type, double X, double par1, double par2, double par3)
{
  switch (type) {
  default : return 0;
  }
}

double random3_dist(int df, int type, double x, double par1, double par2, double par3)
{
  switch(df) {
  case 0: return random3_pdf(type,x,par1,par2,par3);
  case 1: return random3_cdf_lower(type,x,par1,par2,par3);
  case 2: return random3_cdf_upper(type,x,par1,par2,par3);
  case 3: return random3_cdf_lower_inv(type,x,par1,par2,par3);
  case 4: return random3_cdf_upper_inv(type,x,par1,par2,par3);
  }
}

//////////////////////////////////////////////////////////////////////

int random_biv(int s, int type, double par1, double par2, double par3, double* r1, double* r2)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  switch(type) {
  case 0: { gsl_ran_bivariate_gaussian(rng,par1,par2,par3,r1,r2); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

int random_biv_s(gsl_rng* rng, int type, double par1, double par2, double par3, double* r1, double* r2)
{
  switch(type) {
  case 0: { gsl_ran_bivariate_gaussian(rng,par1,par2,par3,r1,r2); break; }
  }
  return 0;
}

int random_biv_v(int s, int type, double par1, double par2, double par3, int rs1, double* r1, int rs2, double* r2)
{
  if (rs1 != rs2) return 2000; // BAD_SIZE

  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  int i;

  switch(type) {
  case 0: { for (i = 0; i < rs1; i++) gsl_ran_bivariate_gaussian(rng,par1,par2,par3,&r1[i],&r2[i]); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

double random_biv_pdf(int type, double x, double y, double par1, double par2, double par3)
{
  switch (type) {
  case 0: return gsl_ran_bivariate_gaussian_pdf(x,y,par1,par2,par3);
  }
}

double random_biv_cdf_lower(int type, double x, double y, double par1, double par2, double par3)
{
  switch (type) {
  default : return 0;
  }
}

double random_biv_cdf_upper(int type, double x, double y, double par1, double par2, double par3)
{
  switch (type) {
  default : return 0;
  }
}

double random_biv_cdf_lower_inv(int type, double x, double y, double par1, double par2, double par3)
{
  switch (type) {
  default : return 0;
  }
}

double random_biv_cdf_upper_inv(int type, double x, double y, double par1, double par2, double par3)
{
  switch (type) {
  default : return 0;
  }
}

double random_biv_dist(int df, int type, double x, double y, double par1, double par2, double par3)
{
  switch(df) {
  case 0: return random_biv_pdf(type,x,y,par1,par2,par3);
  case 1: return random_biv_cdf_lower(type,x,y,par1,par2,par3);
  case 2: return random_biv_cdf_upper(type,x,y,par1,par2,par3);
  case 3: return random_biv_cdf_lower_inv(type,x,y,par1,par2,par3);
  case 4: return random_biv_cdf_upper_inv(type,x,y,par1,par2,par3);
  }
}

//////////////////////////////////////////////////////////////////////

int random_mp(int s, int type, int ps, const double* p, int rs, double* r)
{
  if (ps != rs) return 2000; // BAD_SIZE

  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  switch(type) {
  case 0: { gsl_ran_dirichlet(rng,ps,p,r); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

int random_mp_s(gsl_rng* rng, int type, int ps, const double* p, int rs, double* r)
{
  switch(type) {
  case 0: { gsl_ran_dirichlet(rng,ps,p,r); break; }
  }
  return 0;
}

double random_mp_pdf(int type, int ps, const double* p, int qs, const double* q)
{
  switch (type) {
  case 0: return gsl_ran_dirichlet_pdf(ps,p,q);
  }
  return 0;
}

double random_mp_cdf_lower(int type, int ps, const double* p, int qs, const double* q)
{
  switch (type) {
  default : return 0;
  }
}

double random_mp_cdf_upper(int type, int ps, const double* p, int qs, const double* q)
{
  switch (type) {
  default : return 0;
  }
}

double random_mp_cdf_lower_inv(int type, int ps, const double* p, int qs, const double* q)
{
  switch (type) {
  default : return 0;
  }
}

double random_mp_cdf_upper_inv(int type, int ps, const double* p, int qs, const double* q)
{
  switch (type) {
  default : return 0;
  }
}

 int random_mp_dist(int df, int type, double* r, int ps, const double* p, int qs, const double* q)
{
  switch(df) {
  case 0: { (*r) = random_mp_pdf(type,ps,p,qs,q); break; }
  case 1: { (*r) = random_mp_cdf_lower(type,ps,p,qs,q); break; }
  case 2: { (*r) = random_mp_cdf_upper(type,ps,p,qs,q); break; }
  case 3: { (*r) = random_mp_cdf_lower_inv(type,ps,p,qs,q); break; }
  case 4: { (*r) = random_mp_cdf_upper_inv(type,ps,p,qs,q); break; }
  }
  return 0;
}

//////////////////////////////////////////////////////////////////////

int spherical_vector(int s, int rs, double* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  gsl_ran_dir_nd(rng,rs,r);

  gsl_rng_free(rng);

  return 0;
}

//////////////////////////////////////////////////////////////////////

int discrete1(int s, int type, double par, unsigned int* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  switch(type) {
  case 0: { (*r) = gsl_ran_poisson(rng,par); break; }
  case 1: { (*r) = gsl_ran_bernoulli(rng,par); break; }
  case 2: { (*r) = gsl_ran_laplace(rng,par); break; }
  case 3: { (*r) = gsl_ran_logarithmic(rng,par); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

int discrete1_s(gsl_rng* rng, int type, double par, unsigned int* r)
{
  switch(type) {
  case 0: { (*r) = gsl_ran_poisson(rng,par); break; }
  case 1: { (*r) = gsl_ran_bernoulli(rng,par); break; }
  case 2: { (*r) = gsl_ran_laplace(rng,par); break; }
  case 3: { (*r) = gsl_ran_logarithmic(rng,par); break; }
  }
  return 0;
}

int discrete1_v(int s, int type, double par, int rs, unsigned int* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  int i;

  switch(type) {
  case 0: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_poisson(rng,par); break; }
  case 1: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_bernoulli(rng,par); break; }
  case 2: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_laplace(rng,par); break; }
  case 3: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_logarithmic(rng,par); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

double discrete1_pdf(int type, unsigned int x, double par)
{
  switch (type) {
  case 0: return gsl_ran_poisson_pdf(x,par);
  case 1: return gsl_ran_bernoulli_pdf(x,par);
  case 2: return gsl_ran_geometric_pdf(x,par);
  case 3: return gsl_ran_logarithmic_pdf(x,par);
  }
}

double discrete1_cdf_lower(int type, unsigned int x, double par)
{
  switch (type) {
  case 0: return gsl_cdf_poisson_P(x,par);
  case 2: return gsl_cdf_geometric_P(x,par);
  }
}

double discrete1_cdf_upper(int type, unsigned int x, double par)
{
  switch (type) {
  case 0: return gsl_cdf_poisson_Q(x,par);
  case 2: return gsl_cdf_geometric_Q(x,par);
  }
}

double discrete1_cdf_lower_inv(int type, unsigned int X, double par)
{
  switch (type) {
  default : return 0;
  }
}

double discrete1_cdf_upper_inv(int type, unsigned int X, double par)
{
  switch (type) {
  default : return 0;
  }
}

double discrete1_dist(int df, int type, unsigned int x, double par)
{
  switch(df) {
  case 0: return discrete1_pdf(type,x,par);
  case 1: return discrete1_cdf_lower(type,x,par);
  case 2: return discrete1_cdf_upper(type,x,par);
  case 3: return discrete1_cdf_lower_inv(type,x,par);
  case 4: return discrete1_cdf_upper_inv(type,x,par);
  }
}

//////////////////////////////////////////////////////////////////////

int discrete2(int s, int type, double par1, unsigned int par2, unsigned int* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  switch(type) {
  case 0: { (*r) = gsl_ran_binomial(rng,par1,par2); break; }
  case 1: { (*r) = gsl_ran_negative_binomial(rng,par1,par2*1.0); break; }
  case 2: { (*r) = gsl_ran_pascal(rng,par1,par2); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

int discrete2_s(gsl_rng* rng, int type, double par1, unsigned int par2, unsigned int* r)
{
  switch(type) {
  case 0: { (*r) = gsl_ran_binomial(rng,par1,par2); break; }
  case 1: { (*r) = gsl_ran_negative_binomial(rng,par1,par2*1.0); break; }
  case 2: { (*r) = gsl_ran_pascal(rng,par1,par2); break; }
  }
  return 0;
}

int discrete2_v(int s, int type, double par1, unsigned int par2, int rs, unsigned int* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  int i;

  switch(type) {
  case 0: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_binomial(rng,par1,par2); break; }
  case 1: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_negative_binomial(rng,par1,par2*1.0); break; }
  case 2: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_pascal(rng,par1,par2); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

double discrete2_pdf(int type, unsigned int x, double par1, unsigned int par2)
{
  switch (type) {
  case 0: return gsl_ran_binomial_pdf(x,par1,par2);
  case 1: return gsl_ran_negative_binomial_pdf(x,par1,par2*1.0);
  case 2: return gsl_ran_pascal_pdf(x,par1,par2);
  }
}

double discrete2_cdf_lower(int type, unsigned int x, double par1, unsigned int par2)
{
  switch (type) {
  case 0: return gsl_cdf_binomial_P(x,par1,par2);
  case 1: return gsl_cdf_negative_binomial_P(x,par1,par2*1.0);
  case 2: return gsl_cdf_pascal_P(x,par1,par2*1.0);
  }
}

double discrete2_cdf_upper(int type, unsigned int x, double par1, unsigned int par2)
{
  switch (type) {
  case 0: return gsl_cdf_binomial_Q(x,par1,par2);
  case 1: return gsl_cdf_negative_binomial_Q(x,par1,par2*1.0);
  case 2: return gsl_cdf_pascal_Q(x,par1,par2*1.0);
  }
}

double discrete2_cdf_lower_inv(int type, unsigned int X, double par1, unsigned int par2)
{
  switch (type) {
  default : return 0;
  }
}

double discrete2_cdf_upper_inv(int type, unsigned int X, double par1, unsigned int par2)
{
  switch (type) {
  default : return 0;
  }
}

double discrete2_dist(int df, int type, unsigned int x, double par1, unsigned int par2)
{
  switch(df) {
  case 0: return discrete2_pdf(type,x,par1,par2);
  case 1: return discrete2_cdf_lower(type,x,par1,par2);
  case 2: return discrete2_cdf_upper(type,x,par1,par2);
  case 3: return discrete2_cdf_lower_inv(type,x,par1,par2);
  case 4: return discrete2_cdf_upper_inv(type,x,par1,par2);
  }
}

//////////////////////////////////////////////////////////////////////

int discrete3(int s, int type, unsigned int par1, unsigned int par2, unsigned int par3, double* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  switch(type) {
  case 0: { (*r) = gsl_ran_hypergeometric(rng,par1,par2,par3); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

int discrete3_s(gsl_rng* rng, int type, unsigned int par1, unsigned int par2, unsigned int par3, double* r)
{
  switch(type) {
  case 0: { (*r) = gsl_ran_hypergeometric(rng,par1,par2,par3); break; }
  }

  return 0;
}

int discrete3_v(int s, int type, unsigned int par1, unsigned int par2, unsigned int par3, int rs, double* r)
{
  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  int i;

  switch(type) {
  case 0: { for (i = 0; i < rs; i ++) r[i] = gsl_ran_hypergeometric(rng,par1,par2,par3); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

double discrete3_pdf(int type, double x, unsigned int par1, unsigned int par2, unsigned int par3)
{
  switch (type) {
  case 0: return gsl_ran_hypergeometric_pdf(x,par1,par2,par3);
  }
}

double discrete3_cdf_lower(int type, double x, unsigned int par1, unsigned int par2, unsigned int par3)
{
  switch (type) {
  case 0: return gsl_cdf_hypergeometric_P(x,par1,par2,par3);
  }
}

double discrete3_cdf_upper(int type, double x, unsigned int par1, unsigned int par2, unsigned int par3)
{
  switch (type) {
  case 0: return gsl_cdf_hypergeometric_Q(x,par1,par2,par3);
  }
}

double discrete3_cdf_lower_inv(int type, double X, unsigned int par1, unsigned int par2, unsigned int par3)
{
  switch (type) {
  default : return 0;
  }
}

double discrete3_cdf_upper_inv(int type, double X, unsigned int par1, unsigned int par2, unsigned int par3)
{
  switch (type) {
  default : return 0;
  }
}

double discrete3_dist(int df, int type, double x, unsigned int par1, unsigned int par2, unsigned int par3)
{
  switch(df) {
  case 0: return discrete3_pdf(type,x,par1,par2,par3);
  case 1: return discrete3_cdf_lower(type,x,par1,par2,par3);
  case 2: return discrete3_cdf_upper(type,x,par1,par2,par3);
  case 3: return discrete3_cdf_lower_inv(type,x,par1,par2,par3);
  case 4: return discrete3_cdf_upper_inv(type,x,par1,par2,par3);
  }
}

//////////////////////////////////////////////////////////////////////

int discrete_mp(int s, int type, unsigned int n, int ps, const double* p, int rs, unsigned int* r)
{
  if (ps != rs) return 2000; // BAD_SIZE

  const gsl_rng_type * T;
  gsl_rng * rng;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  rng = gsl_rng_alloc(T);
  gsl_rng_set(rng,s);

  switch(type) {
  case 0: { gsl_ran_multinomial(rng,ps,n,p,r); break; }
  }

  gsl_rng_free(rng);

  return 0;
}

int discrete_mp_s(gsl_rng* rng, int type, unsigned int n, int ps, const double* p, int rs, unsigned int* r)
{
  switch(type) {
  case 0: { gsl_ran_multinomial(rng,ps,n,p,r); break; }
  }
  return 0;
}

double discrete_mp_pdf(int type, int ps, const double* p, int qs, const unsigned int* q)
{
  switch (type) {
  case 0: return gsl_ran_multinomial_pdf(ps,p,q);
  }
  return 0;
}

double discrete_mp_cdf_lower(int type, int ps, const double* p, int qs, const unsigned int* q)
{
  switch (type) {
  default : return 0;
  }
}

double discrete_mp_cdf_upper(int type, int ps, const double* p, int qs, const unsigned int* q)
{
  switch (type) {
  default : return 0;
  }
}

double discrete_mp_cdf_lower_inv(int type, int ps, const double* p, int qs, const unsigned int* q)
{
  switch (type) {
  default : return 0;
  }
}

double discrete_mp_cdf_upper_inv(int type, int ps, const double* p, int qs, const unsigned int* q)
{
  switch (type) {
  default : return 0;
  }
}

 int discrete_mp_dist(int df, int type, double* r, int ps, const double* p, int qs, const unsigned int* q)
{
  switch(df) {
  case 0: { (*r) = discrete_mp_pdf(type,ps,p,qs,q); break; }
  case 1: { (*r) = discrete_mp_cdf_lower(type,ps,p,qs,q); break; }
  case 2: { (*r) = discrete_mp_cdf_upper(type,ps,p,qs,q); break; }
  case 3: { (*r) = discrete_mp_cdf_lower_inv(type,ps,p,qs,q); break; }
  case 4: { (*r) = discrete_mp_cdf_upper_inv(type,ps,p,qs,q); break; }
  }
  return 0;
}

//////////////////////////////////////////////////////////////////////

