#include <gsl/gsl_statistics_double.h>

int mean(double* r, int vs, const double* v)
{
  (*r) = gsl_stats_mean(v,1,vs);
  return 0;
}

int variance(double* r, int vs, const double* v)
{
  (*r) = gsl_stats_variance(v,1,vs);
  return 0;
}

int variance_m(double m, double* r, int vs, const double* v)
{
  (*r) = gsl_stats_variance_m(v,1,vs,m);
  return 0;
}

int stddev(double* r, int vs, const double* v)
{
  (*r) = gsl_stats_sd(v,1,vs);
  return 0;
}

int stddev_m(double m, double* r, int vs, const double* v)
{
  (*r) = gsl_stats_sd_m(v,1,vs,m);
  return 0;
}

int tot_sumsq(double* r, int vs, const double* v)
{
  (*r) = gsl_stats_tss(v,1,vs);
  return 0;
}

int tot_sumsq_m(double m, double* r, int vs, const double* v)
{
  (*r) = gsl_stats_tss_m(v,1,vs,m);
  return 0;
}

int var_with_fixed_m(double m, double* r, int vs, const double* v)
{
  (*r) = gsl_stats_variance_with_fixed_mean(v,1,vs,m);
  return 0;
}

int stddev_with_fixed_m(double m, double* r, int vs, const double* v)
{
  (*r) = gsl_stats_sd_with_fixed_mean(v,1,vs,m);
  return 0;
}

int absdev(double m, double* r, int vs, const double* v)
{  
  (*r) = gsl_stats_absdev(v,1,vs);
  return 0;
}  

int absdev_m(double m, double* r, int vs, const double* v)
{
  (*r) = gsl_stats_absdev_m(v,1,vs,m);
  return 0;
}

int skew(double m, double* r, int vs, const double* v)
{  
  (*r) = gsl_stats_skew(v,1,vs);
  return 0;
}  

int skew_m_sd(double m, double sd, double* r, int vs, const double* v)
{
  (*r) = gsl_stats_skew_m_sd(v,1,vs,m,sd);
  return 0;
}

int kurtosis(double m, double* r, int vs, const double* v)
{  
  (*r) = gsl_stats_kurtosis(v,1,vs);
  return 0;
}  

int kurtosis_m_sd(double m, double sd, double* r, int vs, const double* v)
{
  (*r) = gsl_stats_kurtosis_m_sd(v,1,vs,m,sd);
  return 0;
}

int lag1_autocorrelation(double* r, int vs, const double* v)
{
  (*r) = gsl_stats_lag1_autocorrelation(v,1,vs);
  return 0;
}

int covariance(double* r, int vs, const double* v, int ws, const double* w)
{
  if (vs != ws) return 2000; // BAD_LENGTH
  (*r) = gsl_stats_covariance(v,1,w,1,vs);
  return 0;
}

int covariance_m(double mv, double mw, double* r, int vs, const double* v, int ws, const double* w)
{
  if (vs != ws) return 2000; // BAD_LENGTH
  (*r) = gsl_stats_covariance_m(v,1,w,1,vs,mv,mw);
  return 0;
}

int correlation(double* r, int vs, const double* v, int ws, const double* w)
{
  if (vs != ws) return 2000; // BAD_LENGTH
  (*r) = gsl_stats_correlation(v,1,w,1,vs);
  return 0;
}


int w_mean(double* r, int ws, const double* w, int vs, const double* v)
{
  (*r) = gsl_stats_wmean(w,1,v,1,vs);
  return 0;
}

int w_variance(double* r, int ws, const double* w, int vs, const double* v)
{
  (*r) = gsl_stats_wvariance(w,1,v,1,vs);
  return 0;
}

int w_variance_m(double m, double* r, int ws, const double* w, int vs, const double* v)
{
  (*r) = gsl_stats_wvariance_m(w,1,v,1,vs,m);
  return 0;
}

int w_stddev(double* r, int ws, const double* w, int vs, const double* v)
{
  (*r) = gsl_stats_wsd(w,1,v,1,vs);
  return 0;
}

int w_stddev_m(double m, double* r, int ws, const double* w, int vs, const double* v)
{
  (*r) = gsl_stats_wsd_m(w,1,v,1,vs,m);
  return 0;
}

int w_tot_sumsq(double* r, int ws, const double* w, int vs, const double* v)
{
  (*r) = gsl_stats_wtss(w,1,v,1,vs);
  return 0;
}

int w_tot_sumsq_m(double m, double* r, int ws, const double* w, int vs, const double* v)
{
  (*r) = gsl_stats_wtss_m(w,1,v,1,vs,m);
  return 0;
}

int w_var_with_fixed_m(double m, double* r, int ws, const double* w, int vs, const double* v)
{
  (*r) = gsl_stats_wvariance_with_fixed_mean(w,1,v,1,vs,m);
  return 0;
}

int w_stddev_with_fixed_m(double m, int ws, const double* w, double* r, int vs, const double* v)
{
  (*r) = gsl_stats_wsd_with_fixed_mean(w,1,v,1,vs,m);
  return 0;
}

int w_absdev(double m, int ws, const double* w, double* r, int vs, const double* v)
{  
  (*r) = gsl_stats_wabsdev(w,1,v,1,vs);
  return 0;
}  

int w_absdev_m(double m, int ws, const double* w, double* r, int vs, const double* v)
{
  (*r) = gsl_stats_wabsdev_m(w,1,v,1,vs,m);
  return 0;
}

int w_skew(double m, int ws, const double* w, double* r, int vs, const double* v)
{  
  (*r) = gsl_stats_wskew(w,1,v,1,vs);
  return 0;
}  

int w_skew_m_sd(double m, double sd, double* r, int ws, const double* w, int vs, const double* v)
{
  (*r) = gsl_stats_wskew_m_sd(w,1,v,1,vs,m,sd);
  return 0;
}

int w_kurtosis(double m, int ws, const double* w, double* r, int vs, const double* v)
{  
  (*r) = gsl_stats_wkurtosis(w,1,v,1,vs);
  return 0;
}  

int w_kurtosis_m_sd(double m, double sd, double* r, int ws, const double* w, int vs, const double* v)
{
  (*r) = gsl_stats_wkurtosis_m_sd(w,1,v,1,vs,m,sd);
  return 0;
}

int median(double* r, int vs, const double* v)
{
  (*r) = gsl_stats_median_from_sorted_data(v,1,vs);
  return 0;
}

int quantile(double f, double* r, int vs, const double* v)
{
  (*r) = gsl_stats_quantile_from_sorted_data(v,1,vs,f);
  return 0;
}



