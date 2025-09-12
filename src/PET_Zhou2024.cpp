#include <Rcpp.h>
#include <cmath>
#include "params_i.h"
#include "constants.h"
using namespace Rcpp;


inline double PET_Zhou2024_i(double Rn, double Ta, double Ts, double ea, double Pa) {
  double lambda = cal_lambda_i(Ta) / 86400.0 * 1e6;  // [MJ] -> [W]
  double gamma  = cal_gma_i(Ta, Pa);
  double es_s   = cal_es_i(Ts);

  double beta_w = (gamma * (Ts - Ta)) / (es_s - ea);

  return Rn / (1.0 + beta_w) / lambda; // [mm]
}

// [[Rcpp::export]]
NumericVector PET_Zhou2024(
    NumericVector Rn,
    NumericVector Ta,
    NumericVector Ts,
    NumericVector ea,
    NumericVector Pa
) {
  int n = Rn.size();
  if (Ta.size() != n || Ts.size() != n || ea.size() != n || Pa.size() != n) {
    stop("All input vectors must have the same length");
  }

  NumericVector PETe(n);
  for (int i = 0; i < n; i++) {
    PETe[i] = PET_Zhou2024_i(Rn[i], Ta[i], Ts[i], ea[i], Pa[i]);
  }
  return PETe;
}