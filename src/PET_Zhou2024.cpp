#include <Rcpp.h>
#include <cmath>
#include "params_i.h"
#include "constants.h"
using namespace Rcpp;


inline double PET_Zhou2024_i(
  double Rn, double Ta, double Ts, double ea, double Pa,
  double H, double LE
) {
  double lambda = cal_lambda_i(Ta) / 86400.0 * 1e6;  // [MJ] -> [W]
  double gamma  = cal_gma_i(Ta, Pa);
  double es_s   = cal_es_i(Ts);
  double ea_s = cal_es_i(Ta);

  double delta = (es_s - ea_s) / (Ts - Ta);
  
  // beta_w calculation
  double beta = H / LE;
  if (!R_finite(beta)) beta = (beta > 0) ? 1e8 : -1e8;
  double beta_o = 0.24 * gamma / delta;

  double beta_w = (gamma * (Ts - Ta)) / (es_s - ea);
  if (beta_w > beta) beta_w = beta;
  if (beta_w < beta_o) beta_w = beta_o;

  return Rn / (1.0 + beta_w) / lambda; // [mm]
}

// [[Rcpp::export]]
NumericVector PET_Zhou2024(
    NumericVector Rn, NumericVector Ta, NumericVector Ts,
    NumericVector ea, NumericVector Pa,
    NumericVector H, NumericVector LE
) {
  int n = Rn.size();
  if (Ta.size() != n || Ts.size() != n || ea.size() != n || Pa.size() != n || H.size() != n || LE.size() != n) {
    stop("All input vectors must have the same length");
  }

  NumericVector PETe(n);
  for (int i = 0; i < n; i++) {
    PETe[i] = PET_Zhou2024_i(Rn[i], Ta[i], Ts[i], ea[i], Pa[i], H[i], LE[i]);
  }
  return PETe;
}