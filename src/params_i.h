#include <Rcpp.h>
#include <cmath>
#include "constants.h"
using namespace Rcpp;


inline double cal_es_i(double Ta) {
  return 0.6108 * std::exp((17.27 * Ta) / (Ta + 237.3));
}



inline double cal_lambda_i(double Ta = NAN) {
  if (std::isnan(Ta)) return LAMBDA_DEFAULT;
  return 2.501 - 0.00237 * Ta;
}



inline double cal_gma_i(double Ta = NAN, double Pa = PA_DEFAULT) {
  double lambda = cal_lambda_i(Ta);
  return (CP * Pa) / (EPSILON * lambda);
}



inline double cal_delta_i(double Ta) {
  return 4098 * (0.6108 * std::exp((17.27 * Ta) / (Ta + 237.3))) / std::pow(Ta + 237.3, 2);
}



inline double cal_U2_i(double Uz, double z = 10.0) {
  if (z == 2.0) {
    return Uz;
  } else {
    return Uz * 4.87 / std::log(67.8 * z - 5.42);
  }
}