#include <Rcpp.h>
#include <cmath>
#include "params_i.h"
#include "constants.h"
using namespace Rcpp;


inline double PET_PT1972_i(
    double Ta,
    double Rn,
    double Pa = PA_DEFAULT
) {
    double lambda = cal_lambda_i(Ta);  // [MJ/kg]
    double gma    = cal_gma_i(Ta, Pa);
    double dlt    = cal_delta_i(Ta);

    double coef_W2mm = 0.0864 / lambda;  // 转换因子

    double energy = Rn * coef_W2mm;

    return ALPHA * dlt / (dlt + gma) * energy;
}

// [[Rcpp::export]]
NumericVector PET_PT1972(
    NumericVector Ta,
    NumericVector Rn,
    NumericVector Pa
) {
    int n = Ta.size();
    if (Rn.size() != n || Pa.size() != n) {
        stop("Ta, Rn, and Pa must have the same length.");
    }

    NumericVector PET(n);
    for (int i = 0; i < n; i++) {
        PET[i] = PET_PT1972_i(Ta[i], Rn[i], Pa[i]);
    }
    return PET;
}