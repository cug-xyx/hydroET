#include <Rcpp.h>
#include <cmath>
#include "params_i.h"
#include "constants.h"
using namespace Rcpp;


inline double PET_Penman1948_i(
    double Ta, double Rn,
    double U2, double VPD, double Pa
) {
    double lambda = cal_lambda_i(Ta);
    double gma    = cal_gma_i(Ta, Pa);
    double dlt    = cal_delta_i(Ta);

    double fu = 6.43 * (1.0 + 0.536 * U2) / lambda;  // wind function

    double coef_W2mm = 0.0864 / lambda;
    double energy = Rn * coef_W2mm;

    return dlt / (dlt + gma) * energy + gma / (dlt + gma) * fu * VPD;
}

// [[Rcpp::export]]
NumericVector PET_Penman1948(
    NumericVector Ta, NumericVector Rn,
    NumericVector U2, NumericVector VPD, NumericVector Pa
) {
    int n = Ta.size();
    if (Rn.size() != n || U2.size() != n || VPD.size() != n || Pa.size() != n) {
        stop("All input vectors must have the same length.");
    }

    NumericVector PET(n);
    for (int i = 0; i < n; i++) {
        PET[i] = PET_Penman1948_i(Ta[i], Rn[i], U2[i], VPD[i], Pa[i]);
    }
    return PET;
}



inline double PET_Penman1948_max_i(
    double Ta, double Tdry,
    double Rn, double U2, double Pa
) {
    double lambda    = cal_lambda_i(Ta);
    double gma       = cal_gma_i(Ta, Pa);
    double dlt_Tdry  = cal_delta_i(Tdry);

    double coef_W2mm = 0.0864 / lambda;
    double energy = Rn * coef_W2mm;

    return dlt_Tdry / (dlt_Tdry + gma) * energy + gma / (dlt_Tdry + gma) * cal_es_i(Tdry);
}

// [[Rcpp::export]]
NumericVector PET_Penman1948_max(
    NumericVector Ta, NumericVector Tdry,
    NumericVector Rn, NumericVector U2, NumericVector Pa
) {
    int n = Ta.size();
    if (Tdry.size() != n || Rn.size() != n || U2.size() != n || Pa.size() != n) {
        stop("All input vectors must have the same length.");
    }

    NumericVector PET(n);
    for (int i = 0; i < n; i++) {
        PET[i] = PET_Penman1948_max_i(Ta[i], Tdry[i], Rn[i], U2[i], Pa[i]);
    }
    return PET;
}