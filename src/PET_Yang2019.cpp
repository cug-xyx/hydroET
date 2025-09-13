#include <Rcpp.h>
#include <cmath>
#include "params_i.h"
#include "constants.h"
using namespace Rcpp;


inline double PET_Yang2019_i(
    double Rs, double Rns, double Rs_toa,
    double Ts, double lat, double Pa
) {
    const double n1 = 2.52;
    const double n2 = 2.38;
    const double n3 = 0.035;
    const double cp = 1.013; // [MJ kg-1 degC-1]

    double Ts_K = Ts + T2K;
    double lambda = 2501.0 - 2.32 * (Ts_K - T2K);

    // [0, 1]
    double tau = Rs / Rs_toa;
    if (!R_finite(tau) || Rcpp::NumericVector::is_na(tau)) {
        tau = 0.0;
    } else if (tau > 1.0) {
        tau = 1.0;
    }
    double dTa = n1 * std::exp(n2 * tau) + n3 * std::abs(lat);

    double Rnl_Ts = EMISSIVITY * SIGMA * (std::pow(Ts_K - dTa, 4) - std::pow(Ts_K, 4));
    double Rn_Ts  = Rns + Rnl_Ts;

    double gamma_Ts = (cp * Pa) / (EPSILON * lambda);
    double delta_Ts = 4098.0 * cal_es_i(Ts_K - T2K) / std::pow((Ts_K - T2K) - 35.8, 2);
    double beta_Ts  = 0.24 * gamma_Ts / delta_Ts;

    double LE_max = Rn_Ts / (1.0 + beta_Ts);

    return LE_max / lambda * 86400.0 * 1e-3;  // [mm]
}

// [[Rcpp::export]]
NumericVector PET_Yang2019(
    NumericVector Rs,
    NumericVector Rns,
    NumericVector Rs_toa,
    NumericVector Ts,
    NumericVector lat,
    NumericVector Pa
) {
    int n = Rs.size();
    if (Rns.size() != n || Rs_toa.size() != n || Ts.size() != n ||
        lat.size() != n || Pa.size() != n) {
        stop("All input vectors must have the same length.");
    }

    NumericVector PET(n);
    for (int i = 0; i < n; i++) {
        PET[i] = PET_Yang2019_i(Rs[i], Rns[i], Rs_toa[i], Ts[i], lat[i], Pa[i]);
    }
    return PET;
}