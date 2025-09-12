#include <Rcpp.h>
#include <cmath>
#include "params_i.h"
#include "constants.h"
using namespace Rcpp;


 // [[Rcpp::export]]
NumericVector cal_es(NumericVector Ta) {
  return 0.6108 * exp((17.27 * Ta) / (Ta + 237.3));
}



// [[Rcpp::export]]
NumericVector cal_lambda(NumericVector Ta) {
  int n = Ta.size();
  NumericVector out(n);
  for (int i = 0; i < n; i++) {
     out[i] = cal_lambda_i(Ta[i]);
  }
  return out;
}



 // [[Rcpp::export]]
NumericVector cal_gma(NumericVector Ta, NumericVector Pa) {
  int n = Ta.size();
  NumericVector out(n);
  for (int i = 0; i < n; i++) {
    out[i] = cal_gma_i(Ta[i], Pa[i]);
  }
  return out;
}



// [[Rcpp::export]]
NumericVector cal_delta(NumericVector Ta) {
  int n = Ta.size();
  NumericVector out(n);
  for (int i = 0; i < n; i++) {
    out[i] = cal_delta_i(Ta[i]);
  }
  return out;
}



// [[Rcpp::export]]
NumericVector cal_U2(NumericVector Uz, double z = 10.0) {
  int n = Uz.size();
  NumericVector out(n);
  for (int i = 0; i < n; i++) {
    out[i] = cal_U2_i(Uz[i], z);
  }
  return out;
}