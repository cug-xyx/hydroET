#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector cal_es(NumericVector Ta) {
  int n = Ta.size();
  NumericVector out(n);
  for(int i = 0; i < n; i++) {
    out[i] = 0.6108 * exp((17.27 * Ta[i]) / (Ta[i] + 237.3));
  }
  return out;
}