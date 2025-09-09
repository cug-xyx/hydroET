#' Calculate evapotranspiration by Priestley-Taylor (PT) model
#'
#' @param Ta air temperature [degC]
#' @param Rn surface net radiation [W m-2]
#' @param Pa air pressure [kPa]
#' @param alpha Priestley-Taylor coefficient
#'
#' @return evapotranspiration calculated by Priestley-Taylor model [mm d-1]
#' @export
#'
#' @examples PET_PT1972(10, 50)
PET_PT1972 <- function(
  Ta, Rn,
  Pa = 101.325,
  alpha = 1.26
) {
  lambda <- cal_lambda(Ta = Ta)
  gma <- cal_gma(Pa = Pa, Ta = Ta)
  dlt <- cal_delta(Ta)

  coef_W2mm <- 0.0864 / lambda

  energy <- Rn * coef_W2mm

  alpha * dlt / (dlt + gma) * energy
}
