#' Calculate potential evapotranspiration by Penman 1948 model
#'
#' @param Ta air temperature [degC]
#' @param Rn surface net radiation [W m-2]
#' @param U2 2 meters wind speed [m s-1]
#' @param VPD vapor pressure deficit [kPa]
#' @param Pa air pressure [kPa]
#' @param G soil heat flux [W m-2]
#'
#' @return potential evapotranspiration calculated by Penman 1948 model [mm d-1]
#' @export
#'
#' @examples PET_Penman1948(20, 50, 2, 0.5)
PET_Penman1948 <- function(
  Ta, Rn, U2, VPD,
  Pa = 101.325,
  G  = NULL
) {
  lambda <- cal_lambda(Ta = Ta)
  gma <- cal_gma(Pa = Pa, Ta = Ta)
  dlt <- cal_delta(Ta)
  fu <- 6.43 * (1 + 0.536 * U2) / lambda     # wind function

  coef_W2mm <- 0.0864 / lambda
  if (is.null(G)) {
    energy <- Rn * coef_W2mm
  } else {
    energy <- (Rn - G) * coef_W2mm
  }

  dlt / (dlt + gma) * energy + gma / (dlt + gma) * fu * VPD
}



#' Calculate maximum value of potential evapotranspiration in dry environment
#'
#' @param Tdry dry environment temperature [degC]
#' @param Rn surface net radiation [W m-2]
#' @param U2 2 meters wind speed [m s-1]
#' @param Pa air pressure [kPa]
#' @param Ta air temperature [degC]
#' @param G soil heat flux [W m-2]
#'
#' @return max value of potential evapotranspiration in dry environment [mm d-1]
#' @export
#'
#' @examples PET_Penman1948_max(35, 50, 2)
PET_Penman1948_max <- function(
  Tdry, Rn, U2,
  Pa = 101.325,
  Ta = NULL,
  G  = NULL
) {
  lambda <- cal_lambda(Ta = Ta)
  gma <- cal_gma(Pa = Pa, Ta = Ta)
  dlt_Tdry <- cal_delta(Tdry)
  fu <- 6.43 * (1 + 0.536 * U2) / lambda     # wind function

  coef_W2mm <- 0.0864 / lambda
  if (is.null(G)) {
    energy <- Rn * coef_W2mm
  } else {
    energy <- (Rn - G) * coef_W2mm
  }

  dlt_Tdry / (dlt_Tdry + gma) * energy + gma / (dlt_Tdry + gma) * fu * cal_es(Tdry)
}


