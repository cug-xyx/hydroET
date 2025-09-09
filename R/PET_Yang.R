#' Potential evapotranspiration (PET) calculated using
#' Maximum evaporating model developed by Yang and Roderick (2019)
#'
#' @param Rs shortwave radiation (solar radiation) [W m-2]
#' @param Rns net shortwave radiation [W m-2]
#' @param Rs_toa shortwave radiation form top of atmosphere [W m-2]
#' @param Ta near surface temperature (2m temperature) [degC]
#' @param Ts surfacee temperature [degC]
#' @param lat latitude [deg]
#' @param Pa surface pressure [kPa]
#' @param e surfacec emissivity
#'
#' @return potential evapotranspiration
#' @export
PET_Yang <- function(
  Rs, Rns, Rs_toa,
  Ta, Ts, 
  lat, 
  Pa = 101.325,
  e = 0.96
) {
  sigma <- 5.67 * 10 ^ -8  # [W m-2 K-4]

  Ts <- Ts + 273.15 # [degC] -> [K]
  lambda <- cal_lambda(Ta)
  # beta <- H / LE bowen ratio
  # Ts temperature at the evaporating surface [K]
  # e surface emissivity
  # dTa: Ts与大气有效辐射温度之间的温差 (Yang and Roderick, 2019)

  n1 <- 2.52
  n2 <- 2.38
  n3 <- 0.035
  tou <- Rs / Rs_toa
  dTa <- n1 * exp(n2 * tou) + n3 * abs(lat)

  Rn_Ts <- Rns + e * sigma * (Ts - dTa) ^ 4 - e * sigma * Ts ^ 4

  # m1 and m2 are fitting coefficient
  cp <- 0.001013
  epsilon <- 0.6220016
  gamma_Ts <- cp * Pa / epsilon * cal_lambda(Ts)
  delta_Ts <- 4098 * cal_es(Ts) / (Ts - 35.8) ^ 2
  beta_Ts <- 0.24 * gamma_Ts / delta_Ts # Yang and Roderick, 2019

  LE_max <- Rn_Ts / (1 + beta_Ts)

  LE_max / lambda
}