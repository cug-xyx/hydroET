#' Potential evapotranspiration (PET) calculated using
#' Maximum evaporating model developed by Yang and Roderick (2019)
#'
#' @param Rs shortwave radiation (solar radiation) [W m-2]
#' @param Rns net shortwave radiation [W m-2]
#' @param Rs_toa shortwave radiation form top of atmosphere [W m-2]
#' @param Ts temperature at the evaporating surface [degC]
#' @param lat latitude [deg]
#' @param Pa surface pressure [kPa]
#' @param e surfacec emissivity
#'
#' @return potential evapotranspiration
#' @export
PET_Yang2019 <- function(
  Rs, Rns, Rs_toa,
  Ts, lat, 
  Pa = 101.325,
  e = 0.96
) {
  sigma <- 5.67 * 1e-8  # [W m-2 K-4]

  Ts <- Ts + 273.15 # [degC] -> [K]

  # weak function of surface temperature
  # lambda <- cal_lambda(Ta)
  lambda <- 2.51 * 1e3 - 2.32 * (Ts - 273.15)

  n1 <- 2.52
  n2 <- 2.38
  n3 <- 0.035
  tau <- Rs / Rs_toa
  dTa <- n1 * exp(n2 * tau) + n3 * abs(lat)

  Rnl_Ts <- e * sigma * ((Ts - dTa) ^ 4 - Ts ^ 4)
  # Rnl_Ts <- Rnl_Ts / 86400 * 1e+06 # [MJ m-2] -> [W m-2]

  Rn_Ts <- Rns + Rnl_Ts

  # m1 and m2 are fitting coefficient
  cp <- 1.013
  epsilon <- 0.6220016
  gamma_Ts <- (cp * Pa) / (epsilon * lambda)
  delta_Ts <- (4098 * cal_es(Ts - 273.15)) / ((Ts - 35.8) ^ 2)
  beta_Ts <- 0.24 * gamma_Ts / delta_Ts # Yang and Roderick, 2019

  LE_max <- Rn_Ts / (1 + beta_Ts)

  LE_max / lambda * 86400 * 1e-3 # [W m-2] -> [mm]
}