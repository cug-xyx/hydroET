#' Potential evapotranspiration (PET) calculated using
#' algorithm developed by Zhou and Yu (2024)
#'
#' @param Rn net surface radiation [W m-2]
#' @param Ta near surface (2m) temperature [degC]
#' @param Ts temperature at the evaporating surface [degC]
#' @param ea actual vapor pressure [kPa]
#' @param Pa surface pressure [kPa]
#'
#' @return potential evapotranspiration
#' @export
PET_Zhou2024 <- function(
  Rn, Ta, Ts, ea,
  Pa = 101.325
) {
  lambda <- cal_lambda(Ta) / 86400 * 1e6 # [MJ] -> [W]
  gamma <- cal_gma(Pa = Pa, Ta = Ta)

  es_s <- cal_es(Ts)

  beta_w <- (gamma * (Ts - Ta)) / (es_s - ea)

  PETe <- Rn / (1 + beta_w) / lambda # [mm]

  PETe
}