#' Calculate evapotranspiration by calibration-free complementary relationship model
#'
#' @param Ta air temperature [degC]
#' @param Rn surface net radiation [W m-2]
#' @param U2 2 meters wind speed [m s-1]
#' @param VPD vapor pressure deficit [kPa]
#' @param Pa air pressure [kPa]
#' @param G soil heat flux [W m-2]
#'
#' @return evapotranspiration calculated by calibration-free CR model [mm d-1]
#' @export
#'
#' @examples ET_CR_Ma_ref(20, 50, 2, 0.5)
ET_CR_Ma_ref <- function(
  Ta, Rn, U2, VPD,
  Pa = 101.325,
  G = NULL
) {
  Twb <- cal_Twb(VPD = VPD, Ta = Ta, Pa = Pa)
  Tdry <- cal_Tdry(Twb = Twb, Pa = Pa, Ta = Ta)
  Tw <- cal_Tws(Ta = Ta, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa, G = G)

  Epmax <- PET_Penman1948_max(
    Tdry = Tdry, Rn = Rn, U2 = U2, Pa = Pa, Ta = Ta, G  = G)
  Ep <- PET_Penman1948(
    Ta = Ta, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa, G  = G)
  # TODO: check this funciton
  Ew <- PET_PT1972(
    Ta = Tw, Rn = Rn, Pa = Pa, alpha = 1.26)

  # 2022-07-02 refer to Prof. Ning Ma's code
  Ew <- pmin(Ew, Ep)
  x <- ifelse(Ep > 0, Ew / Ep, 1)
  xmin <- Ew / Epmax
  x <- ifelse(x < xmin, xmin, x)
  X <- (Epmax - Ep) / (Epmax - Ew)
  ET <- Ep * (2 * (X ^ 2) - X ^ 3)
  ET <- pmax(ET, 0)

  ET
}


#' Calculate evapotranspiration by calibration-free 
#' complementary relationship model
#'
#' @param Ta air temperature [degC]
#' @param Rn surface net radiation [W m-2]
#' @param U2 2 meters wind speed [m s-1]
#' @param VPD vapor pressure deficit [kPa]
#' @param Pa air pressure [kPa]
#' @param G soil heat flux [W m-2]
#'
#' @return evapotranspiration calculated by calibration-free CR model [mm d-1]
#' @export
#'
#' @examples ET_CR_Ma(20, 50, 2, 0.5)
ET_CR_Ma <- function(
  Ta, Rn, U2, VPD,
  Pa = 101.325,
  G = NULL
) {
  Twb <- cal_Twb(VPD = VPD, Ta = Ta, Pa = Pa)
  Tdry <- cal_Tdry(Twb = Twb, Pa = Pa, Ta = Ta)
  Tw <- cal_Tws(Ta = Ta, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa, G = G)

  Epmax <- PET_Penman1948_max(
    Tdry = Tdry, Rn = Rn, U2 = U2, Pa = Pa, Ta = Ta, G  = G)
  Ep <- PET_Penman1948(
    Ta = Ta, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa, G  = G)
  # TODO: check this funciton
  Ew <- PET_PT1972(
    Ta = Tw, Rn = Rn, Pa = Pa, alpha = 1.26)

  # old version
  X <- (Epmax - Ep) / (Epmax - Ew) * Ew / Ep
  ET <- (2 - X) * (X ^ 2) * Ep

  ET <- ifelse(Rn < 0 | ET < 0, 0, ET)

  ET
}
