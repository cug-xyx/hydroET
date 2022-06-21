#' Title Calculate evapotranspiration by calibration-free complementary relationship model
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
ET_CR_Ma <- function(Ta, Rn, U2, VPD,
                     Pa = 101.325,
                     G = NULL) {
  Twb  = cal_Twb(VPD = VPD, Ta = Ta, Pa = Pa)
  Tdry = cal_Tdry(Twb = Twb, Pa = Pa, Ta = Ta)
  Tw   = cal_Tws(Ta = Ta, Rn = Rn, U2 = U2,
                 VPD = VPD, Pa = Pa, G = G)

  Epmax = ET_Penman1948_max(Tdry = Tdry, Rn = Rn, U2 = U2,
                            Pa = Pa, Ta = Ta, G  = G)
  Ep    = ET_Penman1948(Ta = Ta, Rn = Rn, U2 = U2,
                        VPD = VPD, Pa = Pa, G  = G)
  Ew    = ET_PT1972(Tw = Tw, Rn = Rn, Pa = Pa,
                    alpha = 1.26, Ta = Ta, G = G)

  X = (Epmax - Ep) / (Epmax - Ew) * Ew / Ep

  ET = (2 - X) * X ^ 2 * Ep

  return(ET)
}
