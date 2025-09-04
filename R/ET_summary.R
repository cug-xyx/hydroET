#' Calculate evapotranspiration of various types
#'
#' @param Ta air temperature [degC]
#' @param Rn surface net radiation [W m-2]
#' @param U2 2 meters wind speed [m s-1]
#' @param VPD vapor pressure deficit [kPa]
#' @param Pa air pressure [kPa]
#' @param G soil heat flux [W m-2]
#' @param alpha Priestley-Taylor coefficient
#'
#' @return A `data.frame` containing various types of evapotranspiration results
#' @export
#'
#' @examples ET_summary(20, 50, 3, 0.5)
ET_summary <- function(Ta, Rn, U2, VPD = VPD,
                       Pa = 101.325,
                       G = NULL,
                       alpha = 1.26) {
  Tw = cal_Tws(Ta = Ta, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa, G = G)

  ET_CR_Ma = ET_CR_Ma(Ta = Ta, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa, G = G)
  ET_Penman48 = ET_Penman1948(Ta = Ta, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa, G = G)
  ET_PT72 = ET_PT1972(Tw = Tw, Rn = Rn, Pa = Pa, alpha = alpha, Ta = Ta, G = G)

  data.frame(ET_CR_Ma, ET_Penman48, ET_PT72)
}
