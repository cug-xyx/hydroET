#' Calculate evapotranspiration by calibration-free complementary relationship model
#'
#' @param Ta air temperature [degC]
#' @param Rn surface net radiation [W m-2]
#' @param U2 2 meters wind speed [m s-1]
#' @param VPD vapor pressure deficit [kPa]
#' @param Pa air pressure [kPa]
#' @param G soil heat flux [W m-2]
#'
#' @importFrom magrittr `%>%` `%<>%`
#' @importFrom dplyr mutate
#'
#' @return evapotranspiration calculated by calibration-free CR model [mm d-1]
#' @export
#'
#' @examples ET_CR_Ma_ref(20, 50, 2, 0.5)
ET_CR_Ma_ref <- function(Ta, Rn, U2, VPD,
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

  # 2022-07-02 refer to Ma's code
  ET_df = data.frame(Ew, Ep, Epmax)
  ET_df %<>%
    dplyr::mutate(Ew = min(Ew, Ep),
           x = ifelse(Ep > 0, Ew / Ep, 1),
           xmin = Ew / Epmax,
           x = ifelse(x < xmin, xmin, x),
           # X = (x - xmin) / (1 - xmin),
           X = (Epmax - Ep) / (Epmax - Ew),
           ET = Ep * (2 * (X ^ 2) - X ^ 3),
           # ET = ifelse(ET < 0, 0, ET)
           )

  ET = ET_df$ET

  # old version
  # X = (Epmax - Ep) / (Epmax - Ew) * Ew / Ep
  # ET = (2 - X) * (X ^ 2) * Ep

  # if (ET < 0) ET = 0

  return(ET)
}


#' Calculate evapotranspiration by calibration-free complementary relationship model
#'
#' @param Ta air temperature [degC]
#' @param Rn surface net radiation [W m-2]
#' @param U2 2 meters wind speed [m s-1]
#' @param VPD vapor pressure deficit [kPa]
#' @param Pa air pressure [kPa]
#' @param G soil heat flux [W m-2]
#'
#' @importFrom magrittr `%>%` `%<>%`
#' @importFrom dplyr tibble pull
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

  # old version
  X = (Epmax - Ep) / (Epmax - Ew) * Ew / Ep
  ET = (2 - X) * (X ^ 2) * Ep

  ET = dplyr::tibble(Rn, ET) %>%
    mutate(ET = ifelse(Rn < 0 | ET < 0, 0, ET)) %>%
    dplyr::pull(ET)

  return(ET)
}
