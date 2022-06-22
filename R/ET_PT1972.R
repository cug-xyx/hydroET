#' Calculate evapotranspiration by Priestley-Taylor (PT) model
#'
#' @param Tw wet environment temperature [degC]
#' @param Rn surface net radiation [W m-2]
#' @param Pa air pressure [kPa]
#' @param alpha Priestley-Taylor coefficient
#' @param Ta air temperature [degC]
#' @param G soil heat flux [W m-2]
#'
#' @return evapotranspiration calculated by Priestley-Taylor model [mm d-1]
#' @export
#'
#' @examples ET_PT1972(10, 50)
ET_PT1972 <- function(Tw, Rn,
                      Pa = 101.325,
                      alpha = 1.26,
                      Ta = NULL,
                      G = NULL) {
  lambda = cal_lambda(Ta = Ta)
  gma    = cal_gma(Pa = Pa, Ta = Ta)
  dlt_Tw = cal_delta(Tw)

  coef_W2mm = 0.0864 / lambda
  if (is.null(G)) {
    energy = Rn * coef_W2mm
  } else {
    energy = (Rn - G) * coef_W2mm
  }

  Ew = alpha * dlt_Tw / (dlt_Tw + gma) * energy

  return(Ew)
}
