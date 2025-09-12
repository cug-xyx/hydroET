#' Calculating vapor pressure deficit
#'
#' @param Ta air temperature [degC]
#' @param Pa air pressure [kPa]
#' @param ea actual vapour pressure [kPa]
#' @param Tdew dew-point temperature [degC]
#' @param q specific humidity [g g-1]
#'
#' @return vapor pressure deficit [kPa]
#' @export
#'
#' @examples cal_VPD(Ta = 20, ea = 1)
cal_VPD <- function(
  Ta,
  Pa = 101.325,
  ea = NULL,
  Tdew = NULL,
  q = NULL
) {
  epsilon <- 0.6220016
  es <- cal_es(Ta = Ta)

  # error should be handled first
  if (!is.null(ea)) {
    return(es - ea)
  } else if (!is.null(Tdew)) {
    ea <- cal_es(Tdew)
    return(es - ea)
  } else if (!is.null(q)) {
    ea <- q * Pa / (epsilon + (1 - epsilon) * q)
    return(es - ea)
  } else {
    stop('Missing key parameters for calculating `ea`')
  }
}


#' Convert vapor pressure deficit to dew-point temperature
#'
#' @param VPD vapor pressure deficit [kPa]
#' @param Ta air temperature [degC]
#'
#' @importFrom  stats uniroot
#'
#' @return dew-point temperature [degC]
#' @export
#'
#' @examples VPD2Td(0.5, 20)
VPD2Td <- function(VPD, Ta) {
  ea <- pmax(cal_es(Ta) - VPD, 0)

  # solve using the inverse function
  func <- function(Td, ea = ea) 0.6108 * exp((17.27 * Td) / (Td + 237.3)) - ea

  sapply(ea, FUN = function(ea) {
    if (is.na(ea)) return(NA)
    uniroot(func, c(-100, 80), extendInt = 'yes', tol = 1e-7, ea = ea)$root
  })
}


#' Calculate wet-bulb temperature
#'
#' @param VPD vapor pressure deficit [kPa]
#' @param Ta air temperature [degC]
#' @param Pa air pressure [kPa]
#'
#' @importFrom  stats uniroot
#'
#' @return wet-bulb temperature [degC]
#' @export
#'
#' @examples cal_Twb(0.5, 20)
cal_Twb <- function(
  VPD, Ta,
  Pa = 101.325
) {
  gma <- cal_gma(Pa = Pa, Ta = Ta)
  Td <- VPD2Td(VPD, Ta)

  # solve using the inverse function
  # gma * Twb + cal_es(Twb) = cal_es(Td) + gma * Ta
  const <- cal_es(Td) + gma * Ta

  func <- function(Twb, const = const, gma = gma) {
    gma * Twb + 0.6108 * exp((17.27 * Twb) / (Twb + 237.3)) - const
  }

  input_data <- data.frame(const = const, gma = gma)
  sapply(seq_len(nrow(input_data)), FUN = function(num) {
    if (NA %in% as.numeric(input_data[num, ])) return(NA)

    uniroot(
      func, c(-100, 180), extendInt = 'yes', tol = 1e-7,
      const = input_data[num, ]$const, gma = input_data[num, ]$gma)$root
  })
}


#' Calculate dry environment temperature
#'
#' @param Twb wet-bulb temperature [degC]
#' @param Pa air pressure [kPa]
#' @param Ta air temperature [degC]
#'
#' @return dry environment temperature [degC]
#' @export
#'
#' @examples cal_Tdry(5)
cal_Tdry <- function(
  Twb,
  Pa = 101.325,
  Ta = NULL
) {
  gma <- cal_gma(Pa = Pa, Ta = Ta)

  Twb + cal_es(Twb) / gma
}


#' Calculate wet surface temperature
#'
#' @param Ta air temperature [degC]
#' @param Rn surface net radiation [W m-2]
#' @param U2 2 meters wind speed [m s-1]
#' @param VPD vapor pressure deficit [kPa]
#' @param Pa air pressure [kPa]
#' @param G soil heat flux [W m-2]
#'
#' @importFrom  stats uniroot
#'
#' @return wet surface temperature [degC]
#' @export
#'
#' @examples cal_Tws(20, 50, 3, 0.5)
cal_Tws <- function(
  Ta, Rn, U2, VPD,
  Pa = 101.325,
  G  = NULL
) {
  lambda <- cal_lambda(Ta = Ta)
  gma <- cal_gma(Pa = Pa, Ta = Ta)
  dlt <- cal_delta(Ta)
  fu <- 2.6 * (1 + 0.54 * U2)     # wind function

  coef_W2mm <- 0.0864 / lambda
  if (is.null(G)) {
    energy <- Rn * coef_W2mm
  } else {
    energy <- (Rn - G) * coef_W2mm
  }

  Ep <- dlt / (dlt + gma) * energy + gma / (dlt + gma) * fu * VPD

  # core computing process
  beta_p <- (energy - Ep) / Ep
  ea <- cal_es(Ta) - VPD

  # beta_p * cal_es(Tws) - gma * Tws = beta_p * ea - gma * Ta
  const <- beta_p * ea - gma * Ta
  func <- function(Tws, beta_p = beta_p, gma = gma, const = const) {
    beta_p * 0.6108 * exp((17.27 * Tws) / (Tws + 237.3)) - gma * Tws - const
  }

  input_data <- data.frame(const = const, gma = gma, beta_p = beta_p)
  Tws <- sapply(seq_len(nrow(input_data)), FUN = function(num) {
    if (NA %in% as.numeric(input_data[num, ])) return(NA)

    uniroot(
      func, c(-100, 180), extendInt = 'yes', tol = 1e-7,
      const = input_data[num, ]$const,
      gma = input_data[num, ]$gma,
      beta_p = input_data[num, ]$beta_p)$root
  })

  Tws <- ifelse(Tws <= Ta, Tws, Ta)

  Tws
}


#' Calibrating PT-alpha coefficients for widely wet surfaces
#'
#' @param Ta air temperature [degC]
#' @param Tws wet surface temperature [degC]
#' @param VPD vapor pressure deficit [kPa]
#' @param Pa air pressure [kPa]
#'
#' @return PT-alpha coefficients
#' @export
#'
#' @examples calib_alpha(20, 10, 0.5)
calib_alpha <- function(
  Ta, Tws, VPD,
  Pa = 101.325
) {
  gma <- cal_gma(Pa = Pa, Ta = Ta)
  ea <- cal_es(Ta) - VPD
  es_Tws <- cal_es(Tws)
  dlt_Ta <- cal_delta(Ta)

  (dlt_Ta + gma) * (es_Tws - ea) / dlt_Ta * ((es_Tws - ea) + gma * (Tws - Ta))
}