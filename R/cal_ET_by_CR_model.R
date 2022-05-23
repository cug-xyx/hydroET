# Author: Yuxuan Xie
# Date  : 2022-05-22


#' Derive the inverse function
#'
#' @param f raw function
#' @param lower the lower end points of the interval to be searched
#' @param upper the upper end points of the interval to be searched
#'
#' @return result of inverse function
#' @export
#'
#' @examples
#' inv = inverse(function(x) x ^ 2)
#' inv(4)
inverse <- function (f,
                    lower = -1000000,
                    upper = 10000000) {
  function (y) stats::uniroot((function (x) f(x) - y),
                       lower = lower, upper = upper,
                       tol = 1e-15, # iterative accuracy
                       extendInt = 'yes'
  )$root
}


#' Calculate lantent heat of vaporization
#'
#' @param Tair air temperature [degC]
#'
#' @return lambda, latent heat of vaporization, about [2.45 MJ kg-1]
#' @export
#'
#' @examples
#' cal_lambda(20)
cal_lambda <- function(Tair) {
  return((2500 - Tair * 2.2)/1000)
}


#' Calculate psychrometric constant
#'
#' @param Pa pressure [kPa]
#' @param lambda lantent heat of vaporization [MJ kg-1]
#'
#' @return psychrometric constant [kPa degC-1]
#' @export
#'
#' @examples
#' cal_gma()
cal_gma <- function(Pa = NULL,
                    lambda = NULL) {
  if (is.null(lambda)) lambda = 2.45  # latent heat of vaporization [MJ kg-1]
  if (is.null(Pa))     Pa     = 101.3 # a standard atmospheric pressure [kPa]

  cp      = 0.001013 # specific heat at constant pressure [MJ kg-1 degC-1]
  epsilon = 0.622    # ratio molecular weight of water vapor/dry air

  gma = (cp * Pa) / (epsilon * lambda) # psychrometric constant [kPa degC-1]

  return(gma)
}


#' Calculate saturation vapor pressure
#'
#' @param Tair air temperature [degC]
#'
#' @return saturation vapor pressure [kPa]
#' @export
#'
#' @examples
#' cal_es(20)
cal_es <- function(Tair) {
  # saturation vapour pressure at the air temperature Tair [kPa]
  es = 0.6108 * exp((17.27 * Tair) / (Tair + 237.3))
}


#' Calculate actual vapor pressure
#'
#' @param Ta air temperature [degC]
#' @param RH relative humidity [\%]
#' @param Tdew dew-point temperature [degC]
#' @param Pa pressure [kPa]
#' @param q specif humidity [kg kg-1]
#'
#' @return actual vapor pressure [kPa]
#' @export
#'
#' @examples
#' cal_ea(q = 0.5)
cal_ea <- function(Ta, RH = NULL,
                   Tdew = NULL,
                   Pa = NULL, q = NULL) {
  # need a exception handing
  if (!is.null(RH)) return(cal_es(Ta) * RH / 100)

  if (is.null(q)) {
    return(cal_es(Tdew))
  } else {
    if (is.null(Pa)) Pa = 101.3

    return(q * Pa / 0.622)
  }
}


#' Calculate the slope of the saturation vapor pressure curve at certain air
#' temperature Tair
#'
#' @param Tair air temperature [degC]
#'
#' @return slope (delta) [kPa degC-1]
#' @export
#'
#' @examples
#' cal_delta(20)
cal_delta <- function(Tair) {
  dlt = 4098 * (0.6108 * exp((17.27 * Tair)/(Tair + 237.3)))/(Tair + 237.3)^2
  return(dlt)
}


#' Converted 10m wind speed to 2m wind speed
#'
#' @param U10 10 meters wind speed [m s-1]
#'
#' @return 2 meters wind speed [m s-1]
#' @export
#'
#' @examples
#' cal_U10_to_U2(5)
cal_U10_to_U2 <- function(U10) {
  U2 = U10 * 4.87/log(67.8 * 10 - 5.42)
  return(U2)
}


cal_q2Td <- function(q, Pa = NULL) {
  ea = cal_ea(q = q, Pa = Pa)

  func <- function(Tdew) 0.6108 * exp((17.27 * Tdew) / (Tdew + 237.3))
  inv = inverse(func, lower = 0.1, upper = 1)
  sapply(ea, FUN = inv)
}


#' Calculate wet-bulb temperature
#'
#' @param Td dew-point temperature [degC]
#' @param Ta air temperature [degC]
#' @param Pa pressure [kPa]
#' @param gma psychrometric constant [kPa degC-1]
#'
#' @return wet-bulb temperature [degC]
#' @export
#'
#' @examples
#' cal_Twb(10, 20)
cal_Twb <- function(Td,
                    Ta,
                    Pa = NULL,
                    gma = NULL) {
  if (is.null(gma)) gma = cal_gma(Pa = Pa)

  es_Td = cal_es(Td) # es at dew-point temperature [kPa]

  # function
  # 0.6108 * exp((17.27 * Twb) / (Twb + 237.3)) + gma * Twb = es_Td + gma * Ta

  # solve using the inverse function
  func <- function(Twb) 0.6108 * exp((17.27 * Twb) / (Twb + 237.3)) + gma * Twb
  y = es_Td + gma * Ta

  inv <- inverse(func, 0.0001, 100)

  sapply(y, FUN = inv)
}


#' Calculate dry environment temperature
#'
#' @param Twb wet-bulb temperature [degC]
#' @param Pa pressure [kPa]
#' @param gma psychrometric constant [kPa degC-1]
#'
#' @return dry environment temperature [degC]
#' @export
#'
#' @examples
#' cal_Tdry(15)
cal_Tdry <- function(Twb,
                     Pa = NULL,
                     gma = NULL) {
  if (is.null(gma)) gma = cal_gma(Pa = Pa)

  Tdry = Twb + cal_es(Twb) / gma

  return(Tdry)
}


#' Calculate maximum value of potential evapotranspiration in dry environment
#'
#' @param Tdry dry environment temperature [degC]
#' @param Rn net radiation [W m-2]
#' @param U2 2 meters wind speed [m s-1]
#' @param G soil heat flux [W m-2]
#' @param Pa pressure [kPa]
#' @param gma psychrometric constant [kPa degC-1]
#'
#' @return max value of potential evapotranspiration in dry environment [mm d-1]
#' @export
#'
#' @examples
#' cal_Ep_max(Tdry = 25, Rn = 50, U2 = 3)
cal_Ep_max <- function(Tdry,
                       Rn,
                       U2,
                       G = NULL,
                       Pa = NULL,
                       gma = NULL) {
  if(is.null(gma)) gma = cal_gma(Pa = Pa) # gamma

  dlt_Tdry = cal_delta(Tdry) # delta of Tdry
  es_Tdry  = cal_es(Tdry)    # es at dry environment temperature [degC]
  fu = 2.6 * (1 + 0.54 * U2) # empirical wind function [mm d-1 kPa-1]

  # available energy
  if (is.null(G)) {
    energy = Rn
  } else (
    energy = Rn - G
  )

  coef_W2mm = 0.0864 / cal_lambda(Tdry)

  Ep_max = dlt_Tdry * energy * coef_W2mm / (dlt_Tdry + gma) +
    gma / (dlt_Tdry + gma) * fu * es_Tdry
  return(Ep_max)
}


#' Calculate wet surface temperature
#'
#' @param Rn net radiation [W m-2]
#' @param Ep potential evapotranspiration [mm d-1]
#' @param Ta air temperature [degC]
#' @param ea actual vapor pressure [kPa]
#' @param Pa pressure [kPa]
#' @param gma psychrometric constant [kPa degC-1]
#' @param G soil heat flux [W m-2]
#'
#' @return wet surface temperature [degC]
#' @export
#'
#' @examples
#' cal_Tws(Rn = 50, Ep = 5, Ta = 20, ea = 0.3)
cal_Tws <- function(Rn,
                    Ep,
                    Ta,
                    ea,
                    Pa = NULL,
                    gma = NULL,
                    G = NULL) {
  if (is.null(gma)) gma = cal_gma(Pa = Pa)

  if (is.null(G)) {
    energy = Rn
  } else {
    energy = Rn - G
  }

  coef_W2mm = 0.0864 / cal_lambda(Ta)

  beta_p = (energy *coef_W2mm - Ep) / Ep # bowen ratio of the well-watered patch

  # function
  # beta_p = gma * (Tws - Ta)/(0.6108 * exp((17.27 * Tws) / (Tws + 237.3)) - ea)

  # solve using the inverse function
  func <- function(Tws, beta_p, gma) beta_p * 0.6108 * exp((17.27*Tws)/(Tws+237.3)) - gma*Tws
  y = beta_p * ea - gma * Ta

  inv <- inverse(func, lower = -10, upper = 100)
  browser()
  df = data.frame(Tws = sapply(y, FUN = inv))
  dplyr::mutate(df, Tws = ifelse(Tws <= Ta, Tws, Ta))$Tws
}


#' Calculate evapotranspiration by Priestley-Taylor (PT) model
#'
#' @param Tw wet environment temperature [degC]
#' @param Rn net radiation [W m-2]
#' @param G soil heat flux [W m-2]
#' @param Pa pressure [kPa]
#' @param gma psychrometric constant [kPa degC-1]
#'
#' @return evapotranspiration calculated by Priestley-Taylor model [mm d-1]
#' @export
#'
#' @examples
#' cal_Ew(Tw = 10, Rn = 50)
cal_Ew <- function(Tw,
                   Rn,
                   G = NULL,
                   Pa = NULL,
                   gma = NULL) {
  if (is.null(gma)) gma = cal_gma(Pa = Pa)

  if (is.null(G)) {
    energy = Rn
  } else {
    energy = Rn - G
  }

  alpha = 1.26 # Priestley-Taylor coefficient

  dlt_Tw = cal_delta(Tw)

  coef_W2mm = 0.0864 / cal_lambda(Tw)

  Ew = alpha * dlt_Tw / (dlt_Tw + gma) * energy * coef_W2mm
  return(Ew)
}

#' Calculate potential evapotranspiration by Penman 1948 model
#'
#' @param Ta air temperature [degC]
#' @param Rn net radiation [W m-2]
#' @param U2 2 meters wind speed [m s-1]
#' @param ea actual vapor pressure [kPa]
#' @param G soil heat flux [W m-2]
#' @param Pa pressure [kPa]
#' @param gma psychrometric constant [kPa degC-1]
#'
#' @return potential evapotranspiration calculated by Penman 1948 model [mm d-1]
#' @export
#'
#' @examples
#' cal_Ep(Ta = 20, Rn = 50, U2 = 5, ea = 0.3)
cal_Ep <- function(Ta,
                   Rn,
                   U2,
                   ea,
                   G = NULL,
                   Pa = NULL,
                   gma = NULL) {
  if (is.null(gma)) gma = cal_gma(Pa = Pa)

  if (is.null(G)) {
    energy = Rn
  } else {
    energy = Rn - G
  }

  fu = 2.6 * (1 + 0.54 * U2) # empirical wind function [mm d-1 kPa-1]

  es = cal_es(Ta)

  dlt = cal_delta(Ta)

  coef_W2mm = 0.0864 / cal_lambda(Ta)

  Ep = dlt / (dlt + gma) * energy*coef_W2mm + gma / (dlt + gma) * fu * (es - ea)

  return(Ep)
}


#' Calculate evapotranspiration by calibration-free complementary relationship model
#'
#' @param Ta air temperature [degC]
#' @param Td dew-point temperature [degC]
#' @param U2 2 meters wind speed [m s-1]
#' @param Rn net radiation [W m-2]
#' @param G soil heat flux [W m-2]
#' @param gma psychrometric constant [kPa degC-1]
#' @param Pa pressure [kPa]
#' @param q specific humidity [kg kg-1]
#' @param RH relative humidity [\%]
#'
#' @return evapotranspiration calculated by calibration-free CR model [mm d-1]
#' @export
#'
#' @examples
#' cal_ET_by_CR_model(Ta = 20, Td = 8, U2 = 3, Rn = 50)
cal_ET_by_CR_model <- function(Ta,
                               Td,
                               U2,
                               Rn,
                               Ep = NULL,
                               G = NULL,
                               gma = NULL,
                               Pa = NULL,
                               q = NULL,
                               RH = NULL) {
  if (is.null(gma)) gma = cal_gma(Pa = Pa)

  ea = cal_ea(Ta = Ta, RH = RH, Tdew = Td, Pa = Pa, q = q)

  Twb = cal_Twb(Td = Td, Ta = Ta, gma = gma)
  Tdry = cal_Tdry(Twb = Twb, gma = gma)

  Ep_max = cal_Ep_max(Tdry = Tdry, Rn = Rn, U2 = U2, G = G, gma = gma)
  if (is.null(Ep)) {
    Ep = cal_Ep(Ta = Ta, Rn = Rn, U2 = U2, ea = ea, G = G, gma = gma)
  }

  Tws = cal_Tws(Rn = Rn, Ep = Ep, Ta = Ta, ea = ea, gma = gma, G = G)
  Ew = cal_Ew(Tw = Tws, Rn = Rn, G = G, gma = gma)

  # function
  # y = (2 - X) * X ^ 2
  # y = ET / Ep
  X = (Ep_max - Ep) / (Ep_max - Ew) * Ew/Ep

  ET = Ep * (2 - X) * X ^ 2
  return(ET)
}






