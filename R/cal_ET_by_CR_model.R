# Author: Yuxuan Xie
# Date  : 2022-05-23


inverse <- function (f,
                    lower = -100,
                    upper = 100) {
  function (y) uniroot((function (x) f(x) - y),
                       lower = lower, upper = upper,
                       tol = 1e-05 # iterative accuracy
  )$root
}


cal_lambda <- function(Tair) {
  return((2500 - Tair * 2.2)/1000)
}


cal_gma <- function(Pa = NULL,
                    lambda = NULL) {
  if (is.null(lambda)) lambda = 2.45  # latent heat of vaporization [MJ kg-1]
  if (is.null(Pa))     Pa     = 101.3 # a standard atmospheric pressure [kPa]

  cp      = 0.001013 # specific heat at constant pressure [MJ kg-1 deg-1]
  epsilon = 0.622    # ratio molecular weight of water vapor/dry air

  gma = (cp * Pa) / (epsilon * lambda) # psychrometric constant [kPa deg-1]

  return(gma)
}

cal_es <- function(Tair) {
  # saturation vapour pressure at the air temperature Tair [kPa]
  es = 0.6108 * exp((17.27 * Tair) / (Tair + 237.3))
}

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



cal_delta <- function(Tair) {
  dlt = 4098 * (0.6108 * exp((17.27 * Tair)/(Tair + 237.3)))/(Tair + 237.3)^2
  return(dlt)
}


cal_U10_to_U2 <- function(U10) {
  U2 = U10 * 4.87/log(67.8 * 10 - 5.42)
  return(U2)
}


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

cal_Tdry <- function(Twb,
                     Pa = NULL,
                     gma = NULL) {
  if (is.null(gma)) gma = cal_gma(Pa = Pa)

  Tdry = Twb + cal_es(Twb) / gma

  return(Tdry)
}


cal_Ep_max <- function(Tdry,
                       Rn,
                       U2,
                       G = NULL,
                       Pa = NULL,
                       gma = NULL) {
  if(is.null(gma)) gma = cal_gma(Pa = Pa) # gamma

  dlt_Tdry = cal_delta(Tdry) # delta of Tdry
  es_Tdry  = cal_es(Tdry)    # es at dry environment temperature [deg]
  fu = 2.6 * (1 + 0.54 * U2) # empirical wind function [mm d-1 kPa-1]

  # available energy
  if (is.null(G)) {
    energy = Rn
  } else (
    energy = Rn - G
  )

  Ep_max = dlt_Tdry * energy/(dlt_Tdry+gma) + gma/(dlt_Tdry+gma) * fu * es_Tdry
  return(Ep_max)
}


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

  beta_p = (energy - Ep) / Ep # bowen ratio of the well-watered patch

  # function
  # beta_p = gma * (Tws - Ta)/(0.6108 * exp((17.27 * Tws) / (Tws + 237.3)) - ea)

  # solve using the inverse function
  func <- function(Tws) beta_p * 0.6108 * exp((17.27*Tws)/(Tws+237.3)) - gma*Tws
  y = beta_p * ea - gma * Ta

  inv <- inverse(func, 0.0001, 100)

  sapply(y, FUN = inv)
}


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

  Ew = alpha * dlt_Tw / (dlt_Tw + gma) * energy
  return(Ew)
}

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

  Ep = dlt / (dlt + gma) * energy + gma / (dlt + gma) * fu * (es - ea)

  return(Ep)
}


cal_ET_by_CR_model <- function(Ta,
                               Td,
                               U2,
                               Rn,
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
  Ep = cal_Ep(Ta = Ta, Rn = Rn, U2 = U2, ea = ea, G = G, gma = gma)

  Tws = cal_Tws(Rn = Rn, Ep = Ep, Ta = Ta, ea = ea, gma = gma, G = G)
  Ew = cal_Ew(Tw = Tws, Rn = Rn, G = G, gma = gma)

  # function
  # y = (2 - X) * X ^ 2
  # y = ET / Ep
  X = (Ep_max - Ep) / (Ep_max - Ew) * Ew/Ep

  ET = Ep * (2 - X) * X ^ 2
  return(ET)
}







