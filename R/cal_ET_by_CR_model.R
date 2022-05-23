# 输入参数
# 1. 空气温度 Ta
# 2. 露点温度 Td
# 3. 风速     U
# 4. 净辐射   Rn

inverse <- function (f,
                    lower = -100,
                    upper = 100) {
  function (y) uniroot((function (x) f(x) - y),
                       lower = lower, upper = upper,
                       tol = 1e-05 # iterative accuracy
  )$root
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
                    gma = NULL) {
  if (is.null(gma)) gma = cal_gma()

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
                     gma = NULL) {
  if (is.null(gma)) gma = cal_gma()

  Tdry = Twb + cal_es(Twb) / gma

  return(Tdry)
}


cal_Ep_max <- function(Tdry,
                       Rn,
                       U2,
                       G = NULL,
                       gma = NULL) {
  if(is.null(gma)) gma = cal_gma() # gamma

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








