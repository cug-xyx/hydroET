#' Calculate lantent heat of vaporization
#'
#' @param Ta air temperature [degC]
#'
#' @return lambda, latent heat of vaporization, about [2.45 MJ kg-1]
#' @export
#'
#' @examples cal_lambda(20)
cal_lambda <- function(Ta = NULL) {
  if (is.null(Ta)) {
    lambda = 2.45
  } else {
    lambda = (2500 - Ta * 2.2) / 1000
  }

  return(lambda)
}


#' Calculate psychrometric constant
#'
#' @param Pa air pressure [kPa]
#' @param Ta lantent heat of vaporization [MJ kg-1]
#'
#' @return psychrometric constant [kPa degC-1]
#' @export
#'
#' @examples cal_gma(Pa = 100, Ta = 20)
cal_gma <- function(Pa = 101.325,   # atmospheric pressure [kPa]
                    Ta = NULL
                    ) {
  cp      = 0.001013 # specific heat at constant pressure [MJ kg-1 degC-1]
  epsilon = 0.622    # ratio molecular weight of water vapor/dry air
  lambda  = cal_lambda(Ta = Ta)

  gma = (cp * Pa) / (epsilon * lambda) # psychrometric constant [kPa degC-1]

  return(gma)
}


#' Calculate the slope of the saturation vapor pressure curve at certain air
#'
#' @param Ta air temperature [degC]
#'
#' @return slope (delta) [kPa degC-1]
#' @export
#'
#' @examples cal_delta(20)
cal_delta <- function(Ta) {
  dlt = 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/(Ta + 237.3)^2

  return(dlt)
}


#' Calculating vapor pressure deficit
#'
#' @param Ta air temperature [degC]
#' @param Pa air pressure [kPa]
#' @param ea actual vapour pressure [kPa]
#' @param Td dew-point temperature [degC]
#' @param q specific humidity [g g-1]
#'
#' @return vapor pressure deficit [kPa]
#' @export
#'
#' @examples cal_VPD(Ta = 20, ea = 1)
cal_VPD <- function(Ta,
                    Pa = 101.325,
                    ea = NULL,
                    Td = NULL,
                    q = NULL) {
  es = cal_es(Ta = Ta)

  # error should be handled first
  if (!is.null(ea)) {
    return(es - ea)
  } else if (!is.null(Td)) {
    ea = cal_es(Td)
    return(es - ea)
  } else if (!is.null(q)) {
    ea = q * Pa / 0.622
    return(es - ea)
  } else {
    stop('Missing key parameters for calculating ea')
  }
}


#' Calculate saturation vapor pressure
#'
#' @param Ta air temperature [degC]
#'
#' @return saturation vapor pressure [kPa]
#' @export
#'
#' @examples cal_es(20)
cal_es <- function(Ta) {
  # saturation vapour pressure at the air temperature [kPa]
  es = 0.6108 * exp((17.27 * Ta) / (Ta + 237.3))

  return(es)
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
  ea = cal_es(Ta) - VPD

  # solve using the inverse function
  func <- function(Td, ea=ea) 0.6108 * exp((17.27 * Td) / (Td + 237.3)) - ea
  Td = sapply(ea, FUN = function(ea) {
    uniroot(func, c(-200, 250), extendInt = 'yes', ea = ea)$root})

  return(Td)
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
cal_Twb <- function(VPD, Ta,
                    Pa = 101.325) {
  gma = cal_gma(Pa = Pa, Ta = Ta)
  Td  = VPD2Td(VPD, Ta)

  # solve using the inverse function
  # gma * Twb + cal_es(Twb) = cal_es(Td) + gma * Ta
  const = cal_es(Td) + gma * Ta

  func <- function(Twb, const = const, gma=gma) {
    gma * Twb + 0.6108 * exp((17.27 * Twb) / (Twb + 237.3)) - const
  }

  input_data = data.frame(const = const, gma = gma)
  Twb = sapply(1:nrow(input_data),FUN = function(num) {
    uniroot(func, c(-200, 250), extendInt = 'yes',
            const=input_data[num, ]$const, gma=input_data[num, ]$gma)$root})

  return(Twb)
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
cal_Tdry <- function(Twb,
                     Pa = 101.325,
                     Ta = NULL) {
  gma = cal_gma(Pa = Pa, Ta = Ta)

  Tdry = Twb + cal_es(Twb) / gma

  return(Tdry)
}


#' Converted zm wind speed to 2m wind speed
#'
#' @param Uz z meters wind speed [m s-1]
#' @param z.wind measurement altitude of wind speed [m]
#'
#' @return 2 meters wind speed [m s-1]
#' @export
#'
#' @examples cal_U2(6)
cal_U2 <- function(Uz, z.wind = 10) {
  if (z.wind == 2) {
    return(Uz)
  } else {
    return(U2 = Uz * 4.87 / log(67.8 * z.wind - 5.42))
  }
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
cal_Tws <- function(Ta, Rn, U2, VPD,
                    Pa = 101.325,
                    G  = NULL) {
  lambda = cal_lambda(Ta = Ta)
  gma    = cal_gma(Pa = Pa, Ta = Ta)
  dlt    = cal_delta(Ta)
  fu     = 2.6 * (1 + 0.54 * U2)     # wind function

  coef_W2mm = 0.0864 / lambda
  if (is.null(G)) {
    energy = Rn * coef_W2mm
  } else {
    energy = (Rn - G) * coef_W2mm
  }

  Ep = dlt / (dlt + gma) * energy + gma / (dlt + gma) * fu * VPD


  # core computing process
  beta_p = (energy - Ep) / Ep
  ea     = cal_es(Ta) - VPD

  # beta_p * cal_es(Tws) - gma * Tws = beta_p * ea - gma * Ta
  const = beta_p * ea - gma * Ta
  func <- function(Tws, beta_p = beta_p, gma = gma, const = const) {
    beta_p * 0.6108 * exp((17.27 * Tws) / (Tws + 237.3)) - gma * Tws - const
  }

  input_data = data.frame(const = const, gma = gma, beta_p = beta_p)
  Tws = sapply(1:nrow(input_data),FUN = function(num) {
    uniroot(func, c(-250, 180), extendInt = 'yes',
            const  = input_data[num, ]$const,
            gma    = input_data[num, ]$gma,
            beta_p = input_data[num, ]$beta_p)$root})
  Tws = dplyr::mutate(data.frame(Tws), Tws = ifelse(Tws <= Ta, Tws, Ta))$Tws

  return(Tws)
}





