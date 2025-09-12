#' Calculate saturation vapor pressure
#' @name cal_es
#' 
#' @param Ta air temperature [degC]
#'
#' @return saturation vapor pressure [kPa]
#' @export
#'
#' @examples cal_es(20)
cal_es


#' Calculate lantent heat of vaporization
#' @name cal_lambda
#'
#' @param Ta air temperature [degC]
#'
#' @return lambda, latent heat of vaporization, about [2.45 MJ kg-1]
#' @export
#'
#' @examples cal_lambda(Ta = 20)
cal_lambda


#' Calculate psychrometric constant
#' @name cal_gma
#'
#' @param Pa air pressure [kPa]
#' @param Ta lantent heat of vaporization [MJ kg-1]
#'
#' @return psychrometric constant [kPa degC-1]
#' @export
#'
#' @examples cal_gma(Pa = 100, Ta = 20)
cal_gma


#' Calculate the slope of the saturation vapor pressure curve at certain air
#' @name cal_delta
#'
#' @param Ta air temperature [degC]
#'
#' @return slope (delta) [kPa degC-1]
#' @export
#'
#' @examples cal_delta(Ta = 20)
cal_delta


#' Converted h(m) wind speed to 2(m) wind speed
#' @name cal_U2
#'
#' @param Uz z meters wind speed [m s-1]
#' @param z measurement altitude of wind speed [m]
#'
#' @return 2 meters wind speed [m s-1]
#' @export
#'
#' @examples cal_U2(6)
cal_U2