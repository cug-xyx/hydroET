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


#' Potential evapotranspiration (PET) calculated using
#' algorithm developed by Zhou and Yu (2024)
#' @name PET_Zhou2024
#'
#' @param Rn net surface radiation [W m-2]
#' @param Ta near surface (2m) temperature [degC]
#' @param Ts temperature at the evaporating surface [degC]
#' @param ea actual vapor pressure [kPa]
#' @param Pa surface pressure [kPa]
#'
#' @return potential evapotranspiration
#' @export
PET_Zhou2024


#' Potential evapotranspiration (PET) calculated using
#' Maximum evaporating model developed by Yang and Roderick (2019)
#' @name PET_Yang2019
#'
#' @param Rs shortwave radiation (solar radiation) [W m-2]
#' @param Rns net shortwave radiation [W m-2]
#' @param Rs_toa shortwave radiation form top of atmosphere [W m-2]
#' @param Ts temperature at the evaporating surface [degC]
#' @param lat latitude [deg]
#' @param Pa surface pressure [kPa]
#'
#' @return potential evapotranspiration
#' @export
PET_Yang2019