# 输入参数
# 1. 空气温度 Ta
# 2. 露点温度 Td
# 3. 风速     U
# 4. 净辐射   Rn

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


