# input params:
# Rn: surface net radiation
# Ta: near surface temperature
# Td: dew-point temperature --> VPD = es - ea = es(Ta) - ea(Td)
# U2: 2-meters wind speed
# Pa: near surface pressure


cal_lambda <- function(Ta) {
  lambda = (2500 - Ta * 2.2) / 1000

  return(lambda)
}


cal_gma <- function(Pa = 101.3,   # atmospheric pressure [kPa]
                    lambda = 2.45 # latent heat of vaporization [MJ kg-1]
                    ) {
  cp      = 0.001013 # specific heat at constant pressure [MJ kg-1 degC-1]
  epsilon = 0.622    # ratio molecular weight of water vapor/dry air

  gma = (cp * Pa) / (epsilon * lambda) # psychrometric constant [kPa degC-1]

  return(gma)
}






