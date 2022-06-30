library(data.table)
library(tidyverse)

d <- fread('inst/extdata/raw/50136.csv')

# function:
# beta_p = gma * (Tws - Ta) / (cal_es(Tws) - ea)
# beta_p * cal_es(Tws) = beta_p * ea + gma * Tws - gma * Ta
# beta_p * cal_es(Tws) - gma * Tws = beta_p * ea - gma * Ta

d %<>%
  mutate(VPD = cal_VPD(Ta = Tavg, Pa = Pa, q = q),
         ET_CR = ET_CR_Ma(Ta = Tavg, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa))



