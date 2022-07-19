library(tidyverse)
library(tidyfst)


fread('H:/data for cal_PET/SITES_CFSV2/59287.csv') %>%
  mutate(VPD = cal_VPD(Ta = Tavg, Pa = Pa, q = q),
         ET_CR_hydroET = ET_CR_Ma(Ta = Tavg, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa)) %>%
  summary()
