library(tidyverse)
library(data.table)


fread('H:/data for cal_PET/SITES_OBS/process_50136.csv') %>%
  mutate(
         ET_CR_hydroET = ifelse(Rn < 0 | VPD < 0, 0,
           ET_CR_Ma(Ta = Tair, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa))) %>%
  summary()
