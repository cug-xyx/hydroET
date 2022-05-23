library(data.table)
library(tidyverse)

d = fread('data/raw/JRA55_SITE_50136.csv')

fread('data/raw/JRA55_SITE_50136.csv') %>%
  mutate(res = cal_ET_by_CR_model(Ta = Tair, q = q, Td = NULL, Rn = Rn, U2 = U2, Pa = Pa))
  # cal_ET_by_CR_model(Ta = Tair, q = q, Td = NULL, Rn = Rn, U2 = U2, Pa = Pa)
cal_gma(Pa = d$Pa)
cal_q2Td(q=d$q, Pa = d$Pa)
cal_ET_by_CR_model(Ta = d$Tair, q = d$q, Td = NULL, Rn = d$Rn, U2 = d$U2, Pa = d$Pa,
                   Ep = d$PET, Td = cal_q2Td(d$q))
Td = cal_q2Td(d$q)
cal_ET_by_CR_model(Td = Td, Ta = d$Tair, U2 = d$U2, Rn = d$Rn, Ep = d$PET)
