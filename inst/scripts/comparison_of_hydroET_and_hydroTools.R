library(tidyverse)
library(data.table)


d <- fread('H:/data for cal_PET/SITES_OBS/process_50136.csv')


# row 2312
for (i in 1:nrow(d)) {
  print(i)
  res = VPD2Td(VPD = d[i, ]$VPD, Ta = d[i, ]$Tair)
}

# d[2312]
# VPD2Td(0.25, -12.2)
# cal_es(-12.2) - 0.24 # ea = es - VPD
# cal_es(-12.2) / 100 * 57 # ea = es * RH
# (cal_es(7.3) + cal_es(-1.7)) / 200 * 57
# cal_VPD(Ta = -12.2, Pa = 97.76, ea)
