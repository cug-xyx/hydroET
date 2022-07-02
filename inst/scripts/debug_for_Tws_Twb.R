library(data.table)
library(tidyverse)
devtools::load_all('../../hydroTools/')

d <- fread('inst/extdata/raw/54511.csv')

# function:
# beta_p = gma * (Tws - Ta) / (cal_es(Tws) - ea)
# beta_p * cal_es(Tws) = beta_p * ea + gma * Tws - gma * Ta
# beta_p * cal_es(Tws) - gma * Tws = beta_p * ea - gma * Ta

d %<>%
  mutate(VPD = hydroET::cal_VPD(Ta = Tavg, Pa = Pa, q = q),
         ET_CR_Ma_old = ET_CR_Ma_old(Ta = Tavg, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa),
         ET_CR_Ma = ET_CR_Ma(Ta = Tavg, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa),
         ET_CR_Ma_hydroTools = ET_CR_Ma2021(Rn = Rn, Tair = Tavg, D = VPD, U2 = U2, Pa = Pa)$ET_a,
         ET_CR_Xiao = ET_CR_Xiao2020(Rn = Rn, Tair = Tavg, D = VPD, U2 = U2, Pa = Pa)$ET_a,
         ET_PM = ET_Penman1948(Ta = Tavg, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa))

d %>% summary()

d %>% select(date, ET_CR_Ma,
             # ET_CR_Ma_hydroTools,
             ET_CR_Xiao, ET_PM) %>%
  pivot_longer(-date, names_to = 'type', values_to = 'ET') %>%
  group_by(year = year(date), type) %>%
  summarise(ET_yearly = sum(ET)) %>%
  ggplot(aes(x = year, y = ET_yearly, color = type)) +
  geom_line()

# debug 2022-07-02
d %>%
  filter(ET_CR_Ma_old <= 100, ET_CR_Ma_old >= 0,
         ET_CR_Ma_hydroTools <= 100, ET_CR_Ma_hydroTools >= 0) %>%
  select(date, ET_CR_Ma, ET_CR_Ma_old, ET_Penman1948 = ET_PM,
         ET_CR_Ma_hydroTools, ET_CR_Xiao) %>%
  pivot_longer(-date, names_to = 'type', values_to = 'ET') %>%
  group_by(year = year(date), type) %>%
  summarise(ET_yearly = sum(ET)) %>%
  ggplot(aes(x = year, y = ET_yearly, color = type)) +
  geom_line()


