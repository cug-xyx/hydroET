library(data.table)
library(tidyverse)
library(magrittr)
devtools::load_all('../../hydroTools/')

# ET_CR_Ma bug过多，选择使用hydroTools包进行计算
# OBS 50136站点异常  54511, 59287 站点正常, Rn<0导致？

# OBS
# d < fread('H:/data for cal_PET/SITES_OBS/process_50137.csv')
# d
# d %>%
#   # filter(year(date) >= 1979, year(date) <= 2019) %>%
#   select(lon ,alt, date, PET, Rn, Tair, VPD, U2, Pa) %>%
#   mutate(
#     ET_CR = ET_CR_Ma2021(Rn = Rn, Tair = Tair, D = VPD, U2 = U2, Pa = Pa)$ET_a) %>%
#   filter(ET_CR >= 10)

d <- fread('H:/data for cal_PET/SITES_CFSV2/50137.csv')
d %<>%
  mutate(
    VPD = hydroET::cal_VPD(Ta = Tavg, Pa = Pa, q = q),
    ET_CR = ET_CR_Ma2021(Rn = Rn, Tair = Tavg, D = VPD, U2 = U2, Pa = Pa)$ET_a
    # ET_CR = ET_CR_Ma(Ta = Tavg, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa)
  )
d %>%
  mutate(yr = year(date),
         # RN_TAG = ifelse(Rn >= 0, 'Rn>0', 'Rn<0'),
         # VPD_TAG = ifelse(VPD >= 0, 'VPD>0', 'VPD<0'),
         TAG = case_when(Rn >= 0 & VPD >= 0 ~ 'Rn, VPD >= 0',
                         Rn <  0 & VPD >= 0 ~ 'Rn < 0',
                         Rn >= 0 & VPD <  0 ~ 'VPD < 0',
                         Rn <  0 & VPD <  0 ~ 'Rn, VPD < 0')) %>%
  filter(ET_CR <= 50) %>%                                          # 筛选处理
  ggplot(aes(x = date, y = ET_CR)) +
  geom_point(aes(color = TAG)) +
  facet_wrap(~yr, scales = 'free') +
  scale_shape_manual(values = c(19, 1)) +
  theme(
    text = element_text(family = 'serif', face = 'bold'),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  ggsci::scale_color_npg()


d %>%
  mutate(yr = year(date),
         # RN_TAG = ifelse(Rn >= 0, 'Rn>0', 'Rn<0'),
         # VPD_TAG = ifelse(VPD >= 0, 'VPD>0', 'VPD<0'),
         TAG = case_when(Rn >= 0 & VPD >= 0 ~ 'Rn, VPD >= 0',
                         Rn <  0 & VPD >= 0 ~ 'Rn < 0',
                         Rn >= 0 & VPD <  0 ~ 'VPD < 0',
                         Rn <  0 & VPD <  0 ~ 'Rn, VPD < 0')) %>%
  filter(ET_CR <= 50, ET_CR >= -10) %>%                                          # 筛选处理
  ggplot(aes(x = Rn, y = ET_CR)) +
  geom_point(aes(color = TAG))



# 检查Rn的异常导致了哪个参数产生异常，最终导致结果异常
# d <- fread('H:/data for cal_PET/SITES_CFSV2/50137.csv')
# d %<>%
#   mutate(VPD = hydroET::cal_VPD(Ta = Tavg, Pa = Pa, q = q),
#          Twb = cal_Twb(VPD = VPD, Ta = Tavg, Pa = Pa),
#          Tdry = cal_Tdry(Twb = Twb, Pa = Pa, Ta = Tavg),
#          Tw = cal_Tws(Ta = Tavg, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa),
#          Epmax = ET_Penman1948_max(Tdry = Tdry, Rn = Rn, U2 = U2, Pa = Pa, Ta = Tavg),
#          Ep = ET_Penman1948(Ta = Tavg, Rn = Rn, U2 = U2, VPD = VPD, Pa = Pa),
#          Ew = ET_PT1972(Tw = Tw, Rn = Rn, Pa = Pa, Ta = Tavg),
#          ET_CR = ET_CR_Ma(Ta = Tavg, Rn = Rn, U2 = U2, VPD = VPD , Pa = Pa),
#          TAG = ifelse(Rn >= 0, 'Rn>=0', 'Rn<0'))
# ## 2022-07-12 对每个中间变量进行检查，未发现异常
# d %>%
#   mutate(yr = year(date)) %>%
#   filter(ET_CR < 50) %>%
#   ggplot(aes(x = date, y = Ew)) +
#   geom_point(aes(color = TAG)) +
#   facet_wrap(~yr, scales = 'free') +
#   theme(
#     legend.position = 'top',
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank()
#   )











