library(data.table)
library(httr)
library(magrittr)
library(dplyr)


l <- GET(
    url = 'http://xxfb.mwr.cn/hydroSearch/greatRiver', # 全国水雨情信息
    add_headers(.headers = headers)
  ) |> content()

dt_0929 <- lapply(l$result$data, as.data.table) |> rbindlist()
dt_0929 %<>% mutate(across(c(everything(), -dateTime, -tm), ~gsub(' ', '', .)))

dt_0929
