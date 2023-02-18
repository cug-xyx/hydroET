library(magrittr)
library(data.table)
library(ggplot2)

yrd <- sf::st_read('I:/order_data/Weilh/yrd_all1/yrd_all.shp')

# 判断是polygon还是matrix还是vector
stf <- c(70, 40.5, 106, 40.5, 106, 25.5, 70, 25.5, 70, 40.5) |>
  matrix(ncol = 2, byrow = T) |> list() |> sf::st_polygon()

rec <- data.frame(ID = 1, geom = sf::st_sfc(stf)) |> sf::st_sf(crs = 4326)

eras <- rmapshaper::ms_erase(rec, yrd) |> sf::as_Spatial()

eras@polygons
ggplot() +
  ggpolypath::geom_polypath(data = eras, mapping = aes(long, lat, group=group), fill = 'white')


mtcars %>%
  ggplot() +
  geom_boxplot(aes(cyl, qsec)) +
  facet_wrap(~vs) +
  facet_subgraphs(
    plot_func = function(dt) ggplot() + geom_point(data = dt, aes(cyl, qsec)),
    data = mtcars,
    group_colnames = 'vs',
    xmin=5.5, xmax = 7.5, ymin = 21, ymax = 23
  )
