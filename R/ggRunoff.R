#' Title Drawing rainfall runoff map based on ggplot2
#'
#' @param d input data [data.frame]
#' @param date Column name of date [character]
#' @param Q column name of runoff [character]
#' @param prcp column name of precipitation [character]
#' @param ymax Y-axis maximum, also second abscissa axis position
#' @param prcp_color fill color for rainfall
#'
#' @importFrom  magrittr `%>%` `%<>%`
#' @import dplyr ggplot2
#'
#' @return rainfall runoff map based on ggplot2
#' @export
#'
#' @examples
#'  d <- data.frame(date = seq(as.POSIXct('2022-05-02'),
#'                         as.POSIXct('2022-05-05'), by = 'day'),
#'                  Q    = seq(10, 40, 10),
#'                  P    = c(1, 2, 0, 2))
#'  ggRunoff(d, date = 'date', Q = 'Q', prcp = 'P')
ggRunoff <- function(d, date, Q, prcp,
                     ymax = NULL,
                     prcp_color = NULL) {

  d %<>%
    rename(date = {{date}},
           Q = {{Q}},
           P = {{prcp}})

  if (is.null(ymax)) ymax = 1.1*(max(d$P) + max(d$Q)) # set Y_max
  if (is.null(prcp_color)) prcp_color = 'blue'

  d %<>%
    mutate(tile_point = (ymax - P / 2)) # 计算 geom_tile 的中心点 max(Q) => 输入参数

  d %>%
  ggplot(aes(x = date)) +
    geom_line(aes(y = Q)) +
    geom_tile(aes(y = tile_point, height = P), fill = prcp_color) +
    scale_y_continuous(
      expand = c(0, 0),
      name = 'Axis 1',
      sec.axis = sec_axis(
        name = 'Axis 2',
        trans = ~ (ymax - .),
        labels = function(x) x
      )
    )
}



