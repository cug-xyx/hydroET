#' Drawing rainfall runoff map based on ggplot2
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
ggRunoff <- function(d, date, Q, prcp, times=1,
                     facet = F,
                     col_q = NULL,
                     ymax = NULL,
                     prcp_color = NULL,
                     axis1_lim = NULL,
                     Axis1='Axis1', Axis2='Axis2',
                     line_size = 1) {

  if(is.null(col_q)) {
    mutate(d, col_q='default_Q')
  }

  d %<>%
    rename(date = {{date}},
           Q = {{Q}},
           P = {{prcp}},
           col_type = {{col_q}}) %>%
    mutate(P = times * P)

  if (is.null(ymax)) ymax = 1.1*(max(d$P) + max(d$Q)) # set Y_max
  if (is.null(prcp_color)) prcp_color = 'blue'

  d %<>%
    mutate(tile_point = (ymax - P / 2)) # 计算 geom_tile 的中心点 max(Q) => 输入参数
  # browser()
  p = d %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = Q, color = col_type), size=line_size) +
    geom_tile(aes(y = tile_point, height = P), fill = prcp_color) +
    scale_y_continuous(
      limits = axis1_lim,
      expand = c(0, 0),
      name = Axis1,
      sec.axis = sec_axis(
        name = Axis2,
        trans = ~ (ymax - .) / times,
        labels = function(x) x
      )
    )
  if (facet) {
    p = p +
      facet_wrap(~type, scales = 'free')
  }
  p
}



