#' add subgraph to ggplot facet object
#'
#' @param data a data.frame
#' @param group_colnames column names of group variable
#' @param plot_func function
#' @param xmin xmin
#' @param xmax xmax
#' @param ymin ymin
#' @param ymax ymax
#'
#' @export
facet_subgraphs <- function(
  data,
  group_colnames,
  plot_func,
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
) {
  if (missing(plot_func)) {
    warnings("missing parameter 'plot_func'")
    plot_func = function(dt) ggplot2::ggplot()
  }

  lapply(
    split(data, data[[group_colnames]]),
    function(dt) {
      ggplot2::layer(

        data = dplyr::select(dt, tidyr::all_of(group_colnames)) |> unique(),

        params = list(
          grob = ggplot2::ggplotGrob(
            plot_func(dt)
          ),
          xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
        ),

        stat = ggplot2::StatIdentity,
        position = ggplot2::PositionIdentity,
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE
      )
    }
  )
}
