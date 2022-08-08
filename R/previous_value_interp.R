#' Interpolate with the previous value
#'
#' @param vect_data raw data
#'
#' @return interpolated data
#' @export
#'
#' @importFrom stats na.omit
#'
#' @examples
#' previous_value_interp(c(1, NA, 2, NA, NA, 3))
previous_value_interp <- function(vect_data) {
  node = na.omit(vect_data)
  node_index = which(!is.na(vect_data))

  na_index = which(is.na(vect_data))
  # TODO: 解决第一个值或最后一个值为NA的情况
  for (ind in na_index) {
    for (n in seq_along(node)) {
      if(ind < node_index[1]) {
        vect_data[ind] = NA
        break
      }
      if(ind > node_index[length(node_index)]) {
        vect_data[ind] = node[length(node)]
        break
      }

      nod = node[n]
      nod_ind = node_index[n]

      if (ind < nod_ind) {
        vect_data[ind] = node[n-1]
        break
      }
    }
  }

  return(vect_data)
}
