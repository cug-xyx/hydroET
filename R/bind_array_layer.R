#' Merge layers of the same shape array
#'
#' @param arr1 A array
#' @param arr2 A array
#'
#' @export
#'
#' @examples bind_array_layer(array(1:2, c(1, 2)), array(1:2, c(1, 2)))
bind_array_layer <- function(arr1, arr2) {
  dim_1 = dim(arr1)
  dim_2 = dim(arr2)

  if (!isTRUE(all.equal(dim_1[1:2], dim_2[1:2]))) {
    stop('Single layer matrices have different shapes')
  }

  if (length(dim_1) == 2) {
    arr1 = array(arr1, dim = c(dim_1, 1))
    dim_1 = dim(arr1)
  }
  if (length(dim_2) == 2) {
    arr2 = array(arr2, dim = c(dim_2, 1))
    dim_2 = dim(arr2)
  }
  shape      = dim_1[1:2]
  total_lyrs = dim_1[3] + dim_2[3]

  return(array(c(arr1, arr2), dim = c(shape, total_lyrs)))
}

