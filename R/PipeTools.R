#' Value not matching
#'
#' @param x vector or NULL: the values to be matched.
#' @param table vector or NULL: the values to be matched against.
#'
#' @return TRUE or FLASE
#' @export
#'
#' @examples 1 %!in% 2:3
`%!in%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}
