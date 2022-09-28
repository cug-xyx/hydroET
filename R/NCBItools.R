#' tidy the fieldvals table of NCBI_info
#'
#' @param NCBI_info NCBI_info
#' @param sample_ID sample ID
#'
#' @importFrom tidyr separate
#' @import data.table
#'
#' @return a tidy data.frame
#' @export
tidy_NCBI_info <- function(NCBI_info, sample_ID) {
  NCBI_info = NCBI_info$facet_counts$facet_fields$fieldvals

  NCBI_info =
    NCBI_info |>
    as.data.frame() |>
    subset(NCBI_info != '1') |>
    tidyr::separate(
      col = NCBI_info,
      into = c('field', 'fieldvals'),
      sep = ': ')
  NCBI_info$sample_ID = sample_ID

  as.data.table(NCBI_info) |>
    dcast(sample_ID ~ field, value.var = 'fieldvals', fun.aggregate = toString)
}







