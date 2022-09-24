#' get NCBI sample information table
#'
#' @param sample_ID sample ID
#' @param tidy whether tidy the fieldvals table
#'
#' @return NCBI_info (list or data.frame)
#' @export
#'
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#'
#' @examples get_NCBI_info(sample_ID = 'ERX2746395')
get_NCBI_info <- function(sample_ID, tidy = T) {
  URL = 'https://www.ncbi.nlm.nih.gov/Traces/solr-proxy-be/solr-proxy-be.cgi?core=run_sel_index'
  PAYLOAD = list(
    'core' = 'run_sel_index',
    'q' = paste0(
      'start=0&rows=0&q=-non_public_run%3A%5B*%20TO%20*%5D%20AND%20((primary_search_ids%3A%22',
      sample_ID,
      '%22))&wt=json&facet=on&facet.mincount=1&facet.limit=150&facet.field=fields',
      '&facet.field=fieldvals&stats=true&stats.field=bases_l&stats.field=bytes_l'
    )
  )

  NCBI_info =
    httr::POST(
    url = URL, body = PAYLOAD) |>
    httr::content(encoding = 'utf-8') |>
    jsonlite::fromJSON()
  NCBI_info$sample_ID = sample_ID

  if (tidy) {
    NCBI_info = NCBI_info |> tidy_NCBI_info(sample_ID = sample_ID)
  }

  NCBI_info
}


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







