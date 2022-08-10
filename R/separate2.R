#' exact_separate with data.table
#'
#' @param d A data frame
#' @param .col_name column name of the col_name to separate [string]
#' @param .col_value column name of the col_value to separate [string]
#' @param .col_name_sep Separation of column names [string]
#' @param .col_value_sep Separation of column values [string]
#'
#' @import data.table
#' @import dplyr
#' @importFrom stringr str_split
#'
#' @export
#'
#' @examples
#' d <- data.frame(
#' FORMAT = c('B:C', 'A:B:C', 'B:C:D', 'A:C', 'A:B'),
#' value = c('2:3', '1:2:3', '2:3:4', '1:3', '1:2'),
#' other_var = 1)
#' separate2(d, .col_name = 'FORMAT', .col_value = 'value')
separate2 <- function(d, .col_name, .col_value,  # string
                      .col_name_sep = '[^[:alnum:]]+',
                      .col_value_sep = '[^[:alnum:]]+') {
  if (!data.table::is.data.table(d)) d %<>% data.table::as.data.table()

  d %>% cbind(
    dplyr::mutate(.,
           dplyr::across(
             dplyr::matches(.col_name),
             ~stringr::str_split(., pattern = .col_name_sep)
           ),
           dplyr::across(
             dplyr::matches(.col_value),
             ~stringr::str_split(., pattern = .col_value_sep)
           )
    ) %>% dplyr::select({{.col_name}}, {{.col_value}}) %>%
      data.table::as.data.table() %>% purrr::pmap(
        ~(matrix(..2, nrow = 1, dimnames = list(1, ..1)) %>% data.table::as.data.table())
      ) %>% data.table::rbindlist(fill = T)
  )
}



