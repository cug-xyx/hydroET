#' Enhanced separate function
#'
#' @param d A data frame
#' @param .col_name column name of the col_name to separate
#' @param .col_value column name of the col_value to separate
#' @param .col_name_sep Separation of column names
#' @param .col_value_sep Separation of column values
#' @param .remove Same as the parameters of `separate`
#' @param .convert Same as the parameters of `separate`
#' @param .extra Same as the parameters of `separate`
#' @param .fill Same as the parameters of `separate`
#' @param ... Same as the parameters of `separate`
#'
#' @importFrom purrr map_dfr
#' @importFrom tidyr separate
#' @importFrom stringr str_split
#'
#' @export
#'
#' @examples
#' d <- data.frame(FORMAT = c('B:C', 'A:B:C', 'B:C:D', 'A:C', 'A:B'),
#' value = c('2:3', '1:2:3', '2:3:4', '1:3', '1:2'),tes = 1)
#' res <- exact_separate(d, .col_name = FORMAT, .col_value = value)
exact_separate <- function(d, .col_name, .col_value,
                           .col_name_sep = '[^[:alnum:]]+',
                           .col_value_sep = '[^[:alnum:]]+',
                           .remove = TRUE, .convert = FALSE, .extra = 'warn',
                           .fill = 'warn', ...) {
  .col_name  = enquo(.col_name)
  .col_value = enquo(.col_value)

  goal_d = d %>% select(!!.col_name, !!.col_value)

  goal_d %<>%
    bind_cols(
      apply(., 1, FUN = function(single_row) {
        tibble(raw_colNm = single_row[1], raw_value = single_row[2]) %>%
          tidyr::separate(raw_value,
                   into = (.$raw_colNm %>% stringr::str_split(.col_name_sep) %>% unlist()),
                   sep = .col_value_sep, remove = .remove,
                   convert = .convert, extra = .extra, fill = .fill, ...) %>%
          select(-raw_colNm)
      }) %>% purrr::map_dfr(~.x)
    ) %>% select(-!!.col_value, -!!.col_name)

  bind_cols(d, goal_d)
}

# tibble(FORMAT = c('B:C', 'A:B:C', 'B:C:D', 'A:C', 'A:B'),
#        value = c('2:3', '1:2:3', '2:3:4', '1:3', '1:2'),
#        tes = 1) %>%
#   exact_separate(.col_name = FORMAT, .col_value = value)
