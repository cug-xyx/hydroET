library(data.table)
library(tidyverse)

separate2 <- function(d, .col_name, .col_value,  # string
                           .col_name_sep = '[^[:alnum:]]+',
                           .col_value_sep = '[^[:alnum:]]+') {
  d %>% cbind(
    mutate(.,
      across(
        matches(.col_name),
        ~str_split(., pattern = .col_name_sep)
        ),
      across(
        matches(.col_value),
        ~str_split(., pattern = .col_value_sep)
      )
    ) %>% select({{.col_name}}, {{.col_value}}) %>%
      as.data.table() %>% pmap(
        ~(matrix(..2, nrow = 1, dimnames = list(1, ..1)) %>% as.data.table())
      ) %>% rbindlist(fill = T)
     )
}

data.table(
  FORMAT = c('B:C', 'A:B:C', 'B:C:D', 'A:C', 'A:B'),
  value = c('2:3', '1:2:3', '2:3:4', '1:3', '1:2'),
  other_var = 1
) %>% separate2(.col_name = 'FORMAT', .col_value = 'value')


