library(readxl)
library(data.table)
library(magrittr)

dt_sheets <- read_excel('D:/code/data/测序编号_From_GZX.xlsx', sheet = 1)

test <- function(sample_ID, tidy = T, project = F) {
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

  if (project) {
    NCBI_info = NCBI_info$facet_counts$facet_fields$fieldvals %>%
      `[`(seq(1, length(.)-1, by = 2)) %>% strsplit(':') %>%
    sapply(
      FUN = function(info) ifelse(info[1] == 'acc_s', info[2], NA)
    ) %>% .[which(!is.na(.))] %>% gsub(' ', '', .) %>%
      lapply(get_NCBI_info) %>%
      rbindlist(fill = T)
  } else{
    if (tidy) {
      NCBI_info = NCBI_info |> tidy_NCBI_info(sample_ID = sample_ID)
    }
  }

  NCBI_info
}

test('PRJDB4749', tidy = F, project = T)

# sample     drs061324
# projection PRJDB4749
test('PRJDB4749', tidy = F)$facet_counts$facet_fields$fieldvals %>%
  `[`(seq(1, length(.)-1, by = 2)) %>% strsplit(':')
  sapply(
    FUN = function(info) ifelse(info[1] == 'acc_s', info[2], NA)
  ) %>% .[which(!is.na(.))] %>% gsub(' ', '', .) %>%
  lapply(get_NCBI_info) %>%
  rbindlist(fill = T)

get_NCBI_info('PRJNA192876', project = T)




