#' Get water level (Z) and runoff (Q) data of hydro station
#'
#' @param stcd hydrologic station
#' @param sdate start date e.m. 1970-01-01
#' @param edate end date
#'
#' @return a data.table with hydro data
#' @export
#'
#' @importFrom magrittr `%>%` `%<>%`
#' @importFrom stringr str_glue str_extract
#' @importFrom httr GET content
#' @importFrom lubridate days
#' @import data.table
#'
#' @examples GetZQData(60106000, sdate = '1970-01-01', edate = '2022-08-30')
GetZQData <- function(stcd, sdate, edate=NULL) {
  # 万县 60106000
  if (missing(stcd)) stop('missing parameter "stcd"')
  if (is.null(edate)) {
    warning('only get data on "sdate"')
    edate = as.POSIXct(sdate) + lubridate::days(1)
  }

  base_url =
    stringr::str_glue(
      "http://113.57.190.228:8001/Web/Charts/GetZQData/?stcd={stcd}&sdate={sdate}&edate={edate}"
    ) %>% as.character()
  # get data
  d = httr::GET(base_url) %>% content() %>%
    .$RealSW %>% data.table::rbindlist() %>% data.table::as.data.table()

  # 判断数据是否为空
  if (nrow(d) == 0) {
    warning(
      stringr::str_glue('empty data from {sdate} to {edate}')
    )
    return(
      data.table::data.table(STCD = stcd, Z = NA, Q = NA, WPTN = NA, DATE = NA)  # DATE 为空代表空数据
    )
  }
  # format TM from web timestamp number to POSIXct
  d[, `:=`(
    DATE = stringr::str_extract(TM, "\\d+") %>% as.numeric() %>% `/`(1e3) %>%
      as.POSIXct(origin = '1970-01-01'),
    TM = NULL
  )][]
}


#' get NCBI sample information table
#'
#' @param sample_ID sample ID
#' @param tidy whether tidy the fieldvals table
#' @param project whether sample_ID is project_ID, `tidy` default to TRUE
#'
#' @return NCBI_info (list or data.frame)
#' @export
#'
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr `%>%`
#'
#' @examples get_NCBI_info(sample_ID = 'ERX2746395')
get_NCBI_info <- function(sample_ID, tidy = T, project = F) {
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


#' get protein homologs from JGI Phytozome
#'
#' @param transcript query transcript name
#'
#' @importFrom  data.table as.data.table rbindlist
#' @importFrom httr GET content
#' @importFrom magrittr `%>%`
#'
#' @return a data.table
#' @export
#'
#' @examples get_protein_homologs('Cre01.g005534.t1.2')
get_protein_homologs <- function(transcript) {
  url_get_id = paste0('https://phytozome-next.jgi.doe.gov/api/db/gene_281?protein=', transcript)

  transcript_id =
    httr::GET(url = url_get_id) %>% httr::content() %>%
    .$`_id` %>% substr(., 5, nchar(.))

  url_get_homologs = paste0(
    'https://phytozome-next.jgi.doe.gov/api/db/homologs/transcript/identifier/',
    transcript_id,
    '?',
    'proteome=281',
    '&',
    'orgids=456,502,281,707,461,227,325,228,229,231,317,539,538,320,318,310,522,521',
    '676,91,572,291,531,566,322,459,548,392,382,309,699,692,575,453,388,494,589,467',
    '689,256,506,553,505,551,451,390,514,691,448,686,700,297,457,530,679,673,492,122',
    '501,677,675,701,275,508,678,510,724,509,718,719,571,567,491,285,580,581,563,442',
    '696,670,534,298,687,385,573,562,561,588,469,540,545,544,541,558,543,559,507,200',
    '305,520,671,445,688,210,444,533,532,119,289,519,518,449,113,233,523,472,264,384',
    '167,447,278,266,474,470,484,585,482,489,582,483,173,485,478,476,477,473,574,486',
    '479,446,277,481,488,487,480,584,475,471,583,526,529,458,527,578,570,569,568,524',
    '221,525,182,154,565,586,321,498,504,550,587,304,290,324,668,462,702,386,323,499',
    '680,503,296,463,693,577,316,490,314,556,549,460,537,515,283,337,343,364,379,336',
    '369,356,333,381,359,372,355,349,362,353,331,361,378,346,328,344,374,380,357,363',
    '352,334,365,345,376,367,354,370,329,348,358,338,366,339,375,351,342,330,368,347',
    '377,350,373,335,326,360,340,371,341,332,327,690,560,497,450,516,672,312,311,500',
    '454,564,552,468,694,669,493,681,682,683,443,513,684,512,685,308,495,590,496,591'
  )

  homologs =
    httr::GET(url = url_get_homologs) %>% httr::content() %>%
    lapply(data.table::as.data.table) %>% data.table::rbindlist(fill = T)

  homologs
}
