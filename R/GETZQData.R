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

