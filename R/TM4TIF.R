#' #' Add time attribute to raster
#' #'
#' #' @param tif_path path of tif file
#' #' @param csv_path path of csv file containing TM
#' #'
#' #' @return a raster with time attr
#' #' @export
#' #'
#' #' @importFrom  dplyr `%>%`
#' #'
#' #' @examples
#' #' # addTM2tif('inst/data/raw/PML_V2_Ec.tif',
#' #' #           'inst/data/raw/PML_V2_Ec.csv')
#' addTM2tif <- function(tif_path,
#'                       csv_path) {
#'   utils::globalVariables(".")
#'
#'   d = terra::rast(tif_path)
#'
#'   TM <- data.table::fread(csv_path) %>%
#'     .$bandname %>%
#'     stringr::str_replace('\\[','') %>%
#'     stringr::str_replace('\\]', '') %>%
#'     stringr::str_split(pattern = ',') %>%
#'     unlist() %>%
#'     stringr::str_sub(1, 11) %>%
#'     as.Date()
#'
#'   terra::time(d) = TM
#'
#'   return(d)
#' }

