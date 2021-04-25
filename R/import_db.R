#' Loads district data for Perú
#'
#' Function 'import_db' the specified dataset.
#'
#' @param dataset is a character specifiying the dataset to request.
#'
#' Current datasets available are:
#'
#' - "Peru_names": Returns the name of the 1874 districts, 196 provinces and 25 regions of Perú as they are reported by the INEI in the REDATAM platform for the 2017 CENSUS.
#'
#' - "Peru_shp": Returns the shapefile for thethe 1874 districts, 196 provinces and 25 regions of Perú
#'
#' @examples
#' df <- data.frame(
#'   reg = c(
#'     "LIMA", "CALLAO", "CAJAMARCA", "AMAZONAS", "SAN MARTIN", "HUANUCO",
#'     "PASCO", "JUNIN", "CUSCO", "PUNO", "APURIMAC", "AYACUCHO",
#'     "HUANCAVELICA", "TUMBES", "PIURA", "LAMBAYEQUE", "LA LIBERTAD",
#'     "ANCASH", "ICA", "AREQUIPA", "TACNA", "MOQUEGUA", "LORETO", "UCAYALI",
#'     "MADRE DE DIOS"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' @importFrom utils read.csv
#' @import RCurl
#' @export import_db

import_db <- function(dataset) {
  if (dataset == "Peru_names") {
    x <- read.csv("https://raw.githubusercontent.com/healthinnovation/lis/master/files/master_distr.csv", stringsAsFactors = F)

  } else if (dataset == "Peru_shp") {

    x <- "https://github.com/healthinnovation/lis/blob/master/files/shp_PER_adm3.Rdata?raw=true"
    x <- readRDS(url(x))
  }

  return(x)
}
